-- Mine Detector Game
-- Copyright (C) 2021 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- V8.1 2021 Aug 01          Pre-drawn flags
-- V8.0 2021 Jul 15          First Ada_GUI version
-- V7.5 2021 Jun 15          Fix GNAT 11 circular-elaboration error
-- v7.4 2019 Jul 01          Counts on flags, better quit handling for Epiphany
-- v7.3 2018 Mar 15          Graphical mine field
-- v7.2 2016 Feb 15          Cleaned up unreferenced packages
-- V7.1 2015 Jun 15          Switched from Docker to Grid and added touch-screen support
-- V7.0 2014 Dec 01          First Gnoga version
--

with Ada.Characters.Latin_1;
with Ada.Exceptions;
with Ada.Numerics;
with Ada.Text_IO;

with Field.Operations;
pragma Elaborate (Field.Operations);

with Ada_GUI;

package body User_IF is
   use Ada;
   use Ada.Characters;

   Gray : constant Ada_GUI.Color_Info := (Red => 224, Green => 224, Blue => 224, Alpha => 1.0);

   type Flag_Set is array (Field.Valid_Count) of Ada_GUI.Widget_ID;

   Mines_Left     : Ada_GUI.Widget_ID;
   Button         : Ada_GUI.Widget_ID;
   Drawing        : Ada_GUI.Widget_ID;
   Flag           : Flag_Set;
   Restart_Button : Ada_GUI.Widget_ID;
   Level          : Ada_GUI.Widget_ID;
   Mark_Check     : Ada_GUI.Widget_ID;
   Step_Check     : Ada_GUI.Widget_ID;
   Rules          : Ada_GUI.Widget_ID;
   About          : Ada_GUI.Widget_ID;
   Quit           : Ada_GUI.Widget_ID;
   Game_Over      : Ada_GUI.Widget_ID;
   Mode_Check     : Ada_GUI.Widget_ID;

   You_Won_Message  : constant String := "You Won";
   You_Lost_Message : constant String := "BOOM!";

   type Level_Info is record
      Name  : String (1 .. 3);
      Mines : Natural;
   end record;

   type Level_List is array (Positive range <>) of Level_Info;

   Levels : constant Level_List := (1 => (Name => " 50", Mines =>  50),
                                    2 => (Name => "100", Mines => 100),
                                    3 => (Name => "150", Mines => 150),
                                    4 => (Name => "200", Mines => 200),
                                    5 => (Name => "250", Mines => 250) );

   Default_Level : constant := 4;

   Auto_Marking_Desired : Boolean := False;
   pragma Atomic (Auto_Marking_Desired);
   Extended_Stepping_Desired : Boolean := True;
   pragma Atomic (Extended_Stepping_Desired);

   subtype Cell_String is String (1 .. 1);

   procedure Show_Game_Over is
      -- null;
   begin -- Show_Game_Over
      case Field.Operations.Game_State is
      when Field.Operations.Won =>
         Game_Over.Set_Text (Text => You_Won_Message);
         Game_Over.Set_Visibility (Visible => True);
      when Field.Operations.Lost =>
         Game_Over.Set_Text (Text => You_Lost_Message);
         Game_Over.Set_Visibility (Visible => True);
      when Field.Operations.In_Progress =>
         null;
      end case;
   end Show_Game_Over;
   pragma Inline (Show_Game_Over);

   use type Field.Operations.Game_State_ID;

   Button_Size : constant := 30;

   Light_Green : constant Ada_GUI.Color_Info := Ada_GUI.To_Color (Ada_GUI.Light_Green);
   Black       : constant Ada_GUI.Color_Info := Ada_GUI.To_Color (Ada_GUI.Black);
   White       : constant Ada_GUI.Color_Info := Ada_GUI.To_Color (Ada_GUI.White);
   Red         : constant Ada_GUI.Color_Info := Ada_GUI.To_Color (Ada_GUI.Red);

   procedure Display (Cell : in Field.Cell_Location; Text : in Cell_String; Stepped : in Boolean) is
      X : constant Natural := (Cell.Column - 1) * Button_Size;
      Y : constant Natural := (Cell.Row - 1) * Button_Size;

      Color : Ada_GUI.Color_Info;
   begin -- Display
      if Text = "X" then
         Color := Red;
      elsif Stepped then
         Color := Gray;
      else
         Color := Light_Green;
      end if;

      Drawing.Draw_Rectangle (From_X     => 0,
                              From_Y     => 0,
                              To_X       => Button_Size - 1,
                              To_Y       => Button_Size - 1,
                              Fill_Color => (None => False, Color => Color) );

      case Text (Text'First) is
      when ' ' =>
         null;
      when '0' .. '9' =>
         Drawing.Draw_Text (X => 7, Y => 22, Text => Text);
      when 'X' =>
         Drawing.Draw_Arc (X          => Button_Size / 2,
                           Y          => Button_Size / 2,
                           Radius     => 10,
                           Start      =>  0.0,
                           Stop       =>  2.0 * Numerics.Pi,
                           Line_Color => (None => True),
                           Fill_Color => (None => False, Color => Black) );
         Drawing.Draw_Line (From_X => 4, From_Y => 4, To_X => Button_Size - 4, To_Y => Button_Size - 4, Width => 3);
         Drawing.Draw_Line (From_X => Button_Size - 4, From_Y => 4, To_X => 4, To_Y => Button_Size - 4, Width => 3);
         Drawing.Draw_Line (From_X => Button_Size / 2, From_Y => 0, To_X => Button_Size / 2, To_Y => Button_Size - 1, Width => 3);
         Drawing.Draw_Line (From_X => 0, From_Y => Button_Size / 2, To_X => Button_Size - 1, To_Y => Button_Size / 2, Width => 3);
      when others =>
         raise Program_Error;
      end case;

      Button.Replace_Pixels (Image => Drawing, X => X, Y => Y);

      if Field.Operations.Game_State /= Field.Operations.In_Progress then
         Show_Game_Over;
      end if;
   end Display;
   pragma Inline (Display);

   procedure Display_Blank (Cell : in Field.Cell_Location) is
      -- null;
   begin -- Display_Blank
      Display (Cell => Cell, Text => " ", Stepped => False);
   end Display_Blank;

   procedure Display_Count (Count   : in Field.Valid_Count;
                            Stepped : in Boolean;
                            Cell    : in Field.Cell_Location)
   is
      Zero_Pos : constant := Character'Pos ('0');
   begin -- Display_Count
      Display (Cell    => Cell,
               Text    => Character'Val (Zero_Pos + Count) & "",
               Stepped => Stepped);
   end Display_Count;

   procedure Display_Mark (Count : in Field.Valid_Count; Cell : in Field.Cell_Location) is
      X : constant Natural := (Cell.Column - 1) * Button_Size;
      Y : constant Natural := (Cell.Row - 1) * Button_Size;
   begin -- Display_Mark
      Button.Replace_Pixels (Image => Flag (Count), X => X, Y => Y);

      if Field.Operations.Game_State /= Field.Operations.In_Progress then
         Show_Game_Over;
      end if;
   end Display_Mark;

   procedure Display_Mine (Cell : in Field.Cell_Location) is
      -- null;
   begin -- Display_Mine
      Display (Cell => Cell, Text => "X", Stepped => True);
   end Display_Mine;

   procedure Display_To_Go (To_Go : in Integer) is
      Image : constant String := Integer'Image (To_Go);
   begin -- Display_To_Go
      Mines_Left.Set_Text (Text => Image);
   end Display_To_Go;

   procedure Reset_Screen is
      -- null;
   begin -- Reset_Screen
      Mines_Left.Set_Text (Text => "0");
      Game_Over.Set_Text  (Text => "");
      Game_Over.Set_Visibility (Visible => False);

      Button_Row : for Row in Field.Valid_Row loop
         Button_Column : for Column in Field.Valid_Column loop
            Display_Blank ( (Row => Row, Column => Column) );
         end loop Button_Column;
      end loop Button_Row;
   end Reset_Screen;

   function Auto_Marking return Boolean is
      -- null;
   begin -- Auto_Marking
      return Auto_Marking_Desired;
   end Auto_Marking;

   function Extended_Stepping return Boolean is
      -- null;
   begin -- Extended_Stepping
      return Extended_Stepping_Desired;
   end Extended_Stepping;

   procedure Button_Press (Mouse_Event : in Ada_GUI.Mouse_Event_Info) is
      Row    : constant Field.Valid_Row    := Mouse_Event.Y / Button_Size + 1;
      Column : constant Field.Valid_Column := Mouse_Event.X / Button_Size + 1;
   begin -- Button_Press
      if Field.Operations.Game_State /= Field.Operations.In_Progress then
         Show_Game_Over;
      elsif Mode_Check.Active then
         Field.Operations.Mark (Cell => (Row => Row, Column => Column) );
      else
         Field.Operations.Step (Cell => (Row => Row, Column => Column) );
      end if;
   exception -- Button_Press
   when E : others =>
      Text_IO.Put_Line (Item => "Button_Press: " & Ada.Exceptions.Exception_Information (E) );
   end Button_Press;

   procedure Right_Click (Mouse_Event : in Ada_GUI.Mouse_Event_Info) is
      Row    : constant Field.Valid_Row    := Mouse_Event.Y / Button_Size + 1;
      Column : constant Field.Valid_Column := Mouse_Event.X / Button_Size + 1;
   begin -- Right_Click
      if Field.Operations.Game_State /= Field.Operations.In_Progress then
         Show_Game_Over;
      else
         Field.Operations.Mark (Cell => (Row => Row, Column => Column) );
      end if;
   exception -- Right_Click
   when E : others =>
      Text_IO.Put_Line (Item => "Right_Click: " & Ada.Exceptions.Exception_Information (E) );
   end Right_Click;

   procedure Rules_Pressed is
      Rules : constant String :=
         "The object of the game is to mark all cells containing " &
         "mines and to step on all cells that do not contain a " &
         "mine." & Latin_1.LF &
         Latin_1.LF &
         "The game is played on a rectangular field of 16 x 30 " &
         "cells. A number of mines are hidden within the field." & Latin_1.LF &
         Latin_1.LF &
         "Some of the cells have numbers on them. The numbers represent " &
         "the total number of mines in that cell and its " &
         "immediate neighbors. As you play the game, additional cells " &
         "will become numbered." & Latin_1.LF &
         Latin_1.LF &
         "You step on a cell by clicking on it. You mark a cell by right " &
         "clicking on it. A marked cell has an M on it. Marking a " &
         "marked cell unmarks it. You can only mark or step " &
         "on a cell with a number on it." & Latin_1.LF &
         Latin_1.LF &
         "When you step on a cell, an auto-stepping algorithm " &
         "automatically steps on any of its neighbors that " &
         "obviously do not contain mines. Since this is then " &
         "done for the neighbors of the stepped-on neighbors, " &
         "the auto-stepping algorithm will spread across areas " &
         "of the field that obviously do not contain mines. The " &
         "auto-stepping algorithm is invoked even if the cell is " &
         "already stepped on. This can be useful to clear around " &
         "a new mark." & Latin_1.LF &
         Latin_1.LF &
         "If you step on a cell containing a mine, either " &
         "directly or indirectly through the auto-stepping " &
         "algorithm, the cell shows an X, and the game is over." &
         Latin_1.LF &
         Latin_1.LF &
         "The game is over when you step on a mine, or when you " &
         "have marked all mines and stepped on all other cells. " &
         "If you win, '" & You_Won_Message & "' appears below the " &
         "'Quit' button. If you lose, '" & You_Lost_Message &
         "' appears there." & Latin_1.LF &
         Latin_1.LF &
         "At the top right of the field is a number. At the " &
         "start of a game this is the number of mines in the " &
         "field. Each time you mark a cell, this number is " &
         "decreased by one. Each time you unmark a marked cell, " &
         "this number is increased by one. If you successfully " &
         "complete a game, this number will be zero." & Latin_1.LF &
         Latin_1.LF &
         "The 'New Game' button starts a new game. Any game in " &
         "progress is abandoned." & Latin_1.LF &
         Latin_1.LF &
         "The level drop-down allows you to choose how many mines " &
         "will be in the field at the start of the next game. You " &
         "can choose from" & Levels (Levels'First).Name & " to " &
         Levels (Levels'Last).Name & " mines. This goes into effect " &
         "the next time you start a new game. At higher numbers of " &
         "mines, it may not be possible to win the game without luck." & Latin_1.LF &
         Latin_1.LF &
         "The 'Auto Mark' check box enables an auto-marking " &
         "algorithm that marks any cells that obviously contain " &
         "a mine. At lower levels, the game does not present much " &
         "of an intellectual challenge with this option. At higher " &
         "levels, it's very difficult to play without this option." & Latin_1.LF &
         Latin_1.LF &
         "The 'Auto Step after Mark' check box enables the auto-" &
         "stepping algorithm after a cell is marked, either " &
         "directly or indirectly through the auto-marking " &
         "algorithm." & Latin_1.LF & Latin_1.LF &
         "The 'Mark' check box is for use with touch screens or other " &
         "systems for which right clicking is difficult or impossible. " &
         "When this box is not checked, clicking on a cells steps on the " &
         "cell. When this box is checked, clicking on a cell marks or " &
         "unmarks the cell.";
   begin -- Rules_Pressed
      Ada_GUI.Show_Message_Box (Text => Rules);
   exception -- Rules_Pressed
   when E : others =>
      Text_IO.Put_Line (Item => "Rules_Pressed: " & Ada.Exceptions.Exception_Information (E) );
   end Rules_Pressed;

   procedure About_Pressed is
      -- null;
   begin -- About_Pressed
      Ada_GUI.Show_Message_Box (Text => "Mine Detector" & Latin_1.LF & "Copyright (C) 2021 by" & Latin_1.LF &
                                        "PragmAda Software Engineering" & Latin_1.LF &
                                        "Released as Free Software under the terms" & Latin_1.LF &
                                        "of the GNU Public License" & Latin_1.LF & '"' & "Ada Inside" & '"');
   exception -- About_Pressed
   when E : others =>
      Text_IO.Put_Line (Item => "About_Pressed: " & Ada.Exceptions.Exception_Information (E) );
   end About_Pressed;

   procedure Create_Level_Option_Menu is
      -- null;
   begin -- Create_Level_Option_Menu
      Add_Options : for I in Levels'range loop
         Level.Insert (Text => Levels (I).Name);
      end loop Add_Options;
   end Create_Level_Option_Menu;

   procedure Play_Game is
      Event : Ada_GUI.Next_Result_Info;

      use type Ada_GUI.Event_Kind_ID;
      use type Ada_GUI.Widget_ID;
   begin -- Play_Game
      Field.Operations.Reset;

      All_Events : loop
         Handle_Error : begin
            Event := Ada_GUI.Next_Event (Timeout => 1.0);

            if not Event.Timed_Out then
               exit All_Events when Event.Event.Kind = Ada_GUI.Window_Closed;

               if Event.Event.Kind in Ada_GUI.Left_Click | Ada_GUI.Right_Click then
                  exit All_Events when Event.Event.ID = Quit;

                  if Event.Event.ID = Button then
                     if Event.Event.Kind = Ada_GUI.Left_Click then
                        Button_Press (Mouse_Event => Event.Event.Mouse);
                     elsif Event.Event.Kind = Ada_GUI.Right_Click then
                        Right_Click (Mouse_Event => Event.Event.Mouse);
                     else
                        null;
                     end if;
                  elsif Event.Event.ID = Restart_Button then
                     Field.Operations.Set_Mine_Count (Levels (Level.Selected).Mines);
                     Field.Operations.Reset;
                  elsif Event.Event.ID = Mark_Check then
                     Auto_Marking_Desired := Mark_Check.Active;
                  elsif Event.Event.ID = Step_Check then
                     Extended_Stepping_Desired := Step_Check.Active;
                  elsif Event.Event.ID = Rules then
                     Rules_Pressed;
                  elsif Event.Event.ID = About then
                     About_Pressed;
                  else
                     null;
                  end if;
               end if;
            end if;
         exception -- Handle_Error
         when E : others =>
            Text_IO.Put_Line (Item => "Event loop: " & Ada.Exceptions.Exception_Information (E) );
         end Handle_Error;
      end loop All_Events;

      Ada_GUI.End_GUI;
   end Play_Game;

   Zero_Pos : constant := Character'Pos ('0');
begin -- User_IF
   Field.Operations.Set_Mine_Count (Levels (Default_Level).Mines);
   Ada_GUI.Set_Up (Grid => (1 => (1 => (Kind => Ada_GUI.Area, Alignment => Ada_GUI.Right),
                                  2 => (Kind => Ada_GUI.Area, Alignment => Ada_GUI.Center),
                                  3 => (Kind => Ada_GUI.Area, Alignment => Ada_GUI.Left) ) ),
                   Title => "Mine Detector");
   Ada_GUI.Set_Background_Color (Color => Ada_GUI.To_Color (Ada_GUI.Light_Blue) );
   Button := Ada_GUI.New_Graphic_Area
      (Width => Field.Valid_Column'Last * Button_Size, Height => Field.Valid_Row'Last * Button_Size);
   Drawing := Ada_GUI.New_Graphic_Area (Width => Button_Size, Height => Button_Size, Break_Before => True);
   Drawing.Set_Visibility (Visible => False);

   Create_Flags : for I in Flag'Range loop
      Flag (I) := Ada_GUI.New_Graphic_Area (Width => Button_Size, Height => Button_Size);
      Flag (I).Set_Visibility (Visible => False);
   end loop Create_Flags;

   Flag (Flag'First).Draw_Rectangle (From_X     => 0,
                                     From_Y     => 0,
                                     To_X       => Button_Size - 1,
                                     To_Y       => Button_Size - 1,
                                     Fill_Color => (None => False, Color => Light_Green) );
   Flag (Flag'First).Draw_Rectangle (From_X     =>  7,
                                     From_Y     =>  5,
                                     To_X       => 21,
                                     To_Y       => 14,
                                     Line_Color => (None => True),
                                     Fill_Color => (None => False, Color => Red) );
   Flag (Flag'First).Draw_Line (From_X => 7, From_Y => 5, To_X => 7, To_Y => 25);

   Copy_Flag : for I in Flag'First + 1 .. Flag'Last loop
      Flag (I).Replace_Pixels (Image => Flag (Flag'First), X => 0, Y => 0);
   end loop Copy_Flag;

   Add_Digit : for I in Flag'Range loop
      Flag (I).Draw_Text (X => Button_Size / 2, Y => 26, Text => Character'Val (Zero_Pos + I) & "", Height => 13);
   end loop Add_Digit;

   Mines_Left := Ada_GUI.New_Background_Text (Column => 3, Text => "0");
   Mines_Left.Set_Text_Aligbnment (Alignment => Ada_GUI.Center);
   Restart_Button := Ada_GUI.New_Button (Column => 3, Text => "New Game", Break_Before => True);
   Level := Ada_GUI.New_Selection_List (Column => 3, Break_Before => True);
   Create_Level_Option_Menu;
   Level.Set_Selected (Index => Default_Level);
   Mark_Check := Ada_GUI.New_Check_Box (Column => 3, Label => "Auto Mark", Break_Before => True);
   Step_Check := Ada_GUI.New_Check_Box (Column => 3, Label => "Auto Step after Mark", Break_Before => True, Active => True);
   Rules := Ada_GUI.New_Button (Column => 3, Text => "Rules", Break_Before => True);
   About := Ada_GUI.New_Button (Column => 3, Text => "About", Break_Before => True);
   Quit := Ada_GUI.New_Button (Column => 3, Text => "Quit", Break_Before => True);
   Game_Over := Ada_GUI.New_Background_Text (Column => 3, Text => You_Won_Message, Break_Before => True);
   Game_Over.Set_Text_Aligbnment (Alignment => Ada_GUI.Center);
   Game_Over.Set_Visibility (Visible => False);
   Mode_Check := Ada_GUI.New_Check_Box (Column => 3, Label => "Mark", Break_Before => True);
exception -- User_IF
when E : others =>
   Text_IO.Put_Line (Item => "User_IF: " & Ada.Exceptions.Exception_Information (E) );
end User_IF;
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; version 2.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.
