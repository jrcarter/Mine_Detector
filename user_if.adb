-- Mine Detector Game
-- Copyright (C) 2018 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- v7.3 2018 Mar 15          Graphical mine field
-- v7.2 2016 Feb 15          Cleaned up unreferenced packages
-- V7.1 2015 Jun 15          Switched from Docker to Grid and added touch-screen support
-- V7.0 2014 Dec 01          First Gnoga version
--

with Ada.Characters.Latin_1;
with Ada.Exceptions;

with Field.Operations;
pragma Elaborate (Field.Operations);

with Gnoga.Application.Singleton;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element.Canvas.Context_2D;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Form;
with Gnoga.Gui.View.Grid;
with Gnoga.Gui.Window;
with Gnoga.Types.Colors;

use Ada;
use Ada.Characters;

package body User_IF is
   Gray : constant Gnoga.Types.RGBA_Type := (Red => 224, Green => 224, Blue => 224, Alpha => 1.0);

   type Flag_Map is array (Field.Valid_Count) of Gnoga.Gui.Element.Canvas.Canvas_Type;

   Window         : Gnoga.Gui.Window.Window_Type;
   Big_View       : Gnoga.Gui.View.Grid.Grid_View_Type;
   Left_View      : aliased Gnoga.Gui.View.View_Type;
   Right_View     : aliased Gnoga.Gui.View.View_Type;
   Mines_Left     : Gnoga.Gui.Element.Common.Span_Type;
   Button         : Gnoga.Gui.Element.Canvas.Canvas_Type;
   Flag           : Flag_Map;
   Drawing        : Gnoga.Gui.Element.Canvas.Canvas_Type;
   Restart_Button : Gnoga.Gui.Element.Common.Button_Type;
   Level_Form     : Gnoga.Gui.Element.Form.Form_Type;
   Level          : Gnoga.Gui.Element.Form.Selection_Type;
   Mark_Form      : Gnoga.Gui.Element.Form.Form_Type;
   Mark_Check     : Gnoga.Gui.Element.Form.Check_Box_Type;
   Mark_Label     : Gnoga.Gui.Element.Form.Label_Type;
   Step_Form      : Gnoga.Gui.Element.Form.Form_Type;
   Step_Check     : Gnoga.Gui.Element.Form.Check_Box_Type;
   Step_Label     : Gnoga.Gui.Element.Form.Label_Type;
   Rules          : Gnoga.Gui.Element.Common.Button_Type;
   About          : Gnoga.Gui.Element.Common.Button_Type;
   Quit           : Gnoga.Gui.Element.Common.Button_Type;
   Game_Over      : Gnoga.Gui.Element.Common.Span_Type;
   Mode_Form      : Gnoga.Gui.Element.Form.Form_Type;
   Mode_Check     : Gnoga.Gui.Element.Form.Check_Box_Type;
   Mode_Label     : Gnoga.Gui.Element.Form.Label_Type;

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

   type Action_ID is (Button_Press, Right_Click, Restart, Quiting);

   protected Sequentializer is
      entry Respond (Action : in Action_ID; Cell : in Field.Cell_Location := (Row => 1, Column => 1) );
   end Sequentializer;

   procedure Show_Game_Over is
      -- null;
   begin -- Show_Game_Over
      case Field.Operations.Game_State is
      when Field.Operations.Won =>
         Game_Over.Text (Value => You_Won_Message);
      when Field.Operations.Lost =>
         Game_Over.Text (Value => You_Lost_Message);
      when Field.Operations.In_Progress =>
         null;
      end case;
   end Show_Game_Over;
   pragma Inline (Show_Game_Over);

   use type Field.Operations.Game_State_ID;

   Button_Size : constant := 30;

   Light_Green : constant Gnoga.Types.RGBA_Type := Gnoga.Types.Colors.To_RGBA (Gnoga.Types.Colors.Light_Green);
   Black       : constant Gnoga.Types.RGBA_Type := Gnoga.Types.Colors.To_RGBA (Gnoga.Types.Colors.Black);
   White       : constant Gnoga.Types.RGBA_Type := Gnoga.Types.Colors.To_RGBA (Gnoga.Types.Colors.White);
   Red         : constant Gnoga.Types.RGBA_Type := Gnoga.Types.Colors.To_RGBA (Gnoga.Types.Colors.Red);

   procedure Display (Cell : in Field.Cell_Location; Text : in Cell_String; Stepped : in Boolean) is
      X : constant Natural := (Cell.Column - 1) * Button_Size;
      Y : constant Natural := (Cell.Row - 1) * Button_Size;

      Rectangle : constant Gnoga.Types.Rectangle_Type := (X => 0, Y => 0, Width => Button_Size, Height => Button_Size);

      Field_Context  : Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type;
      Button_Context : Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type;
   begin -- Display
      Field_Context.Get_Drawing_Context_2D (Canvas => Button);
      Button_Context.Get_Drawing_Context_2D (Canvas => Drawing);

      if Text = "X" then
         Button_Context.Fill_Color (Value => Red);
      elsif Stepped then
         Button_Context.Fill_Color (Value => Gray);
      else
         Button_Context.Fill_Color (Value => Light_Green);
      end if;

      Button_Context.Fill_Rectangle (Rectangle => Rectangle);
      Button_Context.Stroke_Color (Value => Black);
      Button_Context.Line_Width (Value => 1);
      Button_Context.Stroke_Rectangle (Rectangle => Rectangle);

      case Text (Text'First) is
      when ' ' =>
         null;
      when '0' .. '9' =>
         Button_Context.Font (Height => "20px");
         Button_Context.Fill_Color (Value => Black);
         Button_Context.Fill_Text (Text => Text, X => 7, Y => 22);
      when 'X' =>
         Button_Context.Fill_Color (Value => Black);
         Button_Context.Begin_Path;
         Button_Context.Arc_Degrees
            (X => Button_Size / 2, Y => Button_Size / 2, Radius => 10, Starting_Angle => 0.0, Ending_Angle => 360.0);
         Button_Context.Fill;
         Button_Context.Line_Width (Value => 3);
         Button_Context.Begin_Path;
         Button_Context.Move_To (X => 4, Y => 4);
         Button_Context.Line_To (X => Button_Size - 4, Y => Button_Size - 4);
         Button_Context.Stroke;
         Button_Context.Begin_Path;
         Button_Context.Move_To (X => Button_Size - 4, Y => 4);
         Button_Context.Line_To (X => 4, Y => Button_Size - 4);
         Button_Context.Stroke;
         Button_Context.Begin_Path;
         Button_Context.Move_To (X => Button_Size / 2, Y => 0);
         Button_Context.Line_To (X => Button_Size / 2, Y => Button_Size - 1);
         Button_Context.Stroke;
         Button_Context.Begin_Path;
         Button_Context.Move_To (X => 0, Y => Button_Size / 2);
         Button_Context.Line_To (X => Button_Size - 1, Y => Button_Size / 2);
         Button_Context.Stroke;
      when others =>
         raise Program_Error;
      end case;

      Field_Context.Draw_Image (Image => Drawing, X => X, Y => Y);

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

      Field_Context  : Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type;
   begin -- Display_Mark
      Field_Context.Get_Drawing_Context_2D (Canvas => Button);
      Field_Context.Draw_Image (Image => Flag (Count), X => X, Y => Y);

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
      Mines_Left.Text (Value => Image);
   end Display_To_Go;

   procedure Reset_Screen is
      -- null;
   begin -- Reset_Screen
      Mines_Left.Text (Value => "0");
      Game_Over.Text  (Value => "");

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

   procedure When_Close (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      -- null;
   begin -- When_Close
      Sequentializer.Respond (Action => Quiting);
   exception -- When_Close
   when E : others =>
      Gnoga.Log (Message => "When_Close: " & Ada.Exceptions.Exception_Information (E) );
   end When_Close;

   procedure Mark_Toggle (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      -- null;
   begin -- Mark_Toggle
      Auto_Marking_Desired := Mark_Check.Checked;
   exception -- Mark_Toggle
   when E : others =>
      Gnoga.Log (Message => "Mark_Toggle: " & Ada.Exceptions.Exception_Information (E) );
   end Mark_Toggle;

   procedure Step_Toggle (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      -- null;
   begin -- Step_Toggle
      Extended_Stepping_Desired := Step_Check.Checked;
   exception -- Step_Toggle
   when E : others =>
      Gnoga.Log (Message => "Step_Toggle: " & Ada.Exceptions.Exception_Information (E) );
   end Step_Toggle;

   procedure Button_Press (Object : in out Gnoga.Gui.Base.Base_Type'Class; Mouse_Event : in Gnoga.Gui.Base.Mouse_Event_Record) is
      Row    : constant Field.Valid_Row    := Mouse_Event.Y / Button_Size + 1;
      Column : constant Field.Valid_Column := Mouse_Event.X / Button_Size + 1;
   begin -- Button_Press
      case Mouse_Event.Message is
      when Gnoga.Gui.Base.Click =>
         Sequentializer.Respond (Action => Button_Press, Cell => (Row => Row, Column => Column) );
      when others =>
         null;
      end case;
   exception -- Button_Press
   when E : others =>
      Gnoga.Log (Message => "Button_Press: " & Ada.Exceptions.Exception_Information (E) );
   end Button_Press;

   procedure Right_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class; Mouse_Event : in Gnoga.Gui.Base.Mouse_Event_Record) is
      Row    : constant Field.Valid_Row    := Mouse_Event.Y / Button_Size + 1;
      Column : constant Field.Valid_Column := Mouse_Event.X / Button_Size + 1;
   begin -- Right_Click
      case Mouse_Event.Message is
      when Gnoga.Gui.Base.Right_Click =>
         Sequentializer.Respond (Action => Right_Click, Cell => (Row => Row, Column => Column) );
      when others =>
         null;
      end case;
   exception -- Right_Click
   when E : others =>
      Gnoga.Log (Message => "Right_Click: " & Ada.Exceptions.Exception_Information (E) );
   end Right_Click;

   procedure When_Restart_Button (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      -- null;
   begin -- When_Restart_Button
      Sequentializer.Respond (Action => Restart);
   exception -- When_Restart_Button
   when E : others =>
      Gnoga.Log (Message => "When_Restart_Button: " & Ada.Exceptions.Exception_Information (E) );
   end When_Restart_Button;

   procedure Rules_Pressed (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
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
      Window.Alert (Message => Rules);
   exception -- Rules_Pressed
   when E : others =>
      Gnoga.Log (Message => "Rules_Pressed: " & Ada.Exceptions.Exception_Information (E) );
   end Rules_Pressed;

   procedure About_Pressed (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      -- null;
   begin -- About_Pressed
      Window.Alert (Message => "Mine Detector" & Latin_1.LF &
                               "Copyright (C) 2014 by" & Latin_1.LF &
                               "PragmAda Software Engineering" & Latin_1.LF &
                               "Released as Free Software under the terms" & Latin_1.LF &
                               "of the GNU Public License" & Latin_1.LF &
                               '"' & "Ada Inside" & '"');
   exception -- About_Pressed
   when E : others =>
      Gnoga.Log (Message => "About_Pressed: " & Ada.Exceptions.Exception_Information (E) );
   end About_Pressed;

   procedure Create_Level_Option_Menu is
      -- null;
   begin -- Create_Level_Option_Menu
      Add_Options : for I in Levels'range loop
         Level.Add_Option (Value => Levels (I).Name, Text => Levels (I).Name);
      end loop Add_Options;
   end Create_Level_Option_Menu;

   protected body Sequentializer is
      entry Respond (Action : in Action_ID; Cell : in Field.Cell_Location := (Row => 1, Column => 1) ) when True is
         -- null;
      begin -- Respond
         case Action is
         when Button_Press =>
            if Field.Operations.Game_State /= Field.Operations.In_Progress then
               Show_Game_Over;
            elsif Mode_Check.Checked then
               Field.Operations.Mark (Cell => Cell);
            else
               Field.Operations.Step (Cell => Cell);
            end if;
         when Right_Click =>
            if Field.Operations.Game_State /= Field.Operations.In_Progress then
               Show_Game_Over;
            else
               Field.Operations.Mark (Cell => Cell);
            end if;
         when Restart =>
            Field.Operations.Set_Mine_Count (Levels (Level.Selected_Index).Mines);
            Field.Operations.Reset;
         when Quiting =>
            Gnoga.Application.Singleton.End_Application;
         end case;
      end Respond;
   end Sequentializer;
begin -- User_IF
   Field.Operations.Set_Mine_Count (Levels (Default_Level).Mines);
   Gnoga.Application.Title ("Mine Detector");
   Gnoga.Application.HTML_On_Close ("Mine Detector ended.");
   Gnoga.Application.Open_URL;
   Gnoga.Application.Singleton.Initialize (Main_Window => Window);
   Window.Buffer_Connection;
   Big_View.Create (Parent => Window, Layout => Gnoga.Gui.View.Grid.Horizontal_Split, Set_Sizes => False);
   Big_View.Background_Color (Enum => Gnoga.Types.Colors.Light_Blue);
   Left_View.Create (Parent => Big_View.Panel (1, 1).all);
   Left_View.Background_Color (Enum => Gnoga.Types.Colors.Light_Blue);
   Button.Create
      (Parent => Left_View, Width => Field.Valid_Column'Last * Button_Size, Height => Field.Valid_Row'Last * Button_Size);
   Button.On_Mouse_Click_Handler (Handler => Button_Press'Access);
   Button.On_Mouse_Right_Click_Handler (Handler => Right_Click'Access);
   Drawing.Create (Parent => Left_View, Width => Button_Size, Height => Button_Size);
   Drawing.Hidden;

   Create_Flags : for I in Flag'Range loop
      Flag (I).Create (Parent => Left_View, Width => Button_Size, Height => Button_Size);
      Flag (I).Hidden;
   end loop Create_Flags;

   Draw_Flag_0 : declare
      Rectangle : constant Gnoga.Types.Rectangle_Type := (X => 0, Y => 0, Width => Button_Size, Height => Button_Size);

      Context : Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type;
   begin -- Draw_Flag_0
      Context.Get_Drawing_Context_2D (Canvas => Flag (Flag'First) );
      Context.Fill_Color (Value => Light_Green);
      Context.Fill_Rectangle (Rectangle => Rectangle);
      Context.Stroke_Color (Value => Black);
      Context.Line_Width (Value => 1);
      Context.Stroke_Rectangle (Rectangle => Rectangle);
      Context.Fill_Color (Value => Red);
      Context.Fill_Rectangle (Rectangle => (X => 7, Y => 5, Width => 15, Height => 10) );
      Context.Stroke_Color (Value => Black);
      Context.Begin_Path;
      Context.Move_To (X => 7, Y =>  5);
      Context.Line_To (X => 7, Y => 25);
      Context.Stroke;
   end Draw_Flag_0;

   Copy_0 : for I in Flag'First + 1 .. Flag'Last loop
      Copy_One : declare
         Context : Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type;
      begin -- Copy_One
         Context.Get_Drawing_Context_2D (Canvas => Flag (I) );
         Context.Draw_Image (Image => Flag (Flag'First), X => 0, Y => 0);
      end Copy_One;
   end loop Copy_0;

   Draw_Counts : for I in Flag'Range loop
      Draw_One : declare
         Context : Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type;
      begin -- Draw_One
         Context.Get_Drawing_Context_2D (Canvas => Flag (I) );
         Context.Font;
         Context.Fill_Color (Value => Black);
         Context.Fill_Text (Text => Character'Val (Character'Pos ('0') + I) & "", X => Button_Size / 2, Y => 25);
      end Draw_One;
   end loop Draw_Counts;

   Right_View.Create (Parent => Big_View.Panel (1, 2).all);
   Right_View.Background_Color (Enum => Gnoga.Types.Colors.Light_Blue);
   Mines_Left.Create (Parent => Right_View, Content => "0");
   Mines_Left.Width (Value => 100);
   Mines_Left.Text_Alignment (Value => Gnoga.Gui.Element.Center);
   Mines_Left.Display (Value => "block");
   Restart_Button.Create (Parent => Right_View, Content => "New Game");
   Restart_Button.Display (Value => "block");
   Restart_Button.On_Click_Handler (Handler => When_Restart_Button'Access);
   Level_Form.Create (Parent => Right_View);
   Level_Form.Display (Value => "block");
   Level.Create (Form => Level_Form);
   Level.Width (Value => 57);
   Create_Level_Option_Menu;
   Level.Selected (Index => Default_Level);
   Mark_Form.Create (Parent => Right_View);
   Mark_Form.Display (Value => "block");
   Mark_Check.Create (Form => Mark_Form);
   Mark_Check.Checked (Value => False);
   Mark_Check.On_Click_Handler (Handler => Mark_Toggle'Access);
   Mark_Label.Create (Form => Mark_Form, Label_For => Mark_Check, Content => "Auto Mark", Auto_Place => False);
   Step_Form.Create (Parent => Right_View);
   Step_Form.Display (Value => "block");
   Step_Check.Create (Form => Step_Form);
   Step_Check.Checked (Value => True);
   Step_Check.On_Click_Handler (Handler => Step_Toggle'Access);
   Step_Label.Create (Form => Step_Form, Label_For => Step_Check, Content => "Auto Step after Mark", Auto_Place => False);
   Rules.Create (Parent => Right_View, Content => "Rules");
   Rules.Display (Value => "block");
   Rules.On_Click_Handler (Handler => Rules_Pressed'Access);
   About.Create (Parent => Right_View, Content => "About");
   About.Display (Value => "block");
   About.On_Click_Handler (Handler => About_Pressed'Access);
   Quit.Create (Parent => Right_View, Content => "Quit");
   Quit.Display (Value => "block");
   Quit.On_Click_Handler (Handler => When_Close'Access);
   Game_Over.Create (Parent => Right_View, Content => You_Won_Message);
   Game_Over.Width (Value => 100);
   Game_Over.Text_Alignment (Value => Gnoga.Gui.Element.Center);
   Game_Over.Display (Value => "block");
   Mode_Form.Create (Parent => Right_View);
   Mode_Form.Display (Value => "block");
   Mode_Check.Create (Form => Mode_Form);
   Mode_Check.Checked (Value => False);
   Mode_Label.Create (Form => Mode_Form, Label_For => Mode_Check, Content => "Mark", Auto_Place => False);
   Window.Buffer_Connection (Value => False);
   Field.Operations.Reset;
   Gnoga.Application.Singleton.Message_Loop;
exception -- User_IF
when E : others =>
   Gnoga.Log (Message => "User_IF: " & Ada.Exceptions.Exception_Information (E) );
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
