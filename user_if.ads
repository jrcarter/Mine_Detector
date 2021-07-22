-- Mine Detector Game
-- Copyright (C) 2021 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Provide the user interface
--
-- V7.1 2021 Jun 15          Fix GNAT 11 circular-elaboration error
-- V7.0 2014 Dec 01          First Gnoga version
--
with Field;

package User_IF is
   procedure Play_Game;
   -- Called from main program to start the game
   -- Returns when the user quits

   procedure Display_Count (Count    : in Field.Valid_Count;
                            Stepped  : in Boolean;
                            Cell     : in Field.Cell_Location);

   procedure Display_Mark (Count : in Field.Valid_Count; Cell : in Field.Cell_Location); -- Display a marked cell

   procedure Display_Mine (Cell : in Field.Cell_Location); -- Display a mine.

   procedure Display_Blank (Cell : in Field.Cell_Location); -- Display a blank cell

   procedure Display_To_Go (To_Go : in Integer); -- Display # of mines still to mark; can be negative

   procedure Reset_Screen; -- Return to start of game condition

   function Auto_Marking return Boolean; -- Get auto-marking state

   function Extended_Stepping return Boolean; -- Get extended-stepping (after mark) state
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
