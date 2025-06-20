-- Mine Detector Game
-- Copyright (C) 2014 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Encapsulates the definition of the mine field
--
-- V7.0 2014 Dec 01          First Gnoga version
--
package Field is
   subtype Valid_Row    is Positive range 1 .. 16; -- Size of the mine field
   subtype Valid_Column is Positive range 1 .. 30;

   type Cell_Location is record
      Row    : Valid_Row    := 1;
      Column : Valid_Column := 1;
   end record;

   subtype Valid_Count is Natural range 0 .. 9; -- Count of # of mines in a cell & its neighbors
end Field;
--
-- SPDX-License-Identifier: GPL-2.0-only
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com
