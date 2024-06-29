-----------------------------------------------------------------------
--  util-commands-text_io -- Output using Ada.Text_IO
--  Copyright (C) 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Text_IO;
package Util.Commands.Text_IO is
  new Util.Commands.IO (Count_Type => Ada.Text_IO.Positive_Count,
                        Put        => Ada.Text_IO.Put,
                        Put_Line   => Ada.Text_IO.Put_Line,
                        New_Line   => Ada.Text_IO.New_Line);
