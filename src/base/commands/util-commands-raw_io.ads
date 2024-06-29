-----------------------------------------------------------------------
--  util-commands-raw_io -- Output using raw console IO
--  Copyright (C) 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
package Util.Commands.Raw_IO is
  new Util.Commands.IO (Count_Type => Positive,
                        Put        => Util.Commands.Put_Raw,
                        Put_Line   => Util.Commands.Put_Raw_Line,
                        New_Line   => Util.Commands.New_Line_Raw);
