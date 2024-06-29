-----------------------------------------------------------------------
--  util-http-tools -- HTTP Utility Library
--  Copyright (C) 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package Util.Http.Tools is

   --  Save the content stored in the HTTP response into a file.
   --  The response headers are saved only when <b>Save_Headers</b> is true.
   procedure Save_Response (Path         : in String;
                            Response     : in Abstract_Response'Class;
                            Save_Headers : in Boolean := False);

end Util.Http.Tools;
