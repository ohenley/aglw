with Interfaces.C;
with Win32;
with System;

package Aglw.Windows is

   procedure prepare_window (window : in out Aglw.Window);
   procedure create_context;
   procedure swap;
   procedure open_window (window : in out Aglw.Window);

   procedure get_client_width_height (width : in out Natural; height : in out Natural);

end Aglw.Windows;
