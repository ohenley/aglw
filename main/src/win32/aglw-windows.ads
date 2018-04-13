with Interfaces.C;
with Win32;
with System;

package Aglw.Windows is

   procedure prepare_window (window : in out Aglw.Window);
   procedure create_context;
   procedure swap;
   procedure open_window (window : in out Aglw.Window);

   protected type monitors is
      function get_delay return Duration;
      procedure set_delay (new_delay : Duration);
   private
      to_delay : Duration := 0.0;
   end monitors;

   monitor : aliased monitors;

   procedure get_client_width_height (width : in out Natural; height : in out Natural);

end Aglw.Windows;
