with Ada.Unchecked_Conversion;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Aglw.Windows;
with Ada.Real_Time;

package body Aglw is

   procedure init (window : in out Aglw.Window;
                   window_x : Natural := 0;
                   window_y : Natural := 0;
                   window_width : Natural := 200;
                   window_height : Natural := 200;
                   window_title : String := "Title";
                   window_class : String := "Core") is
   begin
      window.x := window_x;
      window.y := window_y;
      window.height := window_height;
      window.width := window_width;
      window.title := Ada.Strings.Unbounded.To_Unbounded_String(window_title);
      window.class := Ada.Strings.Unbounded.To_Unbounded_String(window_class);
   end init;

   protected flag is
      -- Syncronize OpenGL context creation in render
      procedure go_create_context;
      entry create_context;

      -- Syncronize closing of window and render
      procedure go_close;
      function is_ok_close return Boolean;

   private
      can_create_context : Boolean := False;
      can_close : Boolean := False;
   end flag;

   protected body flag is
      procedure go_create_context is
      begin
         can_create_context := True;
      end go_create_context;


      procedure go_close is
      begin
         can_close := True;
      end go_close;

      function is_ok_close return Boolean is
      begin
         return can_close;
      end is_ok_close;

      entry create_context when can_create_context is
      begin
         null;
      end create_context;

   end flag;



   procedure start_window (window : in out Aglw.Window; start : access procedure; setup: access procedure; render : access procedure) is

      terminated : Boolean := False;
      context_created : Boolean := False;
      now : Ada.Real_Time.Time := Ada.Real_Time.Clock;
      old_now : Ada.Real_Time.Time := now;
      delta_time : Duration := 0.0;

      function get_pseudo_frame_average return duration is
      begin
         delta_time := delta_time * 0.5 + Ada.Real_Time.To_Duration (Ada.Real_Time."-" (now, old_now)) * 0.5;
         return delta_time;
      end;

      task window_thread;

      task body window_thread is
      begin
         Aglw.Windows.prepare_window (window);
         flag.go_create_context;
         Aglw.Windows.open_window (window);
         flag.go_close;
      end;
   begin

      flag.create_context;
      Aglw.Windows.create_context;
      -- Client Start Render callback
      start.all;
      setup.all;


      while True
      loop

         exit when flag.is_ok_close;

         now := Ada.Real_Time.Clock;
         render.all;
         Aglw.Windows.swap;
         Aglw.Windows.monitor.set_delay (get_pseudo_frame_average);
         --Ada.Text_IO.Put_Line (Duration'Image(delta_time));
         old_now := now;

      end loop;

   end start_window;

end Aglw;
