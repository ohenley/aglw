with Ada.Unchecked_Conversion;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Aglw.Windows;
with Ada.Real_Time;

package body Aglw is

   main_window : Window;

   protected hub is
      -- Syncronize OpenGL context creation in render
      procedure go_create_context;
      entry create_context;

      -- Syncronize closing of window and render
      procedure go_close;
      function close return Boolean;

      procedure launch_gl;
      procedure ask_to_render;
      procedure update_render;

   private
      can_create_context : Boolean := False;
      can_close : Boolean := False;
      render_update_cb : Callback_Procedure := null;
      do_render : Boolean := False;
   end hub;

   protected body hub is
      procedure go_create_context is
      begin
         can_create_context := True;
      end;

      entry create_context when can_create_context is
      begin
         null;
      end;

      procedure go_close is
      begin
         can_close := True;
      end;

      function close return Boolean is
      begin
         return can_close;
      end;

      procedure launch_GL is
      begin
         render_update_cb := main_window.update_cb;
         main_window.start_cb.all;
      end;

      procedure ask_to_render is
      begin
         do_render := True;
      end;

      procedure update_render is
      begin
         if do_render = true then
            do_render := false;
            render_update_cb.all;
            Aglw.Windows.swap;
         end if;
      end;

   end hub;

   procedure open_window (title : String := "Window name";
                          x : Natural := 0;
                          y : Natural := 0;
                          width : Natural := 200;
                          height : Natural := 200) is
   begin

      main_window.title := Ada.Strings.Unbounded.To_Unbounded_String(title);
      main_window.x := x;
      main_window.y := y;
      main_window.width := width;
      main_window.height := height;

      declare
         task window_thread;
         task update_thread;

         task body window_thread is
         begin
            Aglw.Windows.prepare_window (main_window);
            Aglw.Windows.open_window (main_window);
            hub.go_close;
         end;

         task body update_thread is
         begin
            while True
            loop
               exit when hub.close;
               hub.ask_to_render;
            end loop;
         end;
      begin

         if main_window.start_cb = null then
            Ada.Text_IO.Put_Line ("You did not set a Start procedure! eg. Aglw.set_start_callback (my_start_procedure'Access)");
            return;
         end if;

         if main_window.update_cb = null then
            Ada.Text_IO.Put_Line ("You did not set an Update procedure! eg. Aglw.set_start_callback (my_looping_procedure'Access)");
            return;
         end if;

         hub.create_context;
         Aglw.Windows.create_context;

         hub.launch_GL;
         while True
         loop
            exit when hub.close;
            hub.update_render;
         end loop;

      end;
   end;

   procedure ask_draw is
   begin
      hub.ask_to_render;
   end;

   procedure close_window is
   begin
      null;
   end;

   procedure deinit is
   begin
      null;
   end;

   -- Callbacks Setters
   procedure set_start_callback (cb : Callback_Procedure) is
   begin
      main_window.start_cb := cb;
   end;

   procedure set_update_callback (cb : Callback_Procedure) is
   begin
      main_window.update_cb := cb;
   end;

   procedure set_error_callback (cb : Callback_Procedure) is
   begin
      main_window.error_cb := cb;
   end;

   procedure set_resize_callback (cb : Callback_Procedure) is
   begin
      main_window.resize_cb := cb;
   end;

   procedure set_motion_callback (cb : Callback_Procedure) is
   begin
      main_window.motion_cb := cb;
   end;

   procedure set_visibility_callback (cb : Callback_Procedure) is
   begin
      main_window.visibility_cb := cb;
   end;

   -- Window Setters
   procedure set_window_position (x : Natural; y : Natural) is
   begin
      null;
   end;

   procedure set_window_size (width : Natural; height : Natural) is
   begin
      null;
   end;

   procedure set_fullscreen is
   begin
      null;
   end;

   procedure set_cursor is
   begin
      null;
   end;


   procedure window_just_opened is
   begin
      hub.go_create_context;
   end;

end Aglw;
