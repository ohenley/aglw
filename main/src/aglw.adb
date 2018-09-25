with Ada.Unchecked_Conversion;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Aglw.Windows;
with Ada.Real_Time;

package body Aglw is

   protected hub is
      -- Syncronize OpenGL context creation in render
      procedure go_create_context;
      entry create_context;

      -- Syncronize closing of window and render
      procedure go_close;
      function is_ok_close return Boolean;

      procedure launch_gl;

      procedure ask_to_render;

      procedure update_render;

   private
      can_create_context : Boolean := False;
      can_close : Boolean := False;
      render_update_cb : Callback_Procedure;
      do_render : Boolean := False;
   end hub;

   main_window : Window;

   protected body hub is
      procedure go_create_context is
      begin
         can_create_context := True;
      end go_create_context;

      entry create_context when can_create_context is
      begin
         null;
      end create_context;

      procedure go_close is
      begin
         can_close := True;
      end go_close;

      function is_ok_close return Boolean is
      begin
         return can_close;
      end is_ok_close;

      procedure launch_GL is
      begin
         render_update_cb := main_window.update_cb;
         main_window.start_cb.all;
      end launch_gl;

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
               exit when hub.is_ok_close;
               hub.ask_to_render;
            end loop;
         end;
      begin
         hub.create_context;
         Aglw.Windows.create_context;

         if main_window.start_cb /= null and main_window.update_cb /= null then

            hub.launch_GL;
            while True
            loop
               exit when hub.is_ok_close;
               hub.update_render;
            end loop;
         end if;
      end;

   end;

   procedure ask_draw is
   begin
      hub.ask_to_render;
   end;

   procedure leave_update is
   begin
      null;
   end;

   procedure close_window is
   begin
      null;
   end;

   procedure deinit is
   begin
      null;
   end;

   -- Setters

   -- callbacks
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

   -- window
   procedure set_window_position (pos_x : Natural := 0;
                                  pos_y : Natural := 0) is
   begin
      null;
   end;

   procedure set_window_size (width : Natural := 200;
                              height : Natural := 200) is
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


--     procedure init (window : in out Aglw.Window;
--                     window_x : Natural := 0;
--                     window_y : Natural := 0;
--                     window_width : Natural := 200;
--                     window_height : Natural := 200;
--                     window_title : String := "Title") is
--     begin
--        null;
--     end;
--
--
--     procedure start_window (window : in out Aglw.Window;
--                             start : access procedure;
--                             setup: access procedure;
--                             render : Callback_Procedure) is
--     begin
--        null;
--     end;




--     procedure open_window (window : in out Aglw.Window;
--                            window_x : Natural := 0;
--                            window_y : Natural := 0;
--                     window_width : Natural := 200;
--                     window_height : Natural := 200;
--                     window_title : String := "Title") is
--     begin
--        window.x := window_x;
--        window.y := window_y;
--        window.height := window_height;
--        window.width := window_width;
--        window.title := Ada.Strings.Unbounded.To_Unbounded_String(window_title);
--     end init;

--     function get_pseudo_frame_average return duration is
--     begin
--        delta_time := delta_time * 0.5 + Ada.Real_Time.To_Duration (Ada.Real_Time."-" (now, old_now)) * 0.5;
--        return delta_time;
--     end;



--  procedure start_window (window : in out Aglw.Window; start : access procedure; setup: access procedure; render : Callback_Procedure) is
--
--        --now : Ada.Real_Time.Time := Ada.Real_Time.Clock;
--        --old_now : Ada.Real_Time.Time := now;
--        --delta_time : Duration := 0.0;
--
--        task update;
--        task window_thread;
--
--        task body window_thread is
--        begin
--           Aglw.Windows.prepare_window (window);
--           flag.go_create_context;
--           Aglw.Windows.open_window (window);
--           flag.go_close;
--        end;
--
--        task body update is
--        begin
--           while True
--           loop
--              exit when flag.is_ok_close;
--              flag.ask_to_render;
--           end loop;
--        end;
--
--     begin
--
--        flag.create_context;
--        Aglw.Windows.create_context;
--        flag.launch_gl(start,setup, render);
--
--        while True
--        loop
--           exit when flag.is_ok_close;
--           flag.render;
--        end loop;
--
--     end start_window;
--
--     procedure redraw is
--     begin
--        flag.ask_to_render;
--     end;

