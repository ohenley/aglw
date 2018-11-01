with Ada.Unchecked_Conversion;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Aglw.Windows;
pragma Elaborate (Aglw.Windows);
with Ada.Real_Time;

package body Aglw is

   main_window : Window;

   protected guard is
      -- guard OpenGL context creation execution order
      procedure go_create_context;
      entry create_context;

      -- guard closing of tasks
      procedure go_close;
      function can_close return Boolean;

      -- order/guard call to render
      procedure go_render;
      function can_render return Boolean;

   private
      may_create_context : Boolean := False;
      may_close : Boolean := False;
      may_render : Boolean := False;
   end guard;

   protected body guard is
      procedure go_create_context is
      begin
         may_create_context := True;
      end;

      entry create_context when may_create_context is
      begin
         null;
      end;

      procedure go_close is
      begin
         may_close := True;
      end;

      function can_close return Boolean is
      begin
         return may_close;
      end;

      procedure go_render is
      begin
         may_render := true;
      end;

      function can_render return Boolean is
      begin
         return may_render;
      end;
   end;

   task render_thread is
      entry init;
      entry render;
   end;

   task body render_thread is
   begin
      accept init do
         guard.create_context;
         Aglw.Windows.create_context;
         main_window.start_cb.all;
         guard.go_render;
         return;
      end;
      loop
         accept render do
            main_window.update_cb(main_window.width, main_window.height);
            Aglw.Windows.swap;
            return;
         end;
         exit when guard.can_close;
      end loop;
   end;

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

         task body window_thread is
         begin
            Aglw.Windows.prepare_window (main_window);
            Aglw.Windows.open_window (main_window);
            guard.go_close;
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

         render_thread.init;

         while true
         loop
            exit when guard.can_close;
            delay 0.016; -- 60 fps
            render_thread.render;
         end loop;

      end;
   end;

   procedure ask_draw is
   begin
      if guard.can_render then
         render_thread.render;
      end if;
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

   procedure set_update_callback (cb : Update_Callback_Procedure) is
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
      Ada.Text_IO.Put_Line ("window_just_opened");
      guard.go_create_context;
   end;

end Aglw;
