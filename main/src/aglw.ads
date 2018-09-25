
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Aglw is

   -- Types
   type Callback_Procedure is access procedure;

   type Window is tagged
      record

         x : Natural := 0;
         y : Natural := 0;
         height : Natural := 400;
         width : Natural := 400;
         title : Unbounded_String := To_Unbounded_String("Title");
         class : Unbounded_String := To_Unbounded_String("Core");

         start_cb : Callback_Procedure := null;
         setup_cb : Callback_Procedure := null;
         stop_cb : Callback_Procedure := null;
         update_cb : Callback_Procedure := null;
         error_cb : Callback_Procedure := null;
         resize_cb : Callback_Procedure := null;
         motion_cb : Callback_Procedure := null;
         visibility_cb : Callback_Procedure := null;

      end record;

   -- Library Controls
   procedure open_window (title : String := "Window name";
                          x : Natural := 0;
                          y : Natural := 0;
                          width : Natural := 200;
                          height : Natural := 200);
   procedure ask_draw;
   procedure leave_update;
   procedure close_window;

   -- Callback Setters
   procedure set_start_callback (cb : Callback_Procedure);
   procedure set_update_callback (cb : Callback_Procedure);
   procedure set_error_callback (cb : Callback_Procedure);
   procedure set_resize_callback (cb : Callback_Procedure);
   procedure set_motion_callback (cb : Callback_Procedure);
   procedure set_visibility_callback (cb : Callback_Procedure);

   -- Window Setters
   procedure set_window_position (pos_x : Natural := 0; pos_y : Natural := 0);
   procedure set_window_size (width : Natural := 200; height : Natural := 200);
   procedure set_fullscreen;
   procedure set_cursor;

private

   procedure window_just_opened;

end Aglw;
