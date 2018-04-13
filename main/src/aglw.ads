
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Real_Time;

package Aglw is

   type Window is tagged
      record
         x : Natural := 0;
         y : Natural := 0;
         height : Natural := 200;
         width : Natural := 200;
         title : Unbounded_String := To_Unbounded_String("Title");
         class : Unbounded_String := To_Unbounded_String("Core");
      end record;

   type Window_A is access all Window;

   procedure init (window : in out Aglw.Window;
                   window_x : Natural := 0;
                   window_y : Natural := 0;
                   window_width : Natural := 200;
                   window_height : Natural := 200;
                   window_title : String := "Title";
                   window_class : String := "Core");

   procedure start_window (window : in out Aglw.Window;
                           start : access procedure;
                           setup: access procedure;
                           render : access procedure);

end Aglw;
