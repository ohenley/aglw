with Ada.Unchecked_Conversion;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Interfaces.C.Strings;

with System;
with Win32;
with Win32.Windef;
with Win32.Winuser;

with Interfaces.C;
with Win32.Wingdi;

with Win32.Winmain;
with Win32.Winnt;
with Win32.Gl;

package body Aglw.Windows is


   use Interfaces.C;
   use type System.Address;

   function CP (C_Str : Win32.CHAR_Array) return Win32.LPCSTR is
      function UC is new Ada.Unchecked_Conversion (System.Address, Win32.LPCSTR);
   begin
      return UC (C_Str (C_Str'First)'Address);
   end CP;

   function HANDLE_TO_LONGPTR is new Ada.Unchecked_Conversion (Win32.Winnt.HANDLE, Win32.LONG_PTR);
   function LONGPTR_TO_HANDLE is new Ada.Unchecked_Conversion (Win32.LONG_PTR, Win32.Winnt.HANDLE);
   function CHARPTR_TO_LPCSTR is new Ada.Unchecked_Conversion (Interfaces.C.Strings.chars_ptr, Win32.LPCSTR);
   function MOD_TO_INT is new Ada.Unchecked_Conversion (Interfaces.C.unsigned_long, Win32.INT);

   CLIENTWND     : constant := 0;
   APPMENU       : constant := 1000;
   APPICON       : constant := 1001;
   CWUSEDEF      : constant := 16#80000000#;
   CW_USEDEF     : Win32.INT := MOD_to_INT (CWUSEDEF);

   ps            : aliased Win32.Winuser.PAINTSTRUCT;
   hWndFrame     : Win32.Windef.HWND;

   hInst         : Win32.Windef.HINSTANCE;

   bResult       : Win32.BOOL;
   wndClass_p    : aliased Win32.Winuser.WNDCLASS;
   hDC           : Win32.Windef.HDC;
   hRC_render    : Win32.Windef.HGLRC;

   rect_t        : aliased Win32.Windef.RECT;

   procedure swap is
   begin
      bResult := Win32.Wingdi.SwapBuffers(hDC);
   end swap;

   protected body monitors is

      function get_delay return Duration is
      begin
         return to_delay;
      end get_delay;

      procedure set_delay (new_delay : Duration) is
      begin
         to_delay := new_delay;
      end;

   end monitors;

   function Create_App_Window (window : in out Aglw.Window; hInst : Win32.Windef.HINSTANCE) return Win32.Windef.HWND
   is
      use Interfaces.C.Strings;
      title : Interfaces.C.Strings.chars_ptr := New_String (Ada.Strings.Unbounded.To_String(window.title));
      APPTITLE     : Win32.LPCSTR := CHARPTR_TO_LPCSTR(title);
      class : Interfaces.C.Strings.chars_ptr := New_String (Ada.Strings.Unbounded.To_String(window.class));
      APPCLASS     : Win32.LPCSTR := CHARPTR_TO_LPCSTR (class);
   begin
      return Win32.Winuser.CreateWindow
        (lpClassName  => APPCLASS,
         lpWindowName => APPTITLE,
         dwStyle      => Win32.Winuser.WS_OVERLAPPEDWINDOW,
         X            => Win32.Winuser.CW_USEDEFAULT,
         Y            => Win32.Winuser.CW_USEDEFAULT,
         nWidth       => Win32.INT(window.width),
         nHeight      => Win32.INT(window.height),
         hWndParent   => System.Null_Address,
         hMenu        => System.Null_Address,
         hInstance    => hInst,
         lpParam      => System.Null_Address);
   end Create_App_Window;

   function Create_Proc (hWndFrame : Win32.Windef.HWND) return Win32.BOOL is
   begin
      return Win32.TRUE;
   end Create_Proc;

   procedure Paint_Proc (hWndFrame : Win32.Windef.HWND) is
      hDC_p : Win32.Windef.HDC;
   begin
      hDC_p := Win32.Winuser.BeginPaint (hWndFrame, ps'Access);
      delay (monitor.get_delay);
      bResult := Win32.Winuser.EndPaint (hWndFrame, ps'Access);
   end Paint_Proc;

   procedure Destroy_Proc (hWndFrame : Win32.Windef.HWND) is
   begin
      Win32.Winuser.PostQuitMessage (0);
   end Destroy_Proc;

   function wnd_proc
     (hWndFrame : Win32.Windef.HWND;
      wMsg      : Win32.UINT;
      wParam_p  : Win32.WPARAM;
      lParam_p  : Win32.LPARAM)
      return Win32.LRESULT;
   pragma Convention (Stdcall, wnd_proc);

   function wnd_proc
     (hWndFrame : Win32.Windef.HWND;
      wMsg      : Win32.UINT;
      wParam_p  : Win32.WPARAM;
      lParam_p  : Win32.LPARAM)
      return Win32.LRESULT
   is
      hWndClient : Win32.Windef.HWND;
   begin
      case wMsg is
         when Win32.Winuser.WM_CREATE =>
            bResult := Create_Proc (hWndFrame);

         when Win32.Winuser.WM_PAINT =>
            Paint_Proc (hWndFrame);

         when Win32.Winuser.WM_ERASEBKGND =>
            return Win32.LRESULT(Win32.True);

         when Win32.Winuser.WM_DESTROY =>
            Destroy_Proc (hWndFrame);

         when others =>
            hWndClient :=
              Win32.Windef.HWND (LONGPTR_TO_HANDLE
                                 (Win32.Winuser.GetWindowLongPtr
                                    (hWndFrame,
                                         CLIENTWND)));
      end case;

      return Win32.Winuser.DefFrameProc
        (hWndFrame,
         hWndClient,
         wMsg,
         wParam_p,
         lParam_p);

   end wnd_proc;

   function Register_App_Class (window : Aglw.Window; hInst : Win32.Windef.HINSTANCE) return Win32.BOOL
   is
      use type Win32.Windef.ATOM;
      use Interfaces.C.Strings;
      class : Interfaces.C.Strings.chars_ptr := New_String (Ada.Strings.Unbounded.To_String(window.class));
      APPCLASS     : Win32.LPCSTR := CHARPTR_TO_LPCSTR (class);
   begin

      wndClass_p.style      := Win32.Winuser.CS_HREDRAW or Win32.Winuser.CS_VREDRAW;
      wndClass_p.cbClsExtra := 0;
      wndClass_p.cbWndExtra := Win32.LONG_PTR'Size / 8;
      wndClass_p.hCursor    := Win32.Winuser.LoadCursor (System.Null_Address, Win32.LPCSTR (Win32.Winuser.IDC_ARROW));
      wndClass_p.hInstance  := hInst;
      wndClass_p.lpfnWndProc   := wnd_proc'Access;
      wndClass_p.hIcon         := Win32.Winuser.LoadIcon (hInst, Win32.LPCSTR (Win32.Winuser.MAKEINTRESOURCE (APPICON)));
      wndClass_p.hbrBackground := Win32.Windef.HBRUSH (Win32.Wingdi.GetStockObject (Win32.Wingdi.WHITE_BRUSH));
      wndClass_p.lpszMenuName  := Win32.LPCSTR (Win32.Winuser.MAKEINTRESOURCE (APPMENU));
      wndClass_p.lpszClassName := APPCLASS;

      if Win32.Winuser.RegisterClass (wndClass_p'Access) = 0 then
         return Win32.FALSE;
      end if;

      return Win32.TRUE;
   end Register_App_Class;

   procedure Unregister_App_Class (window : Aglw.Window; hInst : Win32.Windef.HINSTANCE) is
      use Interfaces.C.Strings;
      class : Interfaces.C.Strings.chars_ptr := New_String (Ada.Strings.Unbounded.To_String(window.class));
      APPCLASS     : Win32.LPCSTR := CHARPTR_TO_LPCSTR (class);
   begin
      bResult := Win32.Winuser.UnregisterClass (APPCLASS, hInst);
   end Unregister_App_Class;


   procedure prepare_window (window : in out Aglw.Window) is

      use type System.Address;
      use type Interfaces.C.int;
      use type Win32.BOOL;
   begin

      hInst := Win32.Winmain.Get_hInstance;

      if Win32.Winmain.Get_hPrevInstance = System.Null_Address then
         if Register_App_Class (window, hInst) = Win32.FALSE then
            return;
         end if;
      end if;

      -- Create the window
      hWndFrame := Create_App_Window (window, hInst);

      -- Obtain a device context for the window
      hDC := Win32.Winuser.GetDC (hWndFrame);

      -- Setup OpenGL
      declare
         pdf                        : aliased Win32.Wingdi.PIXELFORMATDESCRIPTOR;
         suggested_pdf              : aliased Win32.Wingdi.PIXELFORMATDESCRIPTOR;
         suggested_pixel_format     : Interfaces.C.int;
         result                     : Interfaces.C.Int;
         use Interfaces.C;

      begin
         pdf.nSize       := Win32.Wingdi.PIXELFORMATDESCRIPTOR'Size/8;
         pdf.nVersion    := 1;
         pdf.dwFlags     := Win32.Wingdi.PFD_DRAW_TO_WINDOW or Win32.Wingdi.PFD_SUPPORT_OPENGL or Win32.Wingdi.PFD_DOUBLEBUFFER;
         pdf.iPixelType  := Win32.Wingdi.PFD_TYPE_RGBA;
         pdf.cColorBits  := 32;
         pdf.cAlphaBits  := 8;
         pdf.cDepthBits  := 24;
         pdf.iLayerType  := Win32.Wingdi.PFD_MAIN_PLANE;
         suggested_pixel_format := Win32.Wingdi.ChoosePixelFormat (hDC, pdf'Unchecked_Access);
         result          := Win32.Wingdi.DescribePixelFormat (hDC, suggested_pixel_format, Win32.Wingdi.PIXELFORMATDESCRIPTOR'Size/8, suggested_pdf'Unchecked_Access);
         bResult         := Win32.Wingdi.SetPixelFormat (hDC, suggested_pixel_format, suggested_pdf'Unchecked_Access);
      end;

   end prepare_window;

   procedure create_context is
   begin
      hRC_render := Win32.Wingdi.wglCreateContext (hDC);
      bResult := Win32.Wingdi.wglMakeCurrent(hDC,hRC_render);
   end create_context;

   procedure get_client_width_height (width : in out Natural; height : in out Natural) is
      lpRect : Win32.Windef.LPRECT := rect_t'Access;
   begin
      bResult := Win32.Winuser.GetClientRect (hWndFrame, lpRect);
      width := Natural (lpRect.right);
      height := Natural (lpRect.bottom);
   end get_client_width_height;

   procedure open_window (window : in out Aglw.Window) is
      msg_p      : Win32.Winuser.LPMSG;
      longResult : Win32.LONG_PTR;
      ret : Win32.BOOL := Win32.BOOL(1);
      use type System.Address;
      use type Interfaces.C.int;
      use type Win32.BOOL;
   begin

      bResult := Win32.Winuser.ShowWindow (hWndFrame, Win32.Winmain.Get_nCmdShow);

      msg_p := new Win32.Winuser.MSG;

      while ret /= Win32.FALSE
      loop
         ret := Win32.Winuser.GetMessage (msg_p, System.Null_Address, 0, 0);

         if ret /= Win32.BOOL(-1) then
            bResult := Win32.Winuser.TranslateMessage (Win32.Winuser.ac_MSG_t (msg_p));
            longResult := Win32.Winuser.DispatchMessage (Win32.Winuser.ac_MSG_t (msg_p));
         else
            return;
         end if;

      end loop;

   end open_window;

   --     procedure open_window (window : in out Aglw.Window) is
   --        msg_p      : Win32.Winuser.LPMSG;
   --        longResult : Win32.LONG_PTR;
   --        use type System.Address;
   --        use type Interfaces.C.int;
   --        use type Win32.BOOL;
   --     begin
   --
   --        bResult := Win32.Winuser.ShowWindow (hWndFrame, Win32.Winmain.Get_nCmdShow);
   --
   --        bResult := Win32.Winuser.UpdateWindow (hWndFrame);
   --
   --        msg_p := new Win32.Winuser.MSG;
   --
   --        while True
   --        loop
   --           declare
   --              use Interfaces.C;
   --              use Win32.Winuser;
   --              use System;
   --              message : Win32.UINT;
   --              quit : Win32.UINT;
   --           begin
   --              while PeekMessage (msg_p, Null_Address, 0, 0, PM_REMOVE) /= Win32.FALSE
   --              loop
   --                 bResult := TranslateMessage (ac_MSG_t (msg_p));
   --                 longResult := DispatchMessage (ac_MSG_t (msg_p));
   --              end loop;
   --
   --              message := msg_p.message;
   --              quit := Win32.UINT(WM_QUIT) or Win32.UINT(WM_CLOSE);
   --
   --              exit when message = quit;
   --           end;
   --        end loop;
   --
   --        Unregister_App_Class (window, hInst);
   --     end open_window;

end Aglw.Windows;
