module Types (F : Ctypes.TYPE) = struct

  module Init = struct
    let timer = F.constant "SDL_INIT_TIMER" F.uint32_t
    let audio = F.constant "SDL_INIT_AUDIO" F.uint32_t
    let video = F.constant "SDL_INIT_VIDEO" F.uint32_t
    let joystick = F.constant "SDL_INIT_JOYSTICK" F.uint32_t
    let haptic = F.constant "SDL_INIT_HAPTIC" F.uint32_t
    let gamecontroller = F.constant "SDL_INIT_GAMECONTROLLER" F.uint32_t
    let events = F.constant "SDL_INIT_EVENTS" F.uint32_t
    let everything = F.constant "SDL_INIT_EVERYTHING" F.uint32_t
    let noparachute = F.constant "SDL_INIT_NOPARACHUTE" F.uint32_t
  end

  module Hint = struct
    let default = F.constant "SDL_HINT_DEFAULT" F.int
    let normal = F.constant "SDL_HINT_NORMAL" F.int
    let override = F.constant "SDL_HINT_OVERRIDE" F.int
  end

  module Log = struct
    let category_application = F.constant "SDL_LOG_CATEGORY_APPLICATION" F.int
    let category_error = F.constant "SDL_LOG_CATEGORY_ERROR" F.int
    let category_system = F.constant "SDL_LOG_CATEGORY_SYSTEM" F.int
    let category_audio = F.constant "SDL_LOG_CATEGORY_AUDIO" F.int
    let category_video = F.constant "SDL_LOG_CATEGORY_VIDEO" F.int
    let category_render = F.constant "SDL_LOG_CATEGORY_RENDER" F.int
    let category_input = F.constant "SDL_LOG_CATEGORY_INPUT" F.int
    let category_custom = F.constant "SDL_LOG_CATEGORY_CUSTOM" F.int

    let priority_verbose = F.constant "SDL_LOG_PRIORITY_VERBOSE" F.int
    let priority_debug = F.constant "SDL_LOG_PRIORITY_DEBUG" F.int
    let priority_info = F.constant "SDL_LOG_PRIORITY_INFO" F.int
    let priority_warn = F.constant "SDL_LOG_PRIORITY_WARN" F.int
    let priority_error = F.constant "SDL_LOG_PRIORITY_ERROR" F.int
    let priority_critical = F.constant "SDL_LOG_PRIORITY_CRITICAL" F.int
  end

  module Blend = struct
    let mode_none = F.constant "SDL_BLENDMODE_NONE" F.uint
    let mode_blend = F.constant "SDL_BLENDMODE_BLEND" F.uint
    let mode_add = F.constant "SDL_BLENDMODE_ADD" F.uint
    let mode_mod = F.constant "SDL_BLENDMODE_MOD" F.uint

    let add = F.constant "SDL_BLENDOPERATION_ADD" F.int
    let subtract = F.constant "SDL_BLENDOPERATION_SUBTRACT" F.int
    let rev_subtract = F.constant "SDL_BLENDOPERATION_REV_SUBTRACT" F.int
    let maximum = F.constant "SDL_BLENDOPERATION_MAXIMUM" F.int
    let minimum = F.constant "SDL_BLENDOPERATION_MINIMUM" F.int

    let zero = F.constant "SDL_BLENDFACTOR_ZERO" F.int
    let one = F.constant "SDL_BLENDFACTOR_ONE" F.int
    let src_color = F.constant "SDL_BLENDFACTOR_SRC_COLOR" F.int
    let one_minus_src_color = F.constant "SDL_BLENDFACTOR_ONE_MINUS_SRC_COLOR" F.int
    let src_alpha = F.constant "SDL_BLENDFACTOR_SRC_ALPHA" F.int
    let one_minus_src_alpha = F.constant "SDL_BLENDFACTOR_ONE_MINUS_SRC_ALPHA" F.int
    let dst_color = F.constant "SDL_BLENDFACTOR_DST_COLOR" F.int
    let one_minus_dst_color = F.constant "SDL_BLENDFACTOR_ONE_MINUS_DST_COLOR" F.int
    let dst_alpha = F.constant "SDL_BLENDFACTOR_DST_ALPHA" F.int
    let one_minus_dst_alpha = F.constant "SDL_BLENDFACTOR_ONE_MINUS_DST_ALPHA" F.int
  end

  module Pixel = struct
    let format_unknown = F.constant "SDL_PIXELFORMAT_UNKNOWN" F.uint32_t
    let format_index1lsb = F.constant "SDL_PIXELFORMAT_INDEX1LSB" F.uint32_t
    let format_index1msb = F.constant "SDL_PIXELFORMAT_INDEX1MSB" F.uint32_t
    let format_index4lsb = F.constant "SDL_PIXELFORMAT_INDEX4LSB" F.uint32_t
    let format_index4msb = F.constant "SDL_PIXELFORMAT_INDEX4MSB" F.uint32_t
    let format_index8 = F.constant "SDL_PIXELFORMAT_INDEX8" F.uint32_t
    let format_rgb332 = F.constant "SDL_PIXELFORMAT_RGB332" F.uint32_t
    let format_rgb444 = F.constant "SDL_PIXELFORMAT_RGB444" F.uint32_t
    let format_rgb555 = F.constant "SDL_PIXELFORMAT_RGB555" F.uint32_t
    let format_bgr555 = F.constant "SDL_PIXELFORMAT_BGR555" F.uint32_t
    let format_argb4444 = F.constant "SDL_PIXELFORMAT_ARGB4444" F.uint32_t
    let format_rgba4444 = F.constant "SDL_PIXELFORMAT_RGBA4444" F.uint32_t
    let format_abgr4444 = F.constant "SDL_PIXELFORMAT_ABGR4444" F.uint32_t
    let format_bgra4444 = F.constant "SDL_PIXELFORMAT_BGRA4444" F.uint32_t
    let format_argb1555 = F.constant "SDL_PIXELFORMAT_ARGB1555" F.uint32_t
    let format_rgba5551 = F.constant "SDL_PIXELFORMAT_RGBA5551" F.uint32_t
    let format_abgr1555 = F.constant "SDL_PIXELFORMAT_ABGR1555" F.uint32_t
    let format_bgra5551 = F.constant "SDL_PIXELFORMAT_BGRA5551" F.uint32_t
    let format_rgb565 = F.constant "SDL_PIXELFORMAT_RGB565" F.uint32_t
    let format_bgr565 = F.constant "SDL_PIXELFORMAT_BGR565" F.uint32_t
    let format_rgb24 = F.constant "SDL_PIXELFORMAT_RGB24" F.uint32_t
    let format_bgr24 = F.constant "SDL_PIXELFORMAT_BGR24" F.uint32_t
    let format_rgb888 = F.constant "SDL_PIXELFORMAT_RGB888" F.uint32_t
    let format_rgbx8888 = F.constant "SDL_PIXELFORMAT_RGBX8888" F.uint32_t
    let format_bgr888 = F.constant "SDL_PIXELFORMAT_BGR888" F.uint32_t
    let format_bgrx8888 = F.constant "SDL_PIXELFORMAT_BGRX8888" F.uint32_t
    let format_argb8888 = F.constant "SDL_PIXELFORMAT_ARGB8888" F.uint32_t
    let format_rgba8888 = F.constant "SDL_PIXELFORMAT_RGBA8888" F.uint32_t
    let format_abgr8888 = F.constant "SDL_PIXELFORMAT_ABGR8888" F.uint32_t
    let format_bgra8888 = F.constant "SDL_PIXELFORMAT_BGRA8888" F.uint32_t
    let format_argb2101010 = F.constant "SDL_PIXELFORMAT_ARGB2101010" F.uint32_t
    let format_yv12 = F.constant "SDL_PIXELFORMAT_YV12" F.uint32_t
    let format_iyuv = F.constant "SDL_PIXELFORMAT_IYUV" F.uint32_t
    let format_yuy2 = F.constant "SDL_PIXELFORMAT_YUY2" F.uint32_t
    let format_uyvy = F.constant "SDL_PIXELFORMAT_UYVY" F.uint32_t
    let format_yvyu = F.constant "SDL_PIXELFORMAT_YVYU" F.uint32_t
  end

  module Flip = struct
    let none = F.constant "SDL_FLIP_NONE" F.int
    let horizontal = F.constant "SDL_FLIP_HORIZONTAL" F.int
    let vertical = F.constant "SDL_FLIP_VERTICAL" F.int
  end

  module Renderer = struct
    let software = F.constant "SDL_RENDERER_SOFTWARE" F.uint32_t
    let accelerated = F.constant "SDL_RENDERER_ACCELERATED" F.uint32_t
    let presentvsync = F.constant "SDL_RENDERER_PRESENTVSYNC" F.uint32_t
    let targettexture = F.constant "SDL_RENDERER_TARGETTEXTURE" F.uint32_t
  end

  module Texture = struct
    let access_static = F.constant "SDL_TEXTUREACCESS_STATIC" F.int
    let access_streaming = F.constant "SDL_TEXTUREACCESS_STREAMING" F.int
    let access_target = F.constant "SDL_TEXTUREACCESS_TARGET" F.int

    let modulate_none = F.constant "SDL_TEXTUREMODULATE_NONE" F.uint32_t
    let modulate_color = F.constant "SDL_TEXTUREMODULATE_COLOR" F.uint32_t
    let modulate_alpha = F.constant "SDL_TEXTUREMODULATE_ALPHA" F.uint32_t
  end

  module Window = struct
    let fullscreen = F.constant "SDL_WINDOW_FULLSCREEN" F.uint32_t
    let fullscreen_desktop = F.constant "SDL_WINDOW_FULLSCREEN_DESKTOP" F.uint32_t
    let opengl = F.constant "SDL_WINDOW_OPENGL" F.uint32_t
    let shown = F.constant "SDL_WINDOW_SHOWN" F.uint32_t
    let hidden = F.constant "SDL_WINDOW_HIDDEN" F.uint32_t
    let borderless = F.constant "SDL_WINDOW_BORDERLESS" F.uint32_t
    let resizable = F.constant "SDL_WINDOW_RESIZABLE" F.uint32_t
    let minimized = F.constant "SDL_WINDOW_MINIMIZED" F.uint32_t
    let maximized = F.constant "SDL_WINDOW_MAXIMIZED" F.uint32_t
    let input_grabbed = F.constant "SDL_WINDOW_INPUT_GRABBED" F.uint32_t
    let input_focus = F.constant "SDL_WINDOW_INPUT_FOCUS" F.uint32_t
    let mouse_focus = F.constant "SDL_WINDOW_MOUSE_FOCUS" F.uint32_t
    let foreign = F.constant "SDL_WINDOW_FOREIGN" F.uint32_t
    let allow_highdpi = F.constant "SDL_WINDOW_ALLOW_HIGHDPI" F.uint32_t
    let mouse_capture = F.constant "SDL_WINDOW_MOUSE_CAPTURE" F.uint32_t
    let always_on_top = F.constant "SDL_WINDOW_ALWAYS_ON_TOP" F.uint32_t
    let skip_taskbar = F.constant "SDL_WINDOW_SKIP_TASKBAR" F.uint32_t
    let utility = F.constant "SDL_WINDOW_UTILITY" F.uint32_t
    let popup_menu = F.constant "SDL_WINDOW_POPUP_MENU" F.uint32_t
    let vulkan = F.constant "SDL_WINDOW_VULKAN" F.uint32_t

    let pos_centered = F.constant "SDL_WINDOWPOS_CENTERED" F.int
    let pos_undefined = F.constant "SDL_WINDOWPOS_UNDEFINED" F.int
  end

  module Gl = struct

    (* contextFlag *)

    let context_debug_flag = F.constant "SDL_GL_CONTEXT_DEBUG_FLAG" F.int
    let context_forward_compatible_flag = F.constant "SDL_GL_CONTEXT_FORWARD_COMPATIBLE_FLAG" F.int
    let context_robust_access_flag = F.constant "SDL_GL_CONTEXT_ROBUST_ACCESS_FLAG" F.int
    let context_reset_isolation_flag = F.constant "SDL_GL_CONTEXT_RESET_ISOLATION_FLAG" F.int
    let context_release_behavior = F.constant "SDL_GL_CONTEXT_RELEASE_BEHAVIOR" F.int

    (* profile *)

    let context_profile_core = F.constant "SDL_GL_CONTEXT_PROFILE_CORE" F.int
    let context_profile_compatibility = F.constant "SDL_GL_CONTEXT_PROFILE_COMPATIBILITY" F.int
    let context_profile_es = F.constant "SDL_GL_CONTEXT_PROFILE_ES" F.int

    (* attr *)

    let red_size = F.constant "SDL_GL_RED_SIZE" F.int
    let green_size = F.constant "SDL_GL_GREEN_SIZE" F.int
    let blue_size = F.constant "SDL_GL_BLUE_SIZE" F.int
    let alpha_size = F.constant "SDL_GL_ALPHA_SIZE" F.int
    let buffer_size = F.constant "SDL_GL_BUFFER_SIZE" F.int
    let doublebuffer = F.constant "SDL_GL_DOUBLEBUFFER" F.int
    let depth_size = F.constant "SDL_GL_DEPTH_SIZE" F.int
    let stencil_size = F.constant "SDL_GL_STENCIL_SIZE" F.int
    let accum_red_size = F.constant "SDL_GL_ACCUM_RED_SIZE" F.int
    let accum_green_size = F.constant "SDL_GL_ACCUM_GREEN_SIZE" F.int
    let accum_blue_size = F.constant "SDL_GL_ACCUM_BLUE_SIZE" F.int
    let accum_alpha_size = F.constant "SDL_GL_ACCUM_ALPHA_SIZE" F.int
    let stereo = F.constant "SDL_GL_STEREO" F.int
    let multisamplebuffers = F.constant "SDL_GL_MULTISAMPLEBUFFERS" F.int
    let multisamplesamples = F.constant "SDL_GL_MULTISAMPLESAMPLES" F.int
    let accelerated_visual = F.constant "SDL_GL_ACCELERATED_VISUAL" F.int
    let context_major_version = F.constant "SDL_GL_CONTEXT_MAJOR_VERSION" F.int
    let context_minor_version = F.constant "SDL_GL_CONTEXT_MINOR_VERSION" F.int
    let context_egl = F.constant "SDL_GL_CONTEXT_EGL" F.int
    let context_flags = F.constant "SDL_GL_CONTEXT_FLAGS" F.int
    let context_profile_mask = F.constant "SDL_GL_CONTEXT_PROFILE_MASK" F.int
    let share_with_current_context = F.constant "SDL_GL_SHARE_WITH_CURRENT_CONTEXT" F.int
    let framebuffer_srgb_capable = F.constant "SDL_GL_FRAMEBUFFER_SRGB_CAPABLE" F.int
  end

  module Message_box = struct
    let error = F.constant "SDL_MESSAGEBOX_ERROR" F.uint32_t
    let warning = F.constant "SDL_MESSAGEBOX_WARNING" F.uint32_t
    let information = F.constant "SDL_MESSAGEBOX_INFORMATION" F.uint32_t

    let button_returnkey_default = F.constant "SDL_MESSAGEBOX_BUTTON_RETURNKEY_DEFAULT" F.uint32_t
    let button_escapekey_default = F.constant "SDL_MESSAGEBOX_BUTTON_ESCAPEKEY_DEFAULT" F.uint32_t

    let color_background = F.constant "SDL_MESSAGEBOX_COLOR_BACKGROUND" F.int
    let color_text = F.constant "SDL_MESSAGEBOX_COLOR_TEXT" F.int
    let color_button_border = F.constant "SDL_MESSAGEBOX_COLOR_BUTTON_BORDER" F.int
    let color_button_background = F.constant "SDL_MESSAGEBOX_COLOR_BUTTON_BACKGROUND" F.int
    let color_button_selected = F.constant "SDL_MESSAGEBOX_COLOR_BUTTON_SELECTED" F.int
    let color_button_max = F.constant "SDL_MESSAGEBOX_COLOR_MAX" F.int
  end

  module Scancode = struct
    let unknown = F.constant "SDL_SCANCODE_UNKNOWN" F.int
    let a = F.constant "SDL_SCANCODE_A" F.int
    let b = F.constant "SDL_SCANCODE_B" F.int
    let c = F.constant "SDL_SCANCODE_C" F.int
    let d = F.constant "SDL_SCANCODE_D" F.int
    let e = F.constant "SDL_SCANCODE_E" F.int
    let f = F.constant "SDL_SCANCODE_F" F.int
    let g = F.constant "SDL_SCANCODE_G" F.int
    let h = F.constant "SDL_SCANCODE_H" F.int
    let i = F.constant "SDL_SCANCODE_I" F.int
    let j = F.constant "SDL_SCANCODE_J" F.int
    let k = F.constant "SDL_SCANCODE_K" F.int
    let l = F.constant "SDL_SCANCODE_L" F.int
    let m = F.constant "SDL_SCANCODE_M" F.int
    let n = F.constant "SDL_SCANCODE_N" F.int
    let o = F.constant "SDL_SCANCODE_O" F.int
    let p = F.constant "SDL_SCANCODE_P" F.int
    let q = F.constant "SDL_SCANCODE_Q" F.int
    let r = F.constant "SDL_SCANCODE_R" F.int
    let s = F.constant "SDL_SCANCODE_S" F.int
    let t = F.constant "SDL_SCANCODE_T" F.int
    let u = F.constant "SDL_SCANCODE_U" F.int
    let v = F.constant "SDL_SCANCODE_V" F.int
    let w = F.constant "SDL_SCANCODE_W" F.int
    let x = F.constant "SDL_SCANCODE_X" F.int
    let y = F.constant "SDL_SCANCODE_Y" F.int
    let z = F.constant "SDL_SCANCODE_Z" F.int
    let k1 = F.constant "SDL_SCANCODE_1" F.int
    let k2 = F.constant "SDL_SCANCODE_2" F.int
    let k3 = F.constant "SDL_SCANCODE_3" F.int
    let k4 = F.constant "SDL_SCANCODE_4" F.int
    let k5 = F.constant "SDL_SCANCODE_5" F.int
    let k6 = F.constant "SDL_SCANCODE_6" F.int
    let k7 = F.constant "SDL_SCANCODE_7" F.int
    let k8 = F.constant "SDL_SCANCODE_8" F.int
    let k9 = F.constant "SDL_SCANCODE_9" F.int
    let k0 = F.constant "SDL_SCANCODE_0" F.int
    let return = F.constant "SDL_SCANCODE_RETURN" F.int
    let escape = F.constant "SDL_SCANCODE_ESCAPE" F.int
    let backspace = F.constant "SDL_SCANCODE_BACKSPACE" F.int
    let tab = F.constant "SDL_SCANCODE_TAB" F.int
    let space = F.constant "SDL_SCANCODE_SPACE" F.int
    let minus = F.constant "SDL_SCANCODE_MINUS" F.int
    let equals = F.constant "SDL_SCANCODE_EQUALS" F.int
    let leftbracket = F.constant "SDL_SCANCODE_LEFTBRACKET" F.int
    let rightbracket = F.constant "SDL_SCANCODE_RIGHTBRACKET" F.int
    let backslash = F.constant "SDL_SCANCODE_BACKSLASH" F.int
    let nonushash = F.constant "SDL_SCANCODE_NONUSHASH" F.int
    let semicolon = F.constant "SDL_SCANCODE_SEMICOLON" F.int
    let apostrophe = F.constant "SDL_SCANCODE_APOSTROPHE" F.int
    let grave = F.constant "SDL_SCANCODE_GRAVE" F.int
    let comma = F.constant "SDL_SCANCODE_COMMA" F.int
    let period = F.constant "SDL_SCANCODE_PERIOD" F.int
    let slash = F.constant "SDL_SCANCODE_SLASH" F.int
    let capslock = F.constant "SDL_SCANCODE_CAPSLOCK" F.int
    let f1 = F.constant "SDL_SCANCODE_F1" F.int
    let f2 = F.constant "SDL_SCANCODE_F2" F.int
    let f3 = F.constant "SDL_SCANCODE_F3" F.int
    let f4 = F.constant "SDL_SCANCODE_F4" F.int
    let f5 = F.constant "SDL_SCANCODE_F5" F.int
    let f6 = F.constant "SDL_SCANCODE_F6" F.int
    let f7 = F.constant "SDL_SCANCODE_F7" F.int
    let f8 = F.constant "SDL_SCANCODE_F8" F.int
    let f9 = F.constant "SDL_SCANCODE_F9" F.int
    let f10 = F.constant "SDL_SCANCODE_F10" F.int
    let f11 = F.constant "SDL_SCANCODE_F11" F.int
    let f12 = F.constant "SDL_SCANCODE_F12" F.int
    let printscreen = F.constant "SDL_SCANCODE_PRINTSCREEN" F.int
    let scrolllock = F.constant "SDL_SCANCODE_SCROLLLOCK" F.int
    let pause = F.constant "SDL_SCANCODE_PAUSE" F.int
    let insert = F.constant "SDL_SCANCODE_INSERT" F.int
    let home = F.constant "SDL_SCANCODE_HOME" F.int
    let pageup = F.constant "SDL_SCANCODE_PAGEUP" F.int
    let delete = F.constant "SDL_SCANCODE_DELETE" F.int
    let kend = F.constant "SDL_SCANCODE_END" F.int
    let pagedown = F.constant "SDL_SCANCODE_PAGEDOWN" F.int
    let right = F.constant "SDL_SCANCODE_RIGHT" F.int
    let left = F.constant "SDL_SCANCODE_LEFT" F.int
    let down = F.constant "SDL_SCANCODE_DOWN" F.int
    let up = F.constant "SDL_SCANCODE_UP" F.int
    let numlockclear = F.constant "SDL_SCANCODE_NUMLOCKCLEAR" F.int
    let kp_divide = F.constant "SDL_SCANCODE_KP_DIVIDE" F.int
    let kp_multiply = F.constant "SDL_SCANCODE_KP_MULTIPLY" F.int
    let kp_minus = F.constant "SDL_SCANCODE_KP_MINUS" F.int
    let kp_plus = F.constant "SDL_SCANCODE_KP_PLUS" F.int
    let kp_enter = F.constant "SDL_SCANCODE_KP_ENTER" F.int
    let kp_1 = F.constant "SDL_SCANCODE_KP_1" F.int
    let kp_2 = F.constant "SDL_SCANCODE_KP_2" F.int
    let kp_3 = F.constant "SDL_SCANCODE_KP_3" F.int
    let kp_4 = F.constant "SDL_SCANCODE_KP_4" F.int
    let kp_5 = F.constant "SDL_SCANCODE_KP_5" F.int
    let kp_6 = F.constant "SDL_SCANCODE_KP_6" F.int
    let kp_7 = F.constant "SDL_SCANCODE_KP_7" F.int
    let kp_8 = F.constant "SDL_SCANCODE_KP_8" F.int
    let kp_9 = F.constant "SDL_SCANCODE_KP_9" F.int
    let kp_0 = F.constant "SDL_SCANCODE_KP_0" F.int
    let kp_period = F.constant "SDL_SCANCODE_KP_PERIOD" F.int
    let nonusbackslash = F.constant "SDL_SCANCODE_NONUSBACKSLASH" F.int
    let application = F.constant "SDL_SCANCODE_APPLICATION" F.int
    let kp_equals = F.constant "SDL_SCANCODE_KP_EQUALS" F.int
    let f13 = F.constant "SDL_SCANCODE_F13" F.int
    let f14 = F.constant "SDL_SCANCODE_F14" F.int
    let f15 = F.constant "SDL_SCANCODE_F15" F.int
    let f16 = F.constant "SDL_SCANCODE_F16" F.int
    let f17 = F.constant "SDL_SCANCODE_F17" F.int
    let f18 = F.constant "SDL_SCANCODE_F18" F.int
    let f19 = F.constant "SDL_SCANCODE_F19" F.int
    let f20 = F.constant "SDL_SCANCODE_F20" F.int
    let f21 = F.constant "SDL_SCANCODE_F21" F.int
    let f22 = F.constant "SDL_SCANCODE_F22" F.int
    let f23 = F.constant "SDL_SCANCODE_F23" F.int
    let f24 = F.constant "SDL_SCANCODE_F24" F.int
    let execute = F.constant "SDL_SCANCODE_EXECUTE" F.int
    let help = F.constant "SDL_SCANCODE_HELP" F.int
    let menu = F.constant "SDL_SCANCODE_MENU" F.int
    let select = F.constant "SDL_SCANCODE_SELECT" F.int
    let stop = F.constant "SDL_SCANCODE_STOP" F.int
    let again = F.constant "SDL_SCANCODE_AGAIN" F.int
    let undo = F.constant "SDL_SCANCODE_UNDO" F.int
    let cut = F.constant "SDL_SCANCODE_CUT" F.int
    let copy = F.constant "SDL_SCANCODE_COPY" F.int
    let paste = F.constant "SDL_SCANCODE_PASTE" F.int
    let find = F.constant "SDL_SCANCODE_FIND" F.int
    let mute = F.constant "SDL_SCANCODE_MUTE" F.int
    let volumeup = F.constant "SDL_SCANCODE_VOLUMEUP" F.int
    let volumedown = F.constant "SDL_SCANCODE_VOLUMEDOWN" F.int
    let kp_comma = F.constant "SDL_SCANCODE_KP_COMMA" F.int
    let kp_equalsas400 = F.constant "SDL_SCANCODE_KP_EQUALSAS400" F.int
    let international1 = F.constant "SDL_SCANCODE_INTERNATIONAL1" F.int
    let international2 = F.constant "SDL_SCANCODE_INTERNATIONAL2" F.int
    let international3 = F.constant "SDL_SCANCODE_INTERNATIONAL3" F.int
    let international4 = F.constant "SDL_SCANCODE_INTERNATIONAL4" F.int
    let international5 = F.constant "SDL_SCANCODE_INTERNATIONAL5" F.int
    let international6 = F.constant "SDL_SCANCODE_INTERNATIONAL6" F.int
    let international7 = F.constant "SDL_SCANCODE_INTERNATIONAL7" F.int
    let international8 = F.constant "SDL_SCANCODE_INTERNATIONAL8" F.int
    let international9 = F.constant "SDL_SCANCODE_INTERNATIONAL9" F.int
    let lang1 = F.constant "SDL_SCANCODE_LANG1" F.int
    let lang2 = F.constant "SDL_SCANCODE_LANG2" F.int
    let lang3 = F.constant "SDL_SCANCODE_LANG3" F.int
    let lang4 = F.constant "SDL_SCANCODE_LANG4" F.int
    let lang5 = F.constant "SDL_SCANCODE_LANG5" F.int
    let lang6 = F.constant "SDL_SCANCODE_LANG6" F.int
    let lang7 = F.constant "SDL_SCANCODE_LANG7" F.int
    let lang8 = F.constant "SDL_SCANCODE_LANG8" F.int
    let lang9 = F.constant "SDL_SCANCODE_LANG9" F.int
    let alterase = F.constant "SDL_SCANCODE_ALTERASE" F.int
    let sysreq = F.constant "SDL_SCANCODE_SYSREQ" F.int
    let cancel = F.constant "SDL_SCANCODE_CANCEL" F.int
    let clear = F.constant "SDL_SCANCODE_CLEAR" F.int
    let prior = F.constant "SDL_SCANCODE_PRIOR" F.int
    let return2 = F.constant "SDL_SCANCODE_RETURN2" F.int
    let separator = F.constant "SDL_SCANCODE_SEPARATOR" F.int
    let out = F.constant "SDL_SCANCODE_OUT" F.int
    let oper = F.constant "SDL_SCANCODE_OPER" F.int
    let clearagain = F.constant "SDL_SCANCODE_CLEARAGAIN" F.int
    let crsel = F.constant "SDL_SCANCODE_CRSEL" F.int
    let exsel = F.constant "SDL_SCANCODE_EXSEL" F.int
    let kp_00 = F.constant "SDL_SCANCODE_KP_00" F.int
    let kp_000 = F.constant "SDL_SCANCODE_KP_000" F.int
    let thousandsseparator = F.constant "SDL_SCANCODE_THOUSANDSSEPARATOR" F.int
    let decimalseparator = F.constant "SDL_SCANCODE_DECIMALSEPARATOR" F.int
    let currencyunit = F.constant "SDL_SCANCODE_CURRENCYUNIT" F.int
    let currencysubunit = F.constant "SDL_SCANCODE_CURRENCYSUBUNIT" F.int
    let kp_leftparen = F.constant "SDL_SCANCODE_KP_LEFTPAREN" F.int
    let kp_rightparen = F.constant "SDL_SCANCODE_KP_RIGHTPAREN" F.int
    let kp_leftbrace = F.constant "SDL_SCANCODE_KP_LEFTBRACE" F.int
    let kp_rightbrace = F.constant "SDL_SCANCODE_KP_RIGHTBRACE" F.int
    let kp_tab = F.constant "SDL_SCANCODE_KP_TAB" F.int
    let kp_backspace = F.constant "SDL_SCANCODE_KP_BACKSPACE" F.int
    let kp_a = F.constant "SDL_SCANCODE_KP_A" F.int
    let kp_b = F.constant "SDL_SCANCODE_KP_B" F.int
    let kp_c = F.constant "SDL_SCANCODE_KP_C" F.int
    let kp_d = F.constant "SDL_SCANCODE_KP_D" F.int
    let kp_e = F.constant "SDL_SCANCODE_KP_E" F.int
    let kp_f = F.constant "SDL_SCANCODE_KP_F" F.int
    let kp_xor = F.constant "SDL_SCANCODE_KP_XOR" F.int
    let kp_power = F.constant "SDL_SCANCODE_KP_POWER" F.int
    let kp_percent = F.constant "SDL_SCANCODE_KP_PERCENT" F.int
    let kp_less = F.constant "SDL_SCANCODE_KP_LESS" F.int
    let kp_greater = F.constant "SDL_SCANCODE_KP_GREATER" F.int
    let kp_ampersand = F.constant "SDL_SCANCODE_KP_AMPERSAND" F.int
    let kp_dblampersand = F.constant "SDL_SCANCODE_KP_DBLAMPERSAND" F.int
    let kp_verticalbar = F.constant "SDL_SCANCODE_KP_VERTICALBAR" F.int
    let kp_dblverticalbar = F.constant "SDL_SCANCODE_KP_DBLVERTICALBAR" F.int
    let kp_colon = F.constant "SDL_SCANCODE_KP_COLON" F.int
    let kp_hash = F.constant "SDL_SCANCODE_KP_HASH" F.int
    let kp_space = F.constant "SDL_SCANCODE_KP_SPACE" F.int
    let kp_at = F.constant "SDL_SCANCODE_KP_AT" F.int
    let kp_exclam = F.constant "SDL_SCANCODE_KP_EXCLAM" F.int
    let kp_memstore = F.constant "SDL_SCANCODE_KP_MEMSTORE" F.int
    let kp_memrecall = F.constant "SDL_SCANCODE_KP_MEMRECALL" F.int
    let kp_memclear = F.constant "SDL_SCANCODE_KP_MEMCLEAR" F.int
    let kp_memadd = F.constant "SDL_SCANCODE_KP_MEMADD" F.int
    let kp_memsubtract = F.constant "SDL_SCANCODE_KP_MEMSUBTRACT" F.int
    let kp_memmultiply = F.constant "SDL_SCANCODE_KP_MEMMULTIPLY" F.int
    let kp_memdivide = F.constant "SDL_SCANCODE_KP_MEMDIVIDE" F.int
    let kp_plusminus = F.constant "SDL_SCANCODE_KP_PLUSMINUS" F.int
    let kp_clear = F.constant "SDL_SCANCODE_KP_CLEAR" F.int
    let kp_clearentry = F.constant "SDL_SCANCODE_KP_CLEARENTRY" F.int
    let kp_binary = F.constant "SDL_SCANCODE_KP_BINARY" F.int
    let kp_octal = F.constant "SDL_SCANCODE_KP_OCTAL" F.int
    let kp_decimal = F.constant "SDL_SCANCODE_KP_DECIMAL" F.int
    let kp_hexadecimal = F.constant "SDL_SCANCODE_KP_HEXADECIMAL" F.int
    let lctrl = F.constant "SDL_SCANCODE_LCTRL" F.int
    let lshift = F.constant "SDL_SCANCODE_LSHIFT" F.int
    let lalt = F.constant "SDL_SCANCODE_LALT" F.int
    let lgui = F.constant "SDL_SCANCODE_LGUI" F.int
    let rctrl = F.constant "SDL_SCANCODE_RCTRL" F.int
    let rshift = F.constant "SDL_SCANCODE_RSHIFT" F.int
    let ralt = F.constant "SDL_SCANCODE_RALT" F.int
    let rgui = F.constant "SDL_SCANCODE_RGUI" F.int
    let mode = F.constant "SDL_SCANCODE_MODE" F.int
    let audionext = F.constant "SDL_SCANCODE_AUDIONEXT" F.int
    let audioprev = F.constant "SDL_SCANCODE_AUDIOPREV" F.int
    let audiostop = F.constant "SDL_SCANCODE_AUDIOSTOP" F.int
    let audioplay = F.constant "SDL_SCANCODE_AUDIOPLAY" F.int
    let audiomute = F.constant "SDL_SCANCODE_AUDIOMUTE" F.int
    let mediaselect = F.constant "SDL_SCANCODE_MEDIASELECT" F.int
    let www = F.constant "SDL_SCANCODE_WWW" F.int
    let mail = F.constant "SDL_SCANCODE_MAIL" F.int
    let calculator = F.constant "SDL_SCANCODE_CALCULATOR" F.int
    let computer = F.constant "SDL_SCANCODE_COMPUTER" F.int
    let ac_search = F.constant "SDL_SCANCODE_AC_SEARCH" F.int
    let ac_home = F.constant "SDL_SCANCODE_AC_HOME" F.int
    let ac_back = F.constant "SDL_SCANCODE_AC_BACK" F.int
    let ac_forward = F.constant "SDL_SCANCODE_AC_FORWARD" F.int
    let ac_stop = F.constant "SDL_SCANCODE_AC_STOP" F.int
    let ac_refresh = F.constant "SDL_SCANCODE_AC_REFRESH" F.int
    let ac_bookmarks = F.constant "SDL_SCANCODE_AC_BOOKMARKS" F.int
    let brightnessdown = F.constant "SDL_SCANCODE_BRIGHTNESSDOWN" F.int
    let brightnessup = F.constant "SDL_SCANCODE_BRIGHTNESSUP" F.int
    let displayswitch = F.constant "SDL_SCANCODE_DISPLAYSWITCH" F.int
    let kbdillumtoggle = F.constant "SDL_SCANCODE_KBDILLUMTOGGLE" F.int
    let kbdillumdown = F.constant "SDL_SCANCODE_KBDILLUMDOWN" F.int
    let kbdillumup = F.constant "SDL_SCANCODE_KBDILLUMUP" F.int
    let eject = F.constant "SDL_SCANCODE_EJECT" F.int
    let sleep = F.constant "SDL_SCANCODE_SLEEP" F.int
    let app1 = F.constant "SDL_SCANCODE_APP1" F.int
    let app2 = F.constant "SDL_SCANCODE_APP2" F.int
    let num_scancodes = F.constant "SDL_NUM_SCANCODES" F.int
  end

  module K = struct
    let scancode_mask = F.constant "SDLK_SCANCODE_MASK" F.int
    let unknown = F.constant "SDLK_UNKNOWN" F.int
    let return = F.constant "SDLK_RETURN" F.int
    let escape = F.constant "SDLK_ESCAPE" F.int
    let backspace = F.constant "SDLK_BACKSPACE" F.int
    let tab = F.constant "SDLK_TAB" F.int
    let space = F.constant "SDLK_SPACE" F.int
    let exclaim = F.constant "SDLK_EXCLAIM" F.int
    let quotedbl = F.constant "SDLK_QUOTEDBL" F.int
    let hash = F.constant "SDLK_HASH" F.int
    let percent = F.constant "SDLK_PERCENT" F.int
    let dollar = F.constant "SDLK_DOLLAR" F.int
    let ampersand = F.constant "SDLK_AMPERSAND" F.int
    let quote = F.constant "SDLK_QUOTE" F.int
    let leftparen = F.constant "SDLK_LEFTPAREN" F.int
    let rightparen = F.constant "SDLK_RIGHTPAREN" F.int
    let asterisk = F.constant "SDLK_ASTERISK" F.int
    let plus = F.constant "SDLK_PLUS" F.int
    let comma = F.constant "SDLK_COMMA" F.int
    let minus = F.constant "SDLK_MINUS" F.int
    let period = F.constant "SDLK_PERIOD" F.int
    let slash = F.constant "SDLK_SLASH" F.int
    let k0 = F.constant "SDLK_0" F.int
    let k1 = F.constant "SDLK_1" F.int
    let k2 = F.constant "SDLK_2" F.int
    let k3 = F.constant "SDLK_3" F.int
    let k4 = F.constant "SDLK_4" F.int
    let k5 = F.constant "SDLK_5" F.int
    let k6 = F.constant "SDLK_6" F.int
    let k7 = F.constant "SDLK_7" F.int
    let k8 = F.constant "SDLK_8" F.int
    let k9 = F.constant "SDLK_9" F.int
    let colon = F.constant "SDLK_COLON" F.int
    let semicolon = F.constant "SDLK_SEMICOLON" F.int
    let less = F.constant "SDLK_LESS" F.int
    let equals = F.constant "SDLK_EQUALS" F.int
    let greater = F.constant "SDLK_GREATER" F.int
    let question = F.constant "SDLK_QUESTION" F.int
    let at = F.constant "SDLK_AT" F.int
    let leftbracket = F.constant "SDLK_LEFTBRACKET" F.int
    let backslash = F.constant "SDLK_BACKSLASH" F.int
    let rightbracket = F.constant "SDLK_RIGHTBRACKET" F.int
    let caret = F.constant "SDLK_CARET" F.int
    let underscore = F.constant "SDLK_UNDERSCORE" F.int
    let backquote = F.constant "SDLK_BACKQUOTE" F.int
    let a = F.constant "SDLK_a" F.int
    let b = F.constant "SDLK_b" F.int
    let c = F.constant "SDLK_c" F.int
    let d = F.constant "SDLK_d" F.int
    let e = F.constant "SDLK_e" F.int
    let f = F.constant "SDLK_f" F.int
    let g = F.constant "SDLK_g" F.int
    let h = F.constant "SDLK_h" F.int
    let i = F.constant "SDLK_i" F.int
    let j = F.constant "SDLK_j" F.int
    let k = F.constant "SDLK_k" F.int
    let l = F.constant "SDLK_l" F.int
    let m = F.constant "SDLK_m" F.int
    let n = F.constant "SDLK_n" F.int
    let o = F.constant "SDLK_o" F.int
    let p = F.constant "SDLK_p" F.int
    let q = F.constant "SDLK_q" F.int
    let r = F.constant "SDLK_r" F.int
    let s = F.constant "SDLK_s" F.int
    let t = F.constant "SDLK_t" F.int
    let u = F.constant "SDLK_u" F.int
    let v = F.constant "SDLK_v" F.int
    let w = F.constant "SDLK_w" F.int
    let x = F.constant "SDLK_x" F.int
    let y = F.constant "SDLK_y" F.int
    let z = F.constant "SDLK_z" F.int
    let capslock = F.constant "SDLK_CAPSLOCK" F.int
    let f1 = F.constant "SDLK_F1" F.int
    let f2 = F.constant "SDLK_F2" F.int
    let f3 = F.constant "SDLK_F3" F.int
    let f4 = F.constant "SDLK_F4" F.int
    let f5 = F.constant "SDLK_F5" F.int
    let f6 = F.constant "SDLK_F6" F.int
    let f7 = F.constant "SDLK_F7" F.int
    let f8 = F.constant "SDLK_F8" F.int
    let f9 = F.constant "SDLK_F9" F.int
    let f10 = F.constant "SDLK_F10" F.int
    let f11 = F.constant "SDLK_F11" F.int
    let f12 = F.constant "SDLK_F12" F.int
    let printscreen = F.constant "SDLK_PRINTSCREEN" F.int
    let scrolllock = F.constant "SDLK_SCROLLLOCK" F.int
    let pause = F.constant "SDLK_PAUSE" F.int
    let insert = F.constant "SDLK_INSERT" F.int
    let home = F.constant "SDLK_HOME" F.int
    let pageup = F.constant "SDLK_PAGEUP" F.int
    let delete = F.constant "SDLK_DELETE" F.int
    let kend = F.constant "SDLK_END" F.int
    let pagedown = F.constant "SDLK_PAGEDOWN" F.int
    let right = F.constant "SDLK_RIGHT" F.int
    let left = F.constant "SDLK_LEFT" F.int
    let down = F.constant "SDLK_DOWN" F.int
    let up = F.constant "SDLK_UP" F.int
    let numlockclear = F.constant "SDLK_NUMLOCKCLEAR" F.int
    let kp_divide = F.constant "SDLK_KP_DIVIDE" F.int
    let kp_multiply = F.constant "SDLK_KP_MULTIPLY" F.int
    let kp_minus = F.constant "SDLK_KP_MINUS" F.int
    let kp_plus = F.constant "SDLK_KP_PLUS" F.int
    let kp_enter = F.constant "SDLK_KP_ENTER" F.int
    let kp_1 = F.constant "SDLK_KP_1" F.int
    let kp_2 = F.constant "SDLK_KP_2" F.int
    let kp_3 = F.constant "SDLK_KP_3" F.int
    let kp_4 = F.constant "SDLK_KP_4" F.int
    let kp_5 = F.constant "SDLK_KP_5" F.int
    let kp_6 = F.constant "SDLK_KP_6" F.int
    let kp_7 = F.constant "SDLK_KP_7" F.int
    let kp_8 = F.constant "SDLK_KP_8" F.int
    let kp_9 = F.constant "SDLK_KP_9" F.int
    let kp_0 = F.constant "SDLK_KP_0" F.int
    let kp_period = F.constant "SDLK_KP_PERIOD" F.int
    let application = F.constant "SDLK_APPLICATION" F.int
    let power = F.constant "SDLK_POWER" F.int
    let kp_equals = F.constant "SDLK_KP_EQUALS" F.int
    let f13 = F.constant "SDLK_F13" F.int
    let f14 = F.constant "SDLK_F14" F.int
    let f15 = F.constant "SDLK_F15" F.int
    let f16 = F.constant "SDLK_F16" F.int
    let f17 = F.constant "SDLK_F17" F.int
    let f18 = F.constant "SDLK_F18" F.int
    let f19 = F.constant "SDLK_F19" F.int
    let f20 = F.constant "SDLK_F20" F.int
    let f21 = F.constant "SDLK_F21" F.int
    let f22 = F.constant "SDLK_F22" F.int
    let f23 = F.constant "SDLK_F23" F.int
    let f24 = F.constant "SDLK_F24" F.int
    let execute = F.constant "SDLK_EXECUTE" F.int
    let help = F.constant "SDLK_HELP" F.int
    let menu = F.constant "SDLK_MENU" F.int
    let select = F.constant "SDLK_SELECT" F.int
    let stop = F.constant "SDLK_STOP" F.int
    let again = F.constant "SDLK_AGAIN" F.int
    let undo = F.constant "SDLK_UNDO" F.int
    let cut = F.constant "SDLK_CUT" F.int
    let copy = F.constant "SDLK_COPY" F.int
    let paste = F.constant "SDLK_PASTE" F.int
    let find = F.constant "SDLK_FIND" F.int
    let mute = F.constant "SDLK_MUTE" F.int
    let volumeup = F.constant "SDLK_VOLUMEUP" F.int
    let volumedown = F.constant "SDLK_VOLUMEDOWN" F.int
    let kp_comma = F.constant "SDLK_KP_COMMA" F.int
    let kp_equalsas400 = F.constant "SDLK_KP_EQUALSAS400" F.int
    let alterase = F.constant "SDLK_ALTERASE" F.int
    let sysreq = F.constant "SDLK_SYSREQ" F.int
    let cancel = F.constant "SDLK_CANCEL" F.int
    let clear = F.constant "SDLK_CLEAR" F.int
    let prior = F.constant "SDLK_PRIOR" F.int
    let return2 = F.constant "SDLK_RETURN2" F.int
    let separator = F.constant "SDLK_SEPARATOR" F.int
    let out = F.constant "SDLK_OUT" F.int
    let oper = F.constant "SDLK_OPER" F.int
    let clearagain = F.constant "SDLK_CLEARAGAIN" F.int
    let crsel = F.constant "SDLK_CRSEL" F.int
    let exsel = F.constant "SDLK_EXSEL" F.int
    let kp_00 = F.constant "SDLK_KP_00" F.int
    let kp_000 = F.constant "SDLK_KP_000" F.int
    let thousandsseparator = F.constant "SDLK_THOUSANDSSEPARATOR" F.int
    let decimalseparator = F.constant "SDLK_DECIMALSEPARATOR" F.int
    let currencyunit = F.constant "SDLK_CURRENCYUNIT" F.int
    let currencysubunit = F.constant "SDLK_CURRENCYSUBUNIT" F.int
    let kp_leftparen = F.constant "SDLK_KP_LEFTPAREN" F.int
    let kp_rightparen = F.constant "SDLK_KP_RIGHTPAREN" F.int
    let kp_leftbrace = F.constant "SDLK_KP_LEFTBRACE" F.int
    let kp_rightbrace = F.constant "SDLK_KP_RIGHTBRACE" F.int
    let kp_tab = F.constant "SDLK_KP_TAB" F.int
    let kp_backspace = F.constant "SDLK_KP_BACKSPACE" F.int
    let kp_a = F.constant "SDLK_KP_A" F.int
    let kp_b = F.constant "SDLK_KP_B" F.int
    let kp_c = F.constant "SDLK_KP_C" F.int
    let kp_d = F.constant "SDLK_KP_D" F.int
    let kp_e = F.constant "SDLK_KP_E" F.int
    let kp_f = F.constant "SDLK_KP_F" F.int
    let kp_xor = F.constant "SDLK_KP_XOR" F.int
    let kp_power = F.constant "SDLK_KP_POWER" F.int
    let kp_percent = F.constant "SDLK_KP_PERCENT" F.int
    let kp_less = F.constant "SDLK_KP_LESS" F.int
    let kp_greater = F.constant "SDLK_KP_GREATER" F.int
    let kp_ampersand = F.constant "SDLK_KP_AMPERSAND" F.int
    let kp_dblampersand = F.constant "SDLK_KP_DBLAMPERSAND" F.int
    let kp_verticalbar = F.constant "SDLK_KP_VERTICALBAR" F.int
    let kp_dblverticalbar = F.constant "SDLK_KP_DBLVERTICALBAR" F.int
    let kp_colon = F.constant "SDLK_KP_COLON" F.int
    let kp_hash = F.constant "SDLK_KP_HASH" F.int
    let kp_space = F.constant "SDLK_KP_SPACE" F.int
    let kp_at = F.constant "SDLK_KP_AT" F.int
    let kp_exclam = F.constant "SDLK_KP_EXCLAM" F.int
    let kp_memstore = F.constant "SDLK_KP_MEMSTORE" F.int
    let kp_memrecall = F.constant "SDLK_KP_MEMRECALL" F.int
    let kp_memclear = F.constant "SDLK_KP_MEMCLEAR" F.int
    let kp_memadd = F.constant "SDLK_KP_MEMADD" F.int
    let kp_memsubtract = F.constant "SDLK_KP_MEMSUBTRACT" F.int
    let kp_memmultiply = F.constant "SDLK_KP_MEMMULTIPLY" F.int
    let kp_memdivide = F.constant "SDLK_KP_MEMDIVIDE" F.int
    let kp_plusminus = F.constant "SDLK_KP_PLUSMINUS" F.int
    let kp_clear = F.constant "SDLK_KP_CLEAR" F.int
    let kp_clearentry = F.constant "SDLK_KP_CLEARENTRY" F.int
    let kp_binary = F.constant "SDLK_KP_BINARY" F.int
    let kp_octal = F.constant "SDLK_KP_OCTAL" F.int
    let kp_decimal = F.constant "SDLK_KP_DECIMAL" F.int
    let kp_hexadecimal = F.constant "SDLK_KP_HEXADECIMAL" F.int
    let lctrl = F.constant "SDLK_LCTRL" F.int
    let lshift = F.constant "SDLK_LSHIFT" F.int
    let lalt = F.constant "SDLK_LALT" F.int
    let lgui = F.constant "SDLK_LGUI" F.int
    let rctrl = F.constant "SDLK_RCTRL" F.int
    let rshift = F.constant "SDLK_RSHIFT" F.int
    let ralt = F.constant "SDLK_RALT" F.int
    let rgui = F.constant "SDLK_RGUI" F.int
    let mode = F.constant "SDLK_MODE" F.int
    let audionext = F.constant "SDLK_AUDIONEXT" F.int
    let audioprev = F.constant "SDLK_AUDIOPREV" F.int
    let audiostop = F.constant "SDLK_AUDIOSTOP" F.int
    let audioplay = F.constant "SDLK_AUDIOPLAY" F.int
    let audiomute = F.constant "SDLK_AUDIOMUTE" F.int
    let mediaselect = F.constant "SDLK_MEDIASELECT" F.int
    let www = F.constant "SDLK_WWW" F.int
    let mail = F.constant "SDLK_MAIL" F.int
    let calculator = F.constant "SDLK_CALCULATOR" F.int
    let computer = F.constant "SDLK_COMPUTER" F.int
    let ac_search = F.constant "SDLK_AC_SEARCH" F.int
    let ac_home = F.constant "SDLK_AC_HOME" F.int
    let ac_back = F.constant "SDLK_AC_BACK" F.int
    let ac_forward = F.constant "SDLK_AC_FORWARD" F.int
    let ac_stop = F.constant "SDLK_AC_STOP" F.int
    let ac_refresh = F.constant "SDLK_AC_REFRESH" F.int
    let ac_bookmarks = F.constant "SDLK_AC_BOOKMARKS" F.int
    let brightnessdown = F.constant "SDLK_BRIGHTNESSDOWN" F.int
    let brightnessup = F.constant "SDLK_BRIGHTNESSUP" F.int
    let displayswitch = F.constant "SDLK_DISPLAYSWITCH" F.int
    let kbdillumtoggle = F.constant "SDLK_KBDILLUMTOGGLE" F.int
    let kbdillumdown = F.constant "SDLK_KBDILLUMDOWN" F.int
    let kbdillumup = F.constant "SDLK_KBDILLUMUP" F.int
    let eject = F.constant "SDLK_EJECT" F.int
    let sleep = F.constant "SDLK_SLEEP" F.int
  end

  module Kmod = struct
    let none = F.constant "KMOD_NONE" F.int
    let lshift = F.constant "KMOD_LSHIFT" F.int
    let rshift = F.constant "KMOD_RSHIFT" F.int
    let lctrl = F.constant "KMOD_LCTRL" F.int
    let rctrl = F.constant "KMOD_RCTRL" F.int
    let lalt = F.constant "KMOD_LALT" F.int
    let ralt = F.constant "KMOD_RALT" F.int
    let lgui = F.constant "KMOD_LGUI" F.int
    let rgui = F.constant "KMOD_RGUI" F.int
    let num = F.constant "KMOD_NUM" F.int
    let caps = F.constant "KMOD_CAPS" F.int
    let mode = F.constant "KMOD_MODE" F.int
    let reserved = F.constant "KMOD_RESERVED" F.int
    let ctrl = F.constant "KMOD_CTRL" F.int
    let shift = F.constant "KMOD_SHIFT" F.int
    let alt = F.constant "KMOD_ALT" F.int
    let gui = F.constant "KMOD_GUI" F.int
  end

  module System_cursor = struct
    let arrow = F.constant "SDL_SYSTEM_CURSOR_ARROW" F.int
    let ibeam = F.constant "SDL_SYSTEM_CURSOR_IBEAM" F.int
    let wait = F.constant "SDL_SYSTEM_CURSOR_WAIT" F.int
    let crosshair = F.constant "SDL_SYSTEM_CURSOR_CROSSHAIR" F.int
    let waitarrow = F.constant "SDL_SYSTEM_CURSOR_WAITARROW" F.int
    let size_nw_se = F.constant "SDL_SYSTEM_CURSOR_SIZENWSE" F.int
    let size_ne_sw = F.constant "SDL_SYSTEM_CURSOR_SIZENESW" F.int
    let size_we = F.constant "SDL_SYSTEM_CURSOR_SIZEWE" F.int
    let size_ns = F.constant "SDL_SYSTEM_CURSOR_SIZENS" F.int
    let size_all = F.constant "SDL_SYSTEM_CURSOR_SIZEALL" F.int
    let no = F.constant "SDL_SYSTEM_CURSOR_NO" F.int
    let hand = F.constant "SDL_SYSTEM_CURSOR_HAND" F.int
  end

  module Button = struct
    let left = F.constant "SDL_BUTTON_LEFT" F.int
    let middle = F.constant "SDL_BUTTON_MIDDLE" F.int
    let right = F.constant "SDL_BUTTON_RIGHT" F.int
    let x1 = F.constant "SDL_BUTTON_X1" F.int
    let x2 = F.constant "SDL_BUTTON_X2" F.int

    let lmask = F.constant "SDL_BUTTON_LMASK" F.int32_t
    let mmask = F.constant "SDL_BUTTON_MMASK" F.int32_t
    let rmask = F.constant "SDL_BUTTON_RMASK" F.int32_t
    let x1mask = F.constant "SDL_BUTTON_X1MASK" F.int32_t
    let x2mask = F.constant "SDL_BUTTON_X2MASK" F.int32_t
  end

  (* Touch *)
  let touch_mouseid = F.constant "SDL_TOUCH_MOUSEID" F.int64_t

  module Hat = struct
    type t = int
    let centered = F.constant "SDL_HAT_CENTERED" F.int
    let up = F.constant "SDL_HAT_UP" F.int
    let right = F.constant "SDL_HAT_RIGHT" F.int
    let down = F.constant "SDL_HAT_DOWN" F.int
    let left = F.constant "SDL_HAT_LEFT" F.int
    let rightup = F.constant "SDL_HAT_RIGHTUP" F.int
    let rightdown = F.constant "SDL_HAT_RIGHTDOWN" F.int
    let leftup = F.constant "SDL_HAT_LEFTUP" F.int
    let leftdown = F.constant "SDL_HAT_LEFTDOWN" F.int
  end

  module Joystick_power_level = struct
    type t = int
    let unknown = F.constant "SDL_JOYSTICK_POWER_UNKNOWN" F.int
    let low = F.constant "SDL_JOYSTICK_POWER_LOW" F.int
    let medium = F.constant "SDL_JOYSTICK_POWER_MEDIUM" F.int
    let full = F.constant "SDL_JOYSTICK_POWER_FULL" F.int
    let max = F.constant "SDL_JOYSTICK_POWER_MAX" F.int
    let wired = F.constant "SDL_JOYSTICK_POWER_WIRED" F.int
  end

  module Joystick_type = struct
    type t = int
    let unknown = F.constant "SDL_JOYSTICK_TYPE_UNKNOWN" F.int
    let gamecontroller = F.constant "SDL_JOYSTICK_TYPE_GAMECONTROLLER" F.int
    let wheel = F.constant "SDL_JOYSTICK_TYPE_WHEEL" F.int
    let arcade_stick = F.constant "SDL_JOYSTICK_TYPE_ARCADE_STICK" F.int
    let flight_stick = F.constant "SDL_JOYSTICK_TYPE_FLIGHT_STICK" F.int
    let dance_pad = F.constant "SDL_JOYSTICK_TYPE_DANCE_PAD" F.int
    let guitar = F.constant "SDL_JOYSTICK_TYPE_GUITAR" F.int
    let drum_kit = F.constant "SDL_JOYSTICK_TYPE_DRUM_KIT" F.int
    let arcade_pad = F.constant "SDL_JOYSTICK_TYPE_ARCADE_PAD" F.int
    let throttle = F.constant "SDL_JOYSTICK_TYPE_THROTTLE" F.int
  end

  module Controller = struct
    type bind_type = int
    let bind_type_none = F.constant "SDL_CONTROLLER_BINDTYPE_NONE" F.int
    let bind_type_button = F.constant "SDL_CONTROLLER_BINDTYPE_BUTTON" F.int
    let bind_type_axis = F.constant "SDL_CONTROLLER_BINDTYPE_AXIS" F.int
    let bind_type_hat = F.constant "SDL_CONTROLLER_BINDTYPE_HAT" F.int

    type axis = int
    let axis_invalid = F.constant "SDL_CONTROLLER_AXIS_INVALID" F.int
    let axis_left_x = F.constant "SDL_CONTROLLER_AXIS_LEFTX" F.int
    let axis_left_y = F.constant "SDL_CONTROLLER_AXIS_LEFTY" F.int
    let axis_right_x = F.constant "SDL_CONTROLLER_AXIS_RIGHTX" F.int
    let axis_right_y = F.constant "SDL_CONTROLLER_AXIS_RIGHTY" F.int
    let axis_trigger_left = F.constant "SDL_CONTROLLER_AXIS_TRIGGERLEFT" F.int
    let axis_trigger_right = F.constant "SDL_CONTROLLER_AXIS_TRIGGERRIGHT" F.int
    let axis_max = F.constant "SDL_CONTROLLER_AXIS_MAX" F.int

    type button = int
    let button_invalid = F.constant "SDL_CONTROLLER_BUTTON_INVALID" F.int
    let button_a = F.constant "SDL_CONTROLLER_BUTTON_A" F.int
    let button_b = F.constant "SDL_CONTROLLER_BUTTON_B" F.int
    let button_x = F.constant "SDL_CONTROLLER_BUTTON_X" F.int
    let button_y = F.constant "SDL_CONTROLLER_BUTTON_Y" F.int
    let button_back = F.constant "SDL_CONTROLLER_BUTTON_BACK" F.int
    let button_guide = F.constant "SDL_CONTROLLER_BUTTON_GUIDE" F.int
    let button_start = F.constant "SDL_CONTROLLER_BUTTON_START" F.int
    let button_left_stick = F.constant "SDL_CONTROLLER_BUTTON_LEFTSTICK" F.int
    let button_right_stick = F.constant "SDL_CONTROLLER_BUTTON_RIGHTSTICK" F.int
    let button_left_shoulder = F.constant "SDL_CONTROLLER_BUTTON_LEFTSHOULDER" F.int
    let button_right_shoulder = F.constant "SDL_CONTROLLER_BUTTON_RIGHTSHOULDER" F.int
    let button_dpad_up = F.constant "SDL_CONTROLLER_BUTTON_DPAD_UP" F.int
    let button_dpad_down = F.constant "SDL_CONTROLLER_BUTTON_DPAD_DOWN" F.int
    let button_dpad_left = F.constant "SDL_CONTROLLER_BUTTON_DPAD_LEFT" F.int
    let button_dpad_right = F.constant "SDL_CONTROLLER_BUTTON_DPAD_RIGHT" F.int
    let button_max = F.constant "SDL_CONTROLLER_BUTTON_MAX" F.int
  end

  let sdl_query = F.constant "SDL_QUERY" F.int

  let disable = F.constant "SDL_DISABLE" F.uint8_t
  let enable = F.constant "SDL_ENABLE" F.uint8_t

  let pressed = F.constant "SDL_PRESSED" F.uint8_t
  let released = F.constant "SDL_RELEASED" F.uint8_t


  module Event = struct
    let first_event = F.constant "SDL_FIRSTEVENT" F.int
    let last_event = F.constant "SDL_LASTEVENT" F.int

    let quit = F.constant "SDL_QUIT" F.int

    let app_terminating = F.constant "SDL_APP_TERMINATING" F.int
    let app_low_memory = F.constant "SDL_APP_LOWMEMORY" F.int
    let app_will_enter_background = F.constant "SDL_APP_WILLENTERBACKGROUND" F.int
    let app_did_enter_background = F.constant "SDL_APP_DIDENTERBACKGROUND" F.int
    let app_will_enter_foreground = F.constant "SDL_APP_WILLENTERFOREGROUND" F.int
    let app_did_enter_foreground = F.constant "SDL_APP_DIDENTERFOREGROUND" F.int

    let display_event = F.constant "SDL_DISPLAYEVENT" F.int
    let window_event = F.constant "SDL_WINDOWEVENT" F.int
    let sys_wm_event = F.constant "SDL_SYSWMEVENT" F.int
    let sensor_update = F.constant "SDL_SENSORUPDATE" F.int
    let user_event = F.constant "SDL_USEREVENT" F.int

    let key_down = F.constant "SDL_KEYDOWN" F.int
    let key_up = F.constant "SDL_KEYUP" F.int
    let keymap_changed = F.constant "SDL_KEYMAPCHANGED" F.int

    let text_editing = F.constant "SDL_TEXTEDITING" F.int
    let text_input = F.constant "SDL_TEXTINPUT" F.int

    let mouse_motion = F.constant "SDL_MOUSEMOTION" F.int
    let mouse_button_down = F.constant "SDL_MOUSEBUTTONDOWN" F.int
    let mouse_button_up = F.constant "SDL_MOUSEBUTTONUP" F.int
    let mouse_wheel = F.constant "SDL_MOUSEWHEEL" F.int

    type mouse_wheel_direction = int
    let mouse_wheel_normal = F.constant "SDL_MOUSEWHEEL_NORMAL" F.int
    let mouse_wheel_flipped = F.constant "SDL_MOUSEWHEEL_FLIPPED" F.int

    let joy_axis_motion = F.constant "SDL_JOYAXISMOTION" F.int
    let joy_ball_motion = F.constant "SDL_JOYBALLMOTION" F.int
    let joy_hat_motion = F.constant "SDL_JOYHATMOTION" F.int
    let joy_button_down = F.constant "SDL_JOYBUTTONDOWN" F.int
    let joy_button_up = F.constant "SDL_JOYBUTTONUP" F.int
    let joy_device_added = F.constant "SDL_JOYDEVICEADDED" F.int
    let joy_device_removed = F.constant "SDL_JOYDEVICEREMOVED" F.int

    let controller_axis_motion = F.constant "SDL_CONTROLLERAXISMOTION" F.int
    let controller_button_down = F.constant "SDL_CONTROLLERBUTTONDOWN" F.int
    let controller_button_up = F.constant "SDL_CONTROLLERBUTTONUP" F.int
    let controller_device_added = F.constant "SDL_CONTROLLERDEVICEADDED" F.int
    let controller_device_removed = F.constant "SDL_CONTROLLERDEVICEREMOVED" F.int
    let controller_device_remapped = F.constant "SDL_CONTROLLERDEVICEREMAPPED" F.int

    let finger_down = F.constant "SDL_FINGERDOWN" F.int
    let finger_up = F.constant "SDL_FINGERUP" F.int
    let finger_motion = F.constant "SDL_FINGERMOTION" F.int

    let dollar_gesture = F.constant "SDL_DOLLARGESTURE" F.int
    let dollar_record = F.constant "SDL_DOLLARRECORD" F.int

    let multi_gesture = F.constant "SDL_MULTIGESTURE" F.int

    let clipboard_update = F.constant "SDL_CLIPBOARDUPDATE" F.int

    let drop_file = F.constant "SDL_DROPFILE" F.int
    let drop_text = F.constant "SDL_DROPTEXT" F.int
    let drop_begin = F.constant "SDL_DROPBEGIN" F.int
    let drop_complete = F.constant "SDL_DROPCOMPLETE" F.int

    let audio_device_added = F.constant "SDL_AUDIODEVICEADDED" F.int
    let audio_device_removed = F.constant "SDL_AUDIODEVICEREMOVED" F.int

    let render_targets_reset = F.constant "SDL_RENDER_TARGETS_RESET" F.int
    let render_device_reset = F.constant "SDL_RENDER_DEVICE_RESET" F.int

    let texteditingevent_text_size = F.constant "SDL_TEXTEDITINGEVENT_TEXT_SIZE" F.int
    let textinputevent_text_size = F.constant "SDL_TEXTINPUTEVENT_TEXT_SIZE" F.int

    (* SDL_WindowEventID *)
    type window_event_id = int
    let window_event_shown = F.constant "SDL_WINDOWEVENT_SHOWN" F.int
    let window_event_hidden = F.constant "SDL_WINDOWEVENT_HIDDEN" F.int
    let window_event_exposed = F.constant "SDL_WINDOWEVENT_EXPOSED" F.int
    let window_event_moved = F.constant "SDL_WINDOWEVENT_MOVED" F.int
    let window_event_resized = F.constant "SDL_WINDOWEVENT_RESIZED" F.int
    let window_event_size_changed = F.constant "SDL_WINDOWEVENT_SIZE_CHANGED" F.int
    let window_event_minimized = F.constant "SDL_WINDOWEVENT_MINIMIZED" F.int
    let window_event_maximized = F.constant "SDL_WINDOWEVENT_MAXIMIZED" F.int
    let window_event_restored = F.constant "SDL_WINDOWEVENT_RESTORED" F.int
    let window_event_enter = F.constant "SDL_WINDOWEVENT_ENTER" F.int
    let window_event_leave = F.constant "SDL_WINDOWEVENT_LEAVE" F.int
    let window_event_focus_gained = F.constant "SDL_WINDOWEVENT_FOCUS_GAINED" F.int
    let window_event_focus_lost = F.constant "SDL_WINDOWEVENT_FOCUS_LOST" F.int
    let window_event_close = F.constant "SDL_WINDOWEVENT_CLOSE" F.int
    let window_event_take_focus = F.constant "SDL_WINDOWEVENT_TAKE_FOCUS" F.int
    let window_event_hit_test = F.constant "SDL_WINDOWEVENT_HIT_TEST" F.int
  end

  module Haptic = struct
    let constant = F.constant "SDL_HAPTIC_CONSTANT" F.int

    let sine = F.constant "SDL_HAPTIC_SINE" F.int
    let left_right = F.constant "SDL_HAPTIC_LEFTRIGHT" F.int
    let triangle = F.constant "SDL_HAPTIC_TRIANGLE" F.int
    let sawtooth_up = F.constant "SDL_HAPTIC_SAWTOOTHUP" F.int
    let sawtooth_down = F.constant "SDL_HAPTIC_SAWTOOTHDOWN" F.int

    let ramp = F.constant "SDL_HAPTIC_RAMP" F.int

    let spring = F.constant "SDL_HAPTIC_SPRING" F.int
    let damper = F.constant "SDL_HAPTIC_DAMPER" F.int
    let inertia = F.constant "SDL_HAPTIC_INERTIA" F.int
    let friction = F.constant "SDL_HAPTIC_FRICTION" F.int

    let custom = F.constant "SDL_HAPTIC_CUSTOM" F.int

    let infinity = F.constant "SDL_HAPTIC_INFINITY" F.int32_t

    type feature = int
    let gain = F.constant "SDL_HAPTIC_GAIN" F.int
    let autocenter = F.constant "SDL_HAPTIC_AUTOCENTER" F.int
    let status = F.constant "SDL_HAPTIC_STATUS" F.int
    let pause = F.constant "SDL_HAPTIC_PAUSE" F.int

    type direction_type = int
    let polar = F.constant "SDL_HAPTIC_POLAR" F.int
    let cartesian = F.constant "SDL_HAPTIC_CARTESIAN" F.int
    let spherical = F.constant "SDL_HAPTIC_SPHERICAL" F.int
  end

  module Audio = struct
    type status = int
    let stopped = F.constant "SDL_AUDIO_STOPPED" F.int
    let playing = F.constant "SDL_AUDIO_PLAYING" F.int
    let paused = F.constant "SDL_AUDIO_PAUSED" F.int

    type format = int
    let s8 = F.constant "AUDIO_S8" F.int
    let u8 = F.constant "AUDIO_U8" F.int
    let s16_lsb = F.constant "AUDIO_S16LSB" F.int
    let s16_msb = F.constant "AUDIO_S16MSB" F.int
    let s16_sys = F.constant "AUDIO_S16SYS" F.int
    let s16 = F.constant "AUDIO_S16" F.int
    let u16_lsb = F.constant "AUDIO_U16LSB" F.int
    let u16_msb = F.constant "AUDIO_U16MSB" F.int
    let u16_sys = F.constant "AUDIO_U16SYS" F.int
    let u16 = F.constant "AUDIO_U16" F.int
    let s32_lsb = F.constant "AUDIO_S32LSB" F.int
    let s32_msb = F.constant "AUDIO_S32MSB" F.int
    let s32_sys = F.constant "AUDIO_S32SYS" F.int
    let s32 = F.constant "AUDIO_S32" F.int
    let f32_lsb = F.constant "AUDIO_F32LSB" F.int
    let f32_msb = F.constant "AUDIO_F32MSB" F.int
    let f32_sys = F.constant "AUDIO_F32SYS" F.int
    let f32 = F.constant "AUDIO_F32" F.int

    type allow = int
    let allow_frequency_change = F.constant "SDL_AUDIO_ALLOW_FREQUENCY_CHANGE" F.int
    let allow_format_change = F.constant "SDL_AUDIO_ALLOW_FORMAT_CHANGE" F.int
    let allow_channels_change = F.constant "SDL_AUDIO_ALLOW_CHANNELS_CHANGE" F.int
    let allow_any_change = F.constant "SDL_AUDIO_ALLOW_ANY_CHANGE" F.int
  end

  module Powerstate = struct
    let unknown = F.constant "SDL_POWERSTATE_UNKNOWN" F.int
    let on_battery = F.constant "SDL_POWERSTATE_ON_BATTERY" F.int
    let no_battery = F.constant "SDL_POWERSTATE_NO_BATTERY" F.int
    let charging = F.constant "SDL_POWERSTATE_CHARGING" F.int
    let charged = F.constant "SDL_POWERSTATE_CHARGED" F.int
  end
end