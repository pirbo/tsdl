module Types = Types_generated

module Functions (F : Ctypes.FOREIGN) = struct
  module Hint = struct
    let framebuffer_acceleration =
      F.foreign_value
        "SDL_HINT_FRAMEBUFFER_ACCELERATION" Ctypes.(array 29 char)
    let idle_timer_disabled =
      F.foreign_value "SDL_HINT_IDLE_TIMER_DISABLED" Ctypes.(array 28 char)
    let mouse_focus_clickthrough =
      F.foreign_value
        "SDL_HINT_MOUSE_FOCUS_CLICKTHROUGH" Ctypes.(array 29 char)
    let orientations =
      F.foreign_value "SDL_HINT_ORIENTATIONS" Ctypes.(array 21 char)
    let render_driver =
      F.foreign_value "SDL_HINT_RENDER_DRIVER" Ctypes.(array 18 char)
    let render_opengl_shaders =
      F.foreign_value "SDL_HINT_RENDER_OPENGL_SHADERS" Ctypes.(array 26 char)
    let render_logical_size_mode =
      F.foreign_value
        "SDL_HINT_RENDER_LOGICAL_SIZE_MODE" Ctypes.(array 29 char)
    let render_scale_quality =
      F.foreign_value "SDL_HINT_RENDER_SCALE_QUALITY" Ctypes.(array 25 char)
    let render_vsync =
      F.foreign_value "SDL_HINT_RENDER_VSYNC" Ctypes.(array 17 char)

    let no_signal_handlers =
      F.foreign_value "SDL_HINT_NO_SIGNAL_HANDLERS" Ctypes.(array 23 char)
    let thread_stack_size =
      F.foreign_value "SDL_HINT_THREAD_STACK_SIZE" Ctypes.(array 22 char)
    let window_frame_usable_while_cursor_hidden =
      F.foreign_value
        "SDL_HINT_WINDOW_FRAME_USABLE_WHILE_CURSOR_HIDDEN"
        Ctypes.(array 44 char)

    let audio_resampling_mode =
      F.foreign_value "SDL_HINT_AUDIO_RESAMPLING_MODE" Ctypes.(array 26 char)
    let mouse_normal_speed_scale =
      F.foreign_value "SDL_HINT_MOUSE_NORMAL_SPEED_SCALE" Ctypes.(array 29 char)
    let mouse_relative_speed_scale =
      F.foreign_value
        "SDL_HINT_MOUSE_RELATIVE_SPEED_SCALE" Ctypes.(array 31 char)
    let touch_mouse_events =
      F.foreign_value "SDL_HINT_TOUCH_MOUSE_EVENTS" Ctypes.(array 23 char)
    let mouse_touch_events =
      F.foreign_value "SDL_HINT_MOUSE_TOUCH_EVENTS" Ctypes.(array 23 char)
  end
end
