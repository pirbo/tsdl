open Ctypes

module Types = Types_generated

module Functions (F : FOREIGN) = struct
  let get_error =
    F.(foreign "SDL_GetError" (void @-> returning string))
  let sdl_free = F.(foreign "SDL_free" (ptr void @-> returning void))
  let set_main_ready = F.(foreign "SDL_SetMainReady" (void @-> returning void))
  let init =
    F.(foreign "SDL_Init" (uint32_t @-> returning int))
  let init_sub_system =
    F.(foreign "SDL_InitSubSystem" (uint32_t @-> returning int))
  let was_init =
    F.(foreign "SDL_WasInit" (uint32_t @-> returning uint32_t))

  module Hint = struct
    let framebuffer_acceleration =
      F.foreign_value "SDL_HINT_FRAMEBUFFER_ACCELERATION" (array 29 char)
    let idle_timer_disabled =
      F.foreign_value "SDL_HINT_IDLE_TIMER_DISABLED" (array 28 char)
    let mouse_focus_clickthrough =
      F.foreign_value "SDL_HINT_MOUSE_FOCUS_CLICKTHROUGH" (array 29 char)
    let orientations =
      F.foreign_value "SDL_HINT_ORIENTATIONS" (array 21 char)
    let render_driver =
      F.foreign_value "SDL_HINT_RENDER_DRIVER" (array 18 char)
    let render_opengl_shaders =
      F.foreign_value "SDL_HINT_RENDER_OPENGL_SHADERS" (array 26 char)
    let render_logical_size_mode =
      F.foreign_value "SDL_HINT_RENDER_LOGICAL_SIZE_MODE" (array 29 char)
    let render_scale_quality =
      F.foreign_value "SDL_HINT_RENDER_SCALE_QUALITY" (array 25 char)
    let render_vsync =
      F.foreign_value "SDL_HINT_RENDER_VSYNC" (array 17 char)

    let no_signal_handlers =
      F.foreign_value "SDL_HINT_NO_SIGNAL_HANDLERS" (array 23 char)
    let thread_stack_size =
      F.foreign_value "SDL_HINT_THREAD_STACK_SIZE" (array 22 char)
    let window_frame_usable_while_cursor_hidden =
      F.foreign_value
        "SDL_HINT_WINDOW_FRAME_USABLE_WHILE_CURSOR_HIDDEN" (array 44 char)

    let audio_resampling_mode =
      F.foreign_value "SDL_HINT_AUDIO_RESAMPLING_MODE" (array 26 char)
    let mouse_normal_speed_scale =
      F.foreign_value "SDL_HINT_MOUSE_NORMAL_SPEED_SCALE" (array 29 char)
    let mouse_relative_speed_scale =
      F.foreign_value "SDL_HINT_MOUSE_RELATIVE_SPEED_SCALE" (array 31 char)
    let touch_mouse_events =
      F.foreign_value "SDL_HINT_TOUCH_MOUSE_EVENTS" (array 23 char)
    let mouse_touch_events =
      F.foreign_value "SDL_HINT_MOUSE_TOUCH_EVENTS" (array 23 char)
  end
end
