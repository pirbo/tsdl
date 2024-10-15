open Ctypes

module Types = Types_generated

let const_string_opt =
  Ctypes_std_views.nullable_view string Ctypes_static.(const char)

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
  let quit =
    F.(foreign "SDL_Quit" (void @-> returning void))
  let quit_sub_system =
    F.(foreign "SDL_QuitSubSystem" (uint32_t @-> returning void))

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

  let clear_hints =
    F.(foreign "SDL_ClearHints" (void @-> returning void))

  let get_hint =
    F.(foreign "SDL_GetHint" (string @-> returning const_string_opt))

  let get_hint_boolean =
    F.(foreign "SDL_GetHintBoolean" (string @-> bool @-> returning bool))

  let set_hint =
    F.(foreign "SDL_SetHint" (string @-> string @-> returning bool))

  let set_hint_with_priority =
    F.(foreign "SDL_SetHintWithPriority"
         (string @-> string @-> int @-> returning bool))

  (* Errors *)

  let clear_error =
    F.(foreign "SDL_ClearError" (void @-> returning void))

  let set_error =
    F.(foreign "SDL_SetError" (string @-> returning int))

  (* Log *)

  let log_get_priority =
    F.(foreign "SDL_LogGetPriority" (int @-> returning int))

  let log_reset_priorities =
    F.(foreign "SDL_LogResetPriorities" (void @-> returning void))

  let log_set_all_priority =
    F.(foreign "SDL_LogSetAllPriority" (int @-> returning void))

  let log_set_priority =
    F.(foreign "SDL_LogSetPriority" (int @-> int @-> returning void))

  (* Version *)

  let get_version =
    F.(foreign "SDL_GetVersion" (ptr Types.version @-> returning void))

  let get_revision =
    F.(foreign "SDL_GetRevision" (void @-> returning string))

  let get_revision_number =
    F.(foreign "SDL_GetRevisionNumber" (void @-> returning int))

  let create_window =
    F.(foreign "SDL_CreateWindow"
         (string @-> int @-> int @-> int @-> int @-> uint32_t @->
          returning Types.Window.opt))
  let destroy_window =
    F.(foreign "SDL_DestroyWindow" (Types.Window.t @-> returning void))

  let pump_events =
    F.(foreign "SDL_PumpEvents" (void @-> returning void))

  (* IO absraction *)

  let load_file_rw =
    F.(foreign "SDL_LoadFile_RW"
         (Types.rw_ops @-> ptr size_t @-> bool @-> returning string_opt))

  let rw_close =
    F.(foreign "SDL_RWclose" (Types.rw_ops @-> returning int))

  let rw_from_file =
    F.(foreign "SDL_RWFromFile"
         (string @-> string @-> returning Types.rw_ops_opt))

  let rw_from_const_mem =
    F.(foreign "SDL_RWFromConstMem"
         (ocaml_string @-> int @-> returning Types.rw_ops_opt))

  let rw_from_mem =
    F.(foreign "SDL_RWFromMem"
         (ocaml_bytes @-> int @-> returning Types.rw_ops_opt))

  (* File system paths *)

  let get_base_path =
    F.(foreign "SDL_GetBasePath" (void @-> returning (ptr char)))

  let get_pref_path =
    F.(foreign "SDL_GetPrefPath" (string @-> string @-> returning (ptr char)))

  (* Rectangles *)

  let enclose_points =
    F.(foreign "SDL_EnclosePoints"
         (ptr void @-> int @-> ptr Types.Rect.t @-> ptr Types.Rect.t @->
          returning bool))

  let has_intersection =
    F.(foreign "SDL_HasIntersection"
         (ptr Types.Rect.t @-> ptr Types.Rect.t @-> returning bool))

  let intersect_rect =
    F.(foreign "SDL_IntersectRect"
         (ptr Types.Rect.t @-> ptr Types.Rect.t @-> ptr Types.Rect.t @->
          returning bool))

  let intersect_rect_and_line =
    F.(foreign "SDL_IntersectRectAndLine"
         (ptr Types.Rect.t @-> ptr int @-> ptr int @-> ptr int @-> ptr int @->
          returning bool))

  let point_in_rect =
    F.(foreign "SDL_PointInRect"
         (ptr Types.Point.t @-> ptr Types.Rect.t @-> returning bool))

  let rect_empty =
    F.(foreign "SDL_RectEmpty" (ptr Types.Rect.t @-> returning bool))

  let rect_equals =
    F.(foreign "SDL_RectEquals"
         (ptr Types.Rect.t @-> ptr Types.Rect.t @-> returning bool))

  let union_rect =
    F.(foreign "SDL_UnionRect"
         (ptr Types.Rect.t @-> ptr Types.Rect.t @-> ptr Types.Rect.t @->
          returning void))

  let alloc_palette =
    F.(foreign "SDL_AllocPalette" (int @-> returning (ptr_opt Types.palette)))

  let free_palette =
    F.(foreign "SDL_FreePalette" (ptr Types.palette @-> returning void))

  let set_palette_colors =
    F.(foreign "SDL_SetPaletteColors"
         (ptr Types.palette @-> ptr void @-> int @-> int @-> returning int))


  let calculate_gamma_ramp =
    F.(foreign "SDL_CalculateGammaRamp" (float @-> ptr void @-> returning void))

  let compose_custom_blend_mode =
    F.(foreign "SDL_ComposeCustomBlendMode"
         (int @-> int @-> int @-> int @-> int @-> int @-> returning uint))
end
