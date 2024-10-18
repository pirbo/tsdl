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
         (int @-> int @-> int @-> int @-> int @-> int @-> returning Types.Blend.mode))

  let alloc_format =
    F.(foreign "SDL_AllocFormat"
         (uint32_t @-> returning (ptr_opt Types.pixel_format)))

  let free_format =
    F.(foreign "SDL_FreeFormat" (ptr Types.pixel_format @-> returning void))

  let get_pixel_format_name =
    F.(foreign "SDL_GetPixelFormatName" (uint32_t @-> returning string))

  let get_rgb =
    F.(foreign "SDL_GetRGB"
         (uint32_t @-> ptr Types.pixel_format @-> ptr uint8_t @->
          ptr uint8_t @-> ptr uint8_t @-> returning void))

  let get_rgba =
    F.(foreign "SDL_GetRGBA"
         (uint32_t @-> ptr Types.pixel_format @-> ptr uint8_t @->
          ptr uint8_t @-> ptr uint8_t @-> ptr uint8_t @-> returning void))

  let map_rgb =
    F.(foreign "SDL_MapRGB"
         (ptr Types.pixel_format @-> uint8_t @-> uint8_t @-> uint8_t @->
          returning uint32_t))

  let map_rgba =
    F.(foreign "SDL_MapRGBA"
         (ptr Types.pixel_format @-> uint8_t @-> uint8_t @-> uint8_t @->
          uint8_t @-> returning uint32_t))

  let masks_to_pixel_format_enum =
    F.(foreign "SDL_MasksToPixelFormatEnum"
         (int @-> uint32_t @-> uint32_t @-> uint32_t @-> uint32_t @->
          returning uint32_t))

  let pixel_format_enum_to_masks =
    F.(foreign "SDL_PixelFormatEnumToMasks"
      (uint32_t @-> ptr int @->
       ptr uint32_t @-> ptr uint32_t @-> ptr uint32_t @-> ptr uint32_t @->
       returning bool))

  let set_pixel_format_palette =
    F.(foreign "SDL_SetPixelFormatPalette"
         (ptr Types.pixel_format @-> ptr Types.palette @-> returning int))

  let blit_scaled =
    (* SDL_BlitScaled is #ifdef'd to SDL_UpperBlitScaled *)
    F.(foreign "SDL_UpperBlitScaled"
         (ptr Types.surface @-> ptr Types.Rect.t @-> ptr Types.surface @->
          ptr Types.Rect.t @-> returning int))

  let blit_surface =
    (* SDL_BlitSurface is #ifdef'd to SDL_UpperBlit *)
    F.(foreign "SDL_UpperBlit"
         (ptr Types.surface @-> ptr Types.Rect.t @-> ptr Types.surface @->
          ptr Types.Rect.t @-> returning int))

  let convert_pixels =
    F.(foreign "SDL_ConvertPixels"
         (int @-> int @-> uint32_t @-> ptr void @-> int @-> uint32_t @->
          ptr void @-> int @-> returning int))

  let convert_surface =
    F.(foreign "SDL_ConvertSurface"
         (ptr Types.surface @-> ptr Types.pixel_format @-> uint32_t @->
          returning (ptr_opt Types.surface)))

  let convert_surface_format =
    F.(foreign "SDL_ConvertSurfaceFormat"
         (ptr Types.surface @-> uint32_t @-> uint32_t @->
          returning (ptr_opt Types.surface)))

  let create_rgb_surface =
    F.(foreign "SDL_CreateRGBSurface"
         (uint32_t @-> int @-> int @-> int @-> uint32_t @-> uint32_t @->
          uint32_t @-> uint32_t @-> returning (ptr_opt Types.surface)))

  let create_rgb_surface_from =
    F.(foreign "SDL_CreateRGBSurfaceFrom"
         (ptr void @-> int @-> int @-> int @-> int @-> uint32_t @->
          uint32_t @-> uint32_t @-> uint32_t @->
          returning (ptr_opt Types.surface)))

  let create_rgb_surface_with_format =
    F.(foreign "SDL_CreateRGBSurfaceWithFormat"
         (uint32_t @-> int @-> int @-> int @-> uint32_t @->
          returning (ptr_opt Types.surface)))

  let create_rgb_surface_with_format_from =
    F.(foreign "SDL_CreateRGBSurfaceWithFormatFrom"
         (ptr void @-> int @-> int @-> int @-> int @-> uint32_t @->
          returning (ptr_opt Types.surface)))

  let duplicate_surface =
    F.(foreign "SDL_DuplicateSurface"
         (ptr Types.surface @-> returning (ptr Types.surface)))

  let fill_rect =
    F.(foreign "SDL_FillRect"
         (ptr Types.surface @-> ptr Types.Rect.t@-> uint32_t @-> returning int))

  let fill_rects =
    F.(foreign "SDL_FillRects"
         (ptr Types.surface @-> ptr void @-> int @-> uint32_t @->
          returning int))

  let free_surface =
    F.(foreign "SDL_FreeSurface" (ptr Types.surface @-> returning void))

  let get_clip_rect =
    F.(foreign "SDL_GetClipRect"
         (ptr Types.surface @-> ptr Types.Rect.t @-> returning void))

  let get_color_key =
    F.(foreign "SDL_GetColorKey"
         (ptr Types.surface @-> ptr uint32_t @-> returning int))

  let get_surface_alpha_mod =
    F.(foreign "SDL_GetSurfaceAlphaMod"
         (ptr Types.surface @-> ptr uint8_t @-> returning int))

  let get_surface_blend_mode =
    F.(foreign "SDL_GetSurfaceBlendMode"
         (ptr Types.surface @-> ptr Types.Blend.mode @-> returning int))

  let get_surface_color_mod =
    F.(foreign "SDL_GetSurfaceColorMod"
         (ptr Types.surface @-> ptr uint8_t @-> ptr uint8_t @-> ptr uint8_t @->
          returning int))

  let load_bmp_rw =
    F.(foreign "SDL_LoadBMP_RW"
         (Types.rw_ops @-> bool @-> returning (ptr_opt Types.surface)))

  let lock_surface =
    F.(foreign "SDL_LockSurface" (ptr Types.surface @-> returning int))

  let lower_blit =
    F.(foreign "SDL_LowerBlit"
         (ptr Types.surface @-> ptr Types.Rect.t @-> ptr Types.surface @->
          ptr Types.Rect.t @-> returning int))

  let lower_blit_scaled =
    F.(foreign "SDL_LowerBlitScaled"
         (ptr Types.surface @-> ptr Types.Rect.t @-> ptr Types.surface @->
          ptr Types.Rect.t @-> returning int))

  let save_bmp_rw =
    F.(foreign "SDL_SaveBMP_RW"
         (ptr Types.surface @-> Types.rw_ops @-> bool @-> returning int))

  let set_clip_rect =
    F.(foreign "SDL_SetClipRect"
         (ptr Types.surface @-> ptr Types.Rect.t @-> returning bool))

  let set_color_key =
    F.(foreign "SDL_SetColorKey"
         (ptr Types.surface @-> bool @-> uint32_t @-> returning int))

  let set_surface_alpha_mod =
    F.(foreign "SDL_SetSurfaceAlphaMod"
         (ptr Types.surface @-> uint8_t @-> returning int))

  let set_surface_blend_mode =
    F.(foreign "SDL_SetSurfaceBlendMode"
         (ptr Types.surface @-> Types.Blend.mode @-> returning int))

  let set_surface_color_mod =
    F.(foreign "SDL_SetSurfaceColorMod"
         (ptr Types.surface @-> uint8_t @-> uint8_t @-> uint8_t @->
          returning int))

  let set_surface_palette =
    F.(foreign "SDL_SetSurfacePalette"
         (ptr Types.surface @-> ptr Types.palette @-> returning int))

  let set_surface_rle =
    F.(foreign "SDL_SetSurfaceRLE"
         (ptr Types.surface @-> bool @-> returning int))

  let unlock_surface =
    F.(foreign "SDL_UnlockSurface" (ptr Types.surface @-> returning void))

  let create_renderer =
    F.(foreign "SDL_CreateRenderer"
         (Types.Window.t @-> int @-> uint32_t @-> returning (ptr_opt void)))

  let create_software_renderer =
    F.(foreign "SDL_CreateSoftwareRenderer"
         (ptr Types.surface @-> returning (ptr_opt void)))

  let destroy_renderer =
    F.(foreign "SDL_DestroyRenderer" (ptr void @-> returning void))

  let get_num_render_drivers =
    F.(foreign "SDL_GetNumRenderDrivers" (void @-> returning int))

  let get_render_draw_blend_mode =
    F.(foreign "SDL_GetRenderDrawBlendMode"
         (ptr void @-> ptr Types.Blend.mode @-> returning int))

  let get_render_draw_color =
    F.(foreign "SDL_GetRenderDrawColor"
         (ptr void @-> ptr uint8_t @-> ptr uint8_t @-> ptr uint8_t @->
          ptr uint8_t @-> returning int))

  let get_render_driver_info =
    F.(foreign "SDL_GetRenderDriverInfo"
         (int @-> ptr Types.renderer_info @-> returning int))

  let get_render_target =
    F.(foreign "SDL_GetRenderTarget" (ptr void @-> returning (ptr_opt void)))

  let get_renderer =
    F.(foreign "SDL_GetRenderer"
         (Types.Window.t @-> returning (ptr_opt void)))

  let get_renderer_info =
    F.(foreign "SDL_GetRendererInfo"
         (ptr void @-> ptr Types.renderer_info @-> returning int))

  let get_renderer_output_size =
    F.(foreign "SDL_GetRendererOutputSize"
         (ptr void @-> ptr int @-> ptr int @-> returning int))

  let render_clear =
    F.(foreign "SDL_RenderClear" (ptr void @-> returning int))

  let render_copy =
    F.(foreign "SDL_RenderCopy"
         (ptr void @-> ptr void @-> ptr Types.Rect.t @-> ptr Types.Rect.t @->
          returning int))

  let render_copy_ex =
    F.(foreign "SDL_RenderCopyEx"
         (ptr void @-> ptr void @-> ptr Types.Rect.t @-> ptr Types.Rect.t @->
          double @-> ptr Types.Point.t @-> int @-> returning int))

  let render_draw_line =
    F.(foreign "SDL_RenderDrawLine"
         (ptr void @-> int @-> int @-> int @-> int @-> returning int))

  let render_draw_line_f =
    F.(foreign "SDL_RenderDrawLineF"
         (ptr void @-> float @-> float @-> float @-> float @-> returning int))

  let render_draw_lines =
    F.(foreign "SDL_RenderDrawLines"
         (ptr void @-> ptr void @-> int @-> returning int))

  let render_draw_point =
    F.(foreign "SDL_RenderDrawPoint"
         (ptr void @-> int @-> int @-> returning int))

  let render_draw_points =
    F.(foreign "SDL_RenderDrawPoints"
         (ptr void @-> ptr void @-> int @-> returning int))

  let render_draw_point_f =
    F.(foreign "SDL_RenderDrawPointF"
         (ptr void @-> float @-> float @-> returning int))

  let render_draw_points_f =
    F.(foreign "SDL_RenderDrawPointsF"
         (ptr void @-> ptr void @-> int @-> returning int))

  let render_draw_rect =
    F.(foreign "SDL_RenderDrawRect"
         (ptr void @-> ptr Types.Rect.t @-> returning int))

  let render_draw_rects =
    F.(foreign "SDL_RenderDrawRects"
         (ptr void @-> ptr void @-> int @-> returning int))

  let render_fill_rect =
    F.(foreign "SDL_RenderFillRect"
         (ptr void @-> ptr Types.Rect.t @-> returning int))

  let render_fill_rects =
    F.(foreign "SDL_RenderFillRects"
         (ptr void @-> ptr void @-> int @-> returning int))

  let render_geometry =
    F.(foreign "SDL_RenderGeometry"
         (ptr void @-> ptr void @-> ptr void @-> int @-> ptr void @-> int @->
          returning int))

  let render_geometry_raw =
    F.(foreign "SDL_RenderGeometryRaw"
         (ptr void @-> ptr void @->
          ptr void @-> int @->
          ptr void @-> int @->
          ptr void @-> int @->
          int @-> ptr void @-> int @-> int @-> returning int))

  let render_get_clip_rect =
    F.(foreign "SDL_RenderGetClipRect"
         (ptr void @-> ptr Types.Rect.t @-> returning void))

  let render_is_clip_enabled =
    F.(foreign "SDL_RenderIsClipEnabled" (ptr void @-> returning bool))

  let render_get_integer_scale =
    F.(foreign "SDL_RenderGetIntegerScale" (ptr void @-> returning bool))

  let render_get_logical_size =
    F.(foreign "SDL_RenderGetLogicalSize"
         (ptr void @-> ptr int @-> ptr int @-> returning void))

  let render_get_scale =
    F.(foreign "SDL_RenderGetScale"
         (ptr void @-> ptr float @-> ptr float @-> returning void))

  let render_get_viewport =
    F.(foreign "SDL_RenderGetViewport"
         (ptr void @-> ptr Types.Rect.t @-> returning void))

  let render_read_pixels =
    F.(foreign "SDL_RenderReadPixels"
         (ptr void @-> ptr Types.Rect.t @-> uint32_t @-> ptr void @-> int @->
          returning int))

  let render_set_clip_rect =
    F.(foreign "SDL_RenderSetClipRect"
         (ptr void @-> ptr Types.Rect.t @-> returning int))

  let render_set_integer_scale =
    F.(foreign "SDL_RenderSetIntegerScale"
         (ptr void @-> bool @-> returning int))

  let render_set_logical_size =
    F.(foreign "SDL_RenderSetLogicalSize"
         (ptr void @-> int @-> int @-> returning int))

  let render_set_scale =
    F.(foreign "SDL_RenderSetScale"
         (ptr void @-> float @-> float @-> returning int))

  let render_set_viewport =
    F.(foreign "SDL_RenderSetViewport"
         (ptr void @-> ptr Types.Rect.t @-> returning int))

  let render_target_supported =
    F.(foreign "SDL_RenderTargetSupported" (ptr void @-> returning bool))

  let set_render_draw_blend_mode =
    F.(foreign "SDL_SetRenderDrawBlendMode"
         (ptr void @-> Types.Blend.mode @-> returning int))

  let set_render_draw_color =
    F.(foreign "SDL_SetRenderDrawColor"
         (ptr void @-> uint8_t @-> uint8_t @-> uint8_t @->uint8_t @->
          returning int))

  let set_render_target =
    F.(foreign "SDL_SetRenderTarget" (ptr void @-> ptr void @-> returning int))
end
