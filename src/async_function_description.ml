open Ctypes

module Types = Types_generated

module Functions (F : FOREIGN) = struct
  let delay =
    F.(foreign "SDL_Delay" (int32_t @-> returning void))

  let render_present =
    F.(foreign "SDL_RenderPresent" (ptr void @-> returning void))
end
