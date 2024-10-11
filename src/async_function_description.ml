open Ctypes

module Types = Types_generated

module Functions (F : FOREIGN) = struct
  let delay =
    F.(foreign "SDL_Delay" (int32_t @-> returning void))
end
