open CamlSDL2
open Core

let pos_x : int ref = ref 0
let pos_y : int ref = ref 0
let b1_active : bool ref = ref false
let b2_active : bool ref = ref false

let rec update ms = function
  | ({ transform_cmp = Some trans
     ; mousein_cmp   = Some _mousein; _} : Entity.t) :: tail ->
    trans.x <- !pos_x;
    trans.y <- !pos_y;
    update ms tail
  | _ :: tail -> update ms tail
  | [] -> ()

let handle_event = function
| Sdl.Event.SDL_MOUSEMOTION e ->
  pos_x := e.mm_x;
  pos_y := e.mm_y
| Sdl.Event.SDL_MOUSEBUTTONDOWN {mb_button = 1; _} ->
  b1_active := true
| Sdl.Event.SDL_MOUSEBUTTONDOWN {mb_button = 2; _} ->
  b2_active := true
| Sdl.Event.SDL_MOUSEBUTTONUP {mb_button = 1; _} ->
  b1_active := false
| Sdl.Event.SDL_MOUSEBUTTONUP {mb_button = 2; _} ->
  b2_active := false
| _ -> ()

