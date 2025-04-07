open CamlSDL2
open Core



let speed = 0.5
let diag_vec = 0.7071

let right_pressed : bool ref = ref false
let left_pressed  : bool ref = ref false
let up_pressed    : bool ref = ref false
let down_pressed  : bool ref = ref false

let get_mag ms =
  Int.to_float ms |> ( *. ) speed

let poll_horizontal_dir () =
  match !right_pressed, !left_pressed with
  | true , false -> 1
  | false, true -> -1
  | _    , _ -> 0

let poll_vertical_dir () =
  match !up_pressed, !down_pressed with
  | true , false -> -1
  | false, true -> 1
  | _    , _ -> 0

let poll_dirs () =
  let horiz = poll_horizontal_dir () in
  let verti = poll_vertical_dir () in
  match horiz, verti with
  | 0,  0  -> (0., 0.)
  | 0,  vn -> (0., (Int.to_float vn))
  | hn, 0  -> ((Int.to_float hn), 0.)
  | hn, vn -> (
    ((Int.to_float hn) *. diag_vec),
    ((Int.to_float vn) *. diag_vec))

let rec update ms = function
  | ({ transform_cmp = Some trans
     ; keyin_cmp     = Some _keyint; _} : Entity.t) :: tail ->
    let (velx, vely) = poll_dirs () in
    let mag = get_mag ms in
    trans.x <- trans.x + (Int.of_float (velx *. mag));
    trans.y <- trans.y + (Int.of_float (vely *. mag));
    update ms tail
  | _ :: tail -> update ms tail
  | [] -> ()

let handle_event = function
| Sdl.Event.SDL_KEYUP {scancode = W; _}
| Sdl.Event.SDL_KEYUP {scancode = UP; _} ->
  up_pressed    := false
| Sdl.Event.SDL_KEYUP {scancode = S; _}
| Sdl.Event.SDL_KEYUP {scancode = DOWN; _} ->
  down_pressed  := false
| Sdl.Event.SDL_KEYUP {scancode = D; _}
| Sdl.Event.SDL_KEYUP {scancode = RIGHT; _} ->
  right_pressed := false
| Sdl.Event.SDL_KEYUP {scancode = A; _}
| Sdl.Event.SDL_KEYUP {scancode = LEFT; _} ->
  left_pressed  := false
| Sdl.Event.SDL_KEYDOWN {scancode = W; _}
| Sdl.Event.SDL_KEYDOWN {scancode = UP; _} ->
  up_pressed    := true
| Sdl.Event.SDL_KEYDOWN {scancode = S; _}
| Sdl.Event.SDL_KEYDOWN {scancode = DOWN; _} ->
  down_pressed  := true
| Sdl.Event.SDL_KEYDOWN {scancode = D; _}
| Sdl.Event.SDL_KEYDOWN {scancode = RIGHT; _} ->
  right_pressed := true
| Sdl.Event.SDL_KEYDOWN {scancode = A; _}
| Sdl.Event.SDL_KEYDOWN {scancode = LEFT; _} ->
  left_pressed  := true
| Sdl.Event.SDL_MOUSEBUTTONDOWN (_e : Sdl.MouseButtonEvent.t) -> ()
  (*if Int.equal e.mb_button 1 then left_pressed := true else
  if Int.equal e.mb_button 3 then right_pressed := true*)
| _ -> ()

