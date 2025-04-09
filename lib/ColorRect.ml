open CamlSDL2

type t =
  { mutable src_rect : Sdl.Rect.t
  ; mutable dst_rect : Sdl.Rect.t
  ; mutable r : int
  ; mutable g : int
  ; mutable b : int
  ; mutable a : int
  }

let make ~w ~h ~r ~g ~b ~a =
  { src_rect = Sdl.Rect.make ~x:0 ~y:0 ~w ~h
  ; dst_rect = Sdl.Rect.make ~x:0 ~y:0 ~w ~h
  ; r
  ; g
  ; b
  ; a
  }
;;
