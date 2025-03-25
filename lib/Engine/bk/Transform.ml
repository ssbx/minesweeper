type t =
  { parent    : t option
  ; childs    : t list
  ; scale     : float
  ; position  : Vector2.t }

let empty () =
  ({ parent = None
   ; childs = []
   ; scale = 1.0
   ; position = Vector2.empty () } : t)
