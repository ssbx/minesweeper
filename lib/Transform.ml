
type t =
  { mutable x : int
  ; mutable y : int
  ; mutable orient : float
  ; mutable scale : float
  ; mutable parent : t option
  ; mutable childs : t list}

let make ~x ~y ?(orient=0.) ?(scale=1.) () =
  { x = x
  ; y = y
  ; orient = orient
  ; scale = scale
  ; parent = None
  ; childs = [] }
