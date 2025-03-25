type t =
  { x : float
  ; y : float }

let empty () = ({x = 0.; y = 0.} : t)
let make ~x ~y = ({x=x; y=y} : t)

