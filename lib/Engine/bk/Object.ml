type t =
  { transform : Transform.t
  ; active    : bool
  ; layer     : int
  ; hash      : string }

let empty () =
  ({ transform = Transform.empty ()
   ; active    = true
   ; layer     = 1
   ; hash      = "hello " } : t)
