
type t =
  { o : Object.t
  ; w : float
  ; h : float }


let objects = Hashtbl.create 100

let make ~w ~h ~o =
  ({ o = o
   ; w = w
   ; h = h } : t)

let extend ?(w=10.) ?(h=10.) (o : Object.t) =
  Hashtbl.add objects o.hash (make ~w ~h ~o)
