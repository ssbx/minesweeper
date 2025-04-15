type t = {
  mutable x : int;
  mutable y : int;
  mutable orient : float;
  mutable scale : float;
  mutable parent : t option;
  mutable childs : t list;
}

let make ~x ~y ?(orient = 0.) ?(scale = 1.) () =
  { x; y; orient; scale; parent = None; childs = [] }
