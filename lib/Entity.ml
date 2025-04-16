open Core

let max : int ref = ref 0

type t = {
  id : int;
  mutable sprite_cmp : Sprite.t option;
  mutable transform_cmp : Transform.t option;
  mutable keyin_cmp : KeyIn.t option;
  mutable color_rect : ColorRect.t option;
  mutable mousein_cmp : MouseIn.t option;
  mutable physics_cmp : Physics.t option;
  mutable physics_cmp2 : Physics2.t option;
}

let data : t list ref = ref []
let get id = List.find_exn ~f:(fun ent -> ent.id = id) !data

let create () =
  let id = !max in
  max := !max + 1;
  data :=
    {
      id;
      sprite_cmp = None;
      transform_cmp = None;
      keyin_cmp = None;
      mousein_cmp = None;
      physics_cmp = None;
      physics_cmp2 = None;
      color_rect = None;
    }
    :: !data;
  id

let add_sprite id (sprite : Sprite.t) =
  let e = get id in
  e.sprite_cmp <- Some sprite

let add_transform id (trans : Transform.t) =
  let e = get id in
  e.transform_cmp <- Some trans

let add_keyin id (keyin : KeyIn.t) =
  let e = get id in
  e.keyin_cmp <- Some keyin

let add_color_rect id (color_rect : ColorRect.t) =
  let e = get id in
  e.color_rect <- Some color_rect

let add_mousein id (mousein : MouseIn.t) =
  let e = get id in
  e.mousein_cmp <- Some mousein

let add_physics id (phy : Physics.t) =
  let e = get id in
  e.physics_cmp <- Some phy

let add_physics2 id (phy : Physics2.t) =
  let e = get id in
  e.physics_cmp2 <- Some phy
