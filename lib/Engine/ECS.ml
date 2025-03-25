open CamlSDL2

(*****************************************************************************
 **** COMPONENTS *************************************************************
 *****************************************************************************)
module KeyIn = struct

  type t = bool

  let make () = (true : t)

end

module Sprite = struct

  type t =
    { mutable src_rect : Sdl.Rect.t
    ; mutable dst_rect : Sdl.Rect.t
    ; texture  : Sdl.Texture.t }

  let imgdir = List.nth AssetFiles.Sites.images 0
  let imgpath name = Filename.concat imgdir name


  let make ~imgname rdr =
    let filename = imgpath imgname in
    let tex = BackEnd.Texture2D.of_png rdr ~filename in
    { src_rect = Sdl.Rect.make ~x:10 ~y:10 ~w:10 ~h:10
    ; dst_rect = Sdl.Rect.make ~x:10 ~y:10 ~w:10 ~h:10
    ; texture  = tex }

end

module Transform = struct

  type t =
    { mutable pos_x : int
    ; mutable pos_y : int
    ; mutable vel_x : int
    ; mutable vel_y : int }

  let make ~px ~py ~vx ~vy =
    { pos_x = px
    ; pos_y = py
    ; vel_x = vx
    ; vel_y = vy }

end

(*****************************************************************************
 **** ENTITY *****************************************************************
 *****************************************************************************)
module Entity = struct
  let max : int ref = ref 0

  type t =
    { id            : int
    ; mutable sprite_cmp    : Sprite.t    option
    ; mutable transform_cmp : Transform.t option
    ; mutable keyin_cmp     : KeyIn.t     option }

  let data : t list ref = ref []
  let get id = List.find (fun ent -> ent.id = id) !data

  let create () =
    let id = !max in
    max := !max + 1;
    data :=
      { id            = !max
      ; sprite_cmp    = None
      ; transform_cmp = None
      ; keyin_cmp     = None } :: !data;
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

end


(*****************************************************************************
 **** SYSTEMS ****************************************************************
 *****************************************************************************)
module SpriteSystem = struct


  let rec update = function
    | ({ sprite_cmp    = Some sprite
       ; transform_cmp = Some trans
       ; _ } : Entity.t) :: tail ->
      sprite.dst_rect <- Sdl.Rect.make ~x:trans.pos_x ~y:trans.pos_y ~w:10 ~h:10;
      update tail
    | _ :: tail -> update tail
    | [] -> ()

  let rec render rdr = function
    | ({sprite_cmp = Some sprite; _} : Entity.t) :: tail ->
      Sdl.render_copy rdr
        ~srcrect:(Some sprite.src_rect)
        ~dstrect:(Some sprite.dst_rect)
        ~texture:sprite.texture;
      render rdr tail
    | _ :: tail -> render rdr tail
    | [] -> ()

end

module TransformSystem = struct

  let rec update = function
    | ({ transform_cmp = Some trans; _ } : Entity.t) :: tail ->
      trans.pos_x <- trans.pos_x + trans.vel_x;
      trans.pos_y <- trans.pos_y + trans.vel_y;
      update tail
    | _ :: tail -> update tail
    | [] -> ()

end

module MovementSystem = struct

  let right_pressed : bool ref = ref false
  let left_pressed  : bool ref = ref false

  let poll_direction () =
    let dir =
      match !right_pressed, !left_pressed with
      | true , false -> 1
      | false, true -> -1
      | _    , _ -> 0
    in
    right_pressed := false;
    left_pressed := false;
    dir


  let rec update = function
    | ({ transform_cmp = Some trans
       ; keyin_cmp     = Some _keyint; _} : Entity.t) :: tail ->
      trans.vel_x <- poll_direction ();
      update tail
    | _ :: tail -> update tail
    | [] -> ()

  let handle_event = function
  | Sdl.Event.SDL_MOUSEBUTTONDOWN (e : Sdl.MouseButtonEvent.t) ->
    if e.mb_button = 1 then left_pressed := true else
    if e.mb_button = 3 then right_pressed := true
  | _ -> ()



end

(*****************************************************************************
 **** GAME *******************************************************************
 *****************************************************************************)

module Events = struct

  let callbacks : (Sdl.Event.t -> unit) list ref = ref []

  let add cb  = callbacks := cb :: !callbacks

  let rec poll () =
    match Sdl.poll_event () with
    | None -> ()
    | Some evt ->
      List.iter (fun cb -> cb evt) !callbacks;
      poll ()

end

module Game = struct

  let update entities =
    TransformSystem.update entities;
    MovementSystem.update entities;
    SpriteSystem.update entities

  let render entities rdr =
    Sdl.render_clear rdr;
    SpriteSystem.render rdr entities;
    Sdl.render_present rdr

  let rec loop entities rdr = function
  | 0 -> print_endline "quit"
  | n ->
    Events.poll ();
    update entities;
    render entities rdr;
    loop entities rdr (n - 1)

  let main rdr =
    Events.add MovementSystem.handle_event;

    let bird = Entity.create () in
    Entity.add_sprite bird (Sprite.make ~imgname:"mine.png" rdr);
    Entity.add_transform bird (Transform.make ~px:10 ~py:10 ~vx:0 ~vy:0);
    Entity.add_keyin bird (KeyIn.make ());
    let num_iters = 100 in
    loop !Entity.data rdr num_iters

end
