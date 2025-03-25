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
    let tex = Utils.Texture2D.of_png rdr ~filename in
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
    { id                    : int
    ; mutable sprite_cmp    : Sprite.t    option
    ; mutable transform_cmp : Transform.t option
    ; mutable keyin_cmp     : KeyIn.t     option }

  let data : t list ref = ref []
  let get id = List.find (fun ent -> ent.id = id) !data

  let create () =
    let id = !max in
    max := !max + 1;
    data :=
      { id            = id
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
      sprite.dst_rect <- Sdl.Rect.make ~x:trans.pos_x ~y:trans.pos_y ~w:100 ~h:100;
      update tail
    | _ :: tail -> update tail
    | [] -> ()

  let rec render rdr = function
    | ({sprite_cmp = Some sprite; _} : Entity.t) :: tail ->
      Sdl.render_copy rdr
        ~srcrect:None
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
  let up_pressed    : bool ref = ref false
  let down_pressed  : bool ref = ref false

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

  let rec update = function
    | ({ transform_cmp = Some trans
       ; keyin_cmp     = Some _keyint; _} : Entity.t) :: tail ->
      trans.vel_x <- poll_horizontal_dir ();
      trans.vel_y <- poll_vertical_dir ();
      update tail
    | _ :: tail -> update tail
    | [] ->
      right_pressed := false;
      left_pressed  := false;
      up_pressed    := false;
      down_pressed  := false

  let handle_event = function
  | Sdl.Event.SDL_KEYDOWN {scancode = UP; _} ->
    up_pressed    := true
  | Sdl.Event.SDL_KEYDOWN {scancode = DOWN; _} ->
    down_pressed  := true
  | Sdl.Event.SDL_KEYDOWN {scancode = RIGHT; _} ->
    right_pressed := true
  | Sdl.Event.SDL_KEYDOWN {scancode = LEFT; _} ->
    left_pressed  := true
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

  let quit : bool ref = ref false

  let handle_event = function
  | Sdl.Event.SDL_QUIT _ -> quit := true
  | Sdl.Event.SDL_KEYDOWN {scancode = ESCAPE; _} ->
    quit := true
  | _ -> ()

  let update entities =
    TransformSystem.update entities;
    MovementSystem.update entities;
    SpriteSystem.update entities

  let render entities rdr =
    Sdl.render_clear rdr;
    SpriteSystem.render rdr entities;
    Sdl.render_present rdr

  let rec loop entities rdr = function
  | true -> print_endline "quit"
  | false ->
    Events.poll ();
    update entities;
    render entities rdr;
    loop entities rdr !quit

  let main () =
    let w = 800
    and h = 600 in
    let (win, rdr) = Utils.Screen.init ~w ~h in

    Events.add handle_event;
    Events.add MovementSystem.handle_event;

    let bird = Entity.create () in
    Entity.add_sprite bird (Sprite.make ~imgname:"mine.png" rdr);
    Entity.add_transform bird (Transform.make ~px:10 ~py:10 ~vx:0 ~vy:0);
    Entity.add_keyin bird (KeyIn.make ());
    loop !Entity.data rdr !quit;
    Utils.Screen.destroy (win, rdr)

end
