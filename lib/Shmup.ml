open CamlSDL2
open Core

(*****************************************************************************
 **** COMPONENTS *************************************************************
 *****************************************************************************)
module KeyIn = struct

  type t = bool

  let make () = (true : t)

end

module MouseIn = struct

  type t = bool

  let make () = (true : t)

end

module Sprite = struct

  type t =
    { mutable src_rect : Sdl.Rect.t
    ; mutable dst_rect : Sdl.Rect.t
    ; mutable w : int
    ; mutable h : int
    ; texture  : Sdl.Texture.t }

  let imgdir = Option.value_exn (List.nth AssetFiles.Sites.images 0)
  let imgpath name = Filename.concat imgdir name

  let make ~imgname rdr =
    let filename = imgpath imgname in
    let tex = Utils.Texture2D.of_png rdr ~filename in
    let (_, _, w, h) = Sdl.query_texture tex in
    { src_rect = Sdl.Rect.make ~x:0 ~y:0 ~w ~h
    ; dst_rect = Sdl.Rect.make ~x:0 ~y:0 ~w ~h
    ; w = w; h = h
    ; texture  = tex }

  let destroy sprite = Sdl.destroy_texture sprite.texture

end

module ColorRect = struct
  type t =
    { mutable src_rect : Sdl.Rect.t
    ; mutable dst_rect : Sdl.Rect.t
    ; r : int
    ; g : int
    ; b : int
    ; a : int }

  let make ~w ~h ~r ~g ~b ~a =
    { src_rect = Sdl.Rect.make ~x:0 ~y:0 ~w ~h
    ; dst_rect = Sdl.Rect.make ~x:0 ~y:0 ~w ~h
    ; r = r
    ; g = g
    ; b = b
    ; a = a }

end

module Transform = struct

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
    ; mutable keyin_cmp     : KeyIn.t     option
    ; mutable color_rect    : ColorRect.t option
    ; mutable mousein_cmp   : MouseIn.t option }

  let data : t list ref = ref []
  let get id = List.find_exn ~f:(fun ent -> ent.id = id) !data

  let create () =
    let id = !max in
    max := !max + 1;
    data :=
      { id            = id
      ; sprite_cmp    = None
      ; transform_cmp = None
      ; keyin_cmp     = None
      ; mousein_cmp   = None
      ; color_rect    = None } :: !data;
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



end


(*****************************************************************************
 **** SYSTEMS ****************************************************************
 *****************************************************************************)
let scale i f = Int.to_float i |> ( *. ) f |> Int.of_float

let print_rect str (r : Sdl.Rect.t) =
  Printf.printf "%s x:%i y:%i w:%i h:%i\n" str r.x r.y r.w r.h

module ColorRectSystem = struct

  let rec update = function
    | ({ color_rect    = Some crect
       ; transform_cmp = Some trans
       ; _ } : Entity.t) :: tail ->
      let scaled_w = scale crect.src_rect.w trans.scale
      and scaled_h = scale crect.src_rect.h trans.scale in
      crect.dst_rect <- Sdl.Rect.make
        ~x:(trans.x - (scaled_w / 2))
        ~y:(trans.y - (scaled_h / 2))
        ~w:scaled_w
        ~h:scaled_h;
      update tail
    | _ :: tail -> update tail
    | [] -> ()

  let rec render rdr = function
    | ({color_rect = Some crect; _} : Entity.t) :: tail ->
      Sdl.set_render_draw_color rdr
        ~r:crect.r
        ~g:crect.g
        ~b:crect.b
        ~a:crect.a;
      Sdl.render_fill_rect rdr crect.dst_rect;
      render rdr tail
    | _ :: tail -> render rdr tail
    | [] -> ()


end

module SpriteSystem = struct

  let rec update = function
    | ({ sprite_cmp    = Some sprite
       ; transform_cmp = Some trans
       ; _ } : Entity.t) :: tail ->
      let scaled_w = scale sprite.src_rect.w trans.scale
      and scaled_h = scale sprite.src_rect.h trans.scale in
      sprite.dst_rect <- Sdl.Rect.make
        ~x:(trans.x - (scaled_w / 2))
        ~y:(trans.y - (scaled_h / 2))
        ~w:scaled_w
        ~h:scaled_h;
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
    | ({ transform_cmp = Some _trans; _ } : Entity.t) :: tail ->
      update tail
    | _ :: tail -> update tail
    | [] -> ()

end

module MovementSystem = struct

  let speed = 0.5
  let diag_vec = 0.7071

  let right_pressed : bool ref = ref false
  let left_pressed  : bool ref = ref false
  let up_pressed    : bool ref = ref false
  let down_pressed  : bool ref = ref false

  let get_mag ms =
    Int.to_float ms |> ( *. ) speed

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

  let poll_dirs () =
    let horiz = poll_horizontal_dir () in
    let verti = poll_vertical_dir () in
    match horiz, verti with
    | 0,  0  -> (0., 0.)
    | 0,  vn -> (0., (Int.to_float vn))
    | hn, 0  -> ((Int.to_float hn), 0.)
    | hn, vn -> (
      ((Int.to_float hn) *. diag_vec),
      ((Int.to_float vn) *. diag_vec))

  let rec update ms = function
    | ({ transform_cmp = Some trans
       ; keyin_cmp     = Some _keyint; _} : Entity.t) :: tail ->
      let (velx, vely) = poll_dirs () in
      let mag = get_mag ms in
      trans.x <- trans.x + (Int.of_float (velx *. mag));
      trans.y <- trans.y + (Int.of_float (vely *. mag));
      update ms tail
    | _ :: tail -> update ms tail
    | [] -> ()

  let handle_event = function
  | Sdl.Event.SDL_KEYUP {scancode = W; _}
  | Sdl.Event.SDL_KEYUP {scancode = UP; _} ->
    up_pressed    := false
  | Sdl.Event.SDL_KEYUP {scancode = S; _}
  | Sdl.Event.SDL_KEYUP {scancode = DOWN; _} ->
    down_pressed  := false
  | Sdl.Event.SDL_KEYUP {scancode = D; _}
  | Sdl.Event.SDL_KEYUP {scancode = RIGHT; _} ->
    right_pressed := false
  | Sdl.Event.SDL_KEYUP {scancode = A; _}
  | Sdl.Event.SDL_KEYUP {scancode = LEFT; _} ->
    left_pressed  := false
  | Sdl.Event.SDL_KEYDOWN {scancode = W; _}
  | Sdl.Event.SDL_KEYDOWN {scancode = UP; _} ->
    up_pressed    := true
  | Sdl.Event.SDL_KEYDOWN {scancode = S; _}
  | Sdl.Event.SDL_KEYDOWN {scancode = DOWN; _} ->
    down_pressed  := true
  | Sdl.Event.SDL_KEYDOWN {scancode = D; _}
  | Sdl.Event.SDL_KEYDOWN {scancode = RIGHT; _} ->
    right_pressed := true
  | Sdl.Event.SDL_KEYDOWN {scancode = A; _}
  | Sdl.Event.SDL_KEYDOWN {scancode = LEFT; _} ->
    left_pressed  := true
  | Sdl.Event.SDL_MOUSEBUTTONDOWN (_e : Sdl.MouseButtonEvent.t) -> ()
    (*if Int.equal e.mb_button 1 then left_pressed := true else
    if Int.equal e.mb_button 3 then right_pressed := true*)
  | _ -> ()

end

module MouseInSystem = struct

  let pos_x : int ref = ref 0
  let pos_y : int ref = ref 0
  let b1_active : bool ref = ref false
  let b2_active : bool ref = ref false

  let rec update ms = function
    | ({ transform_cmp = Some trans
       ; mousein_cmp   = Some _mousein; _} : Entity.t) :: tail ->
      trans.x <- !pos_x;
      trans.y <- !pos_y;
      update ms tail
    | _ :: tail -> update ms tail
    | [] -> ()

  let handle_event = function
  | Sdl.Event.SDL_MOUSEMOTION e ->
    pos_x := e.mm_x;
    pos_y := e.mm_y
  | Sdl.Event.SDL_MOUSEBUTTONDOWN {mb_button = 1; _} ->
    b1_active := true
  | Sdl.Event.SDL_MOUSEBUTTONDOWN {mb_button = 2; _} ->
    b2_active := true
  | Sdl.Event.SDL_MOUSEBUTTONUP {mb_button = 1; _} ->
    b1_active := false
  | Sdl.Event.SDL_MOUSEBUTTONUP {mb_button = 2; _} ->
    b2_active := false
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
      List.iter ~f:(fun cb -> cb evt) !callbacks;
      poll ()

end

module Game = struct

  let ticks_elapsed : int ref = ref 0
  let quit : bool ref = ref false

  let get_step () =
    let ticks_current = Sdl.get_ticks () in
    let step = ticks_current - !ticks_elapsed in
    ticks_elapsed := ticks_current;
    step

  let handle_event = function
  | Sdl.Event.SDL_QUIT _ -> quit := true
  | Sdl.Event.SDL_KEYDOWN {scancode = ESCAPE; _} ->
    quit := true
  | _ -> ()

  let update ms entities =
    TransformSystem.update entities;
    MovementSystem.update ms entities;
    MouseInSystem.update ms entities;
    ColorRectSystem.update entities;
    SpriteSystem.update entities

  let render entities rdr =
    Sdl.set_render_draw_color rdr ~r:0 ~g:100 ~b:150 ~a:255;
    Sdl.render_clear rdr;
    ColorRectSystem.render rdr entities;
    SpriteSystem.render rdr entities;
    Sdl.render_present rdr

  let rec loop entities rdr = function
  | true -> print_endline "quit"
  | false ->
    Events.poll ();
    let ms = get_step () in
    update ms entities;
    render entities rdr;
    loop entities rdr !quit

  let main () =
    let scr_w = 1280
    and scr_h = 720 in
    let w = 1280
    and h = 720 in
    let (win, rdr) = Utils.Screen.init ~w:scr_w ~h:scr_h in
    (*Sdl.render_set_logical_size rdr ~width:w ~height:h;*)
    Sdl.show_cursor ~toggle:false;

    Events.add handle_event;
    Events.add MovementSystem.handle_event;
    Events.add MouseInSystem.handle_event;

    (* vessel vivant *)
    let cmp_sprite = Sprite.make ~imgname:"ship9_x2.png" rdr in
    let cmp_trans = Transform.make ~scale:1. ~x:(w / 2) ~y:(h / 2) () in
    let cmp_keyin = KeyIn.make () in

    let vessel = Entity.create () in
    Entity.add_sprite vessel cmp_sprite;
    Entity.add_transform vessel cmp_trans;
    Entity.add_keyin vessel cmp_keyin;

    (* grab vivant *)
    let cmp_spritex = Sprite.make ~imgname:"mine.png" rdr in
    let cmp_transx = Transform.make ~scale:0.1 ~x:(w / 2) ~y:(h / 2) () in
    let cmp_mouseinx = MouseIn.make () in

    let grab = Entity.create () in
    Entity.add_sprite grab cmp_spritex;
    Entity.add_transform grab cmp_transx;
    Entity.add_mousein grab cmp_mouseinx;

    (* enemi 1 *)
    let cmp_spritey = Sprite.make ~imgname:"ship2_x2.png" rdr in
    let cmp_transy = Transform.make ~scale:1. ~x:120 ~y:120 () in

    let enemy1 = Entity.create () in
    Entity.add_sprite enemy1 cmp_spritey;
    Entity.add_transform enemy1 cmp_transy;


    ticks_elapsed := Sdl.get_ticks ();
    loop !Entity.data rdr !quit;
    Sprite.destroy cmp_sprite;
    Sprite.destroy cmp_spritex;
    Sprite.destroy cmp_spritey;

    Utils.Screen.destroy (win, rdr)

end
