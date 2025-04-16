open CamlSDL2
open Core
open Gamekit
open Chipmunk

let imgdir = Option.value_exn (List.nth AssetFiles.Sites.images 0)
let imgpath name = Filename.concat imgdir name

let make_ball ~x ~y =
  let body = Cp.body_new 1. Float.infinity in
  Cp.body_set_position body (Cp.Vect.make ~x ~y);
  let shape = Cp.circle_shape_new body 0.95 Cp.Vect.zero in
  Cp.shape_set_elasticity shape 0.;
  Cp.shape_set_friction shape 0.;
  shape

let init_phy_1 () =
  let space = Cp.space_new () in
  Cp.space_set_iterations space 1;
  Cp.space_use_spatial_hash space 2. 10000;
  space

let init_phy_2 space =
  let body = Cp.body_new 1000000000. Float.infinity in
  Cp.space_add_body space body;
  Cp.body_set_position body (Cp.Vect.make ~x:(-1000.) ~y:(-10.));
  Cp.body_set_velocity body (Cp.Vect.make ~x:(400.) ~y:(0.));

  let shape = Cp.circle_shape_new body 8. Cp.Vect.zero in
  Cp.space_add_shape space shape;
  Cp.shape_set_elasticity shape 0.;
  Cp.shape_set_friction shape 0.;
  Cp.shape_set_filter shape Cp.ShapeFilter.not_grabbable_filter;
  body

let free_phy space = 
  Cp.free_all_space_children space;
  Cp.space_free space

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
    | Sdl.Event.SDL_KEYDOWN { scancode = ESCAPE; _ } -> quit := true
    | _ -> ()

  let update ms entities space =
    PhysicsSystem2.update ms entities space;
    TransformSystem.update entities;
    MovementSystem.update ms entities;
    MouseInSystem.update ms entities;
    ColorRectSystem.update entities;
    SpriteSystem.update entities

  let render entities rdr =
    Sdl.set_render_draw_color rdr ~r:100 ~g:100 ~b:100 ~a:255;
    Sdl.render_clear rdr;
    ColorRectSystem.render rdr entities;
    SpriteSystem.render rdr entities;
    Sdl.render_present rdr

  let rec loop entities rdr space = function
    | true -> print_endline "quit"
    | false ->
        Events.poll ();
        let ms = get_step () in
        update ms entities space;
        render entities rdr;
        loop entities rdr space !quit

  let main () =
    let scr_w = 1280 and scr_h = 720 in
    let win, rdr = Utils.Screen.init ~w:scr_w ~h:scr_h in
    (*Sdl.render_set_logical_size rdr ~width:w ~height:h;*)
    Sdl.show_cursor ~toggle:false;
    Events.add handle_event;
    Events.add MovementSystem.handle_event;
    Events.add MouseInSystem.handle_event;
    let _mh = scr_h / 2 in

    let space = init_phy_1 () in

    let width, height = 10, 10 in
    let width_f, height_f = (Int.to_float width), (Int.to_float height) in
    let spritey_path = imgpath "circle.png" in
    let cmp_sprite = Sprite.make ~filename:spritey_path rdr in

    for x = 1 to width do
      for y = 1 to height do
        let x_jitter = 0.05 *. (Random.float 1.) in
        let y_jitter = 0.05 *. (Random.float 1.) in
        let x_f = Int.to_float(x) in
        let y_f = Int.to_float(y) in
        let shape = make_ball
            ~x:(2. *. (x_f -. width_f /. 2. +. x_jitter))
            ~y:(2. *. (height_f /. 2. -. y_f +. y_jitter)) in
        let body = Cp.shape_get_body shape in
        Cp.space_add_body space body;
        Cp.space_add_shape space shape;


        let cmp_physics2 = body in
        let pos = Cp.body_get_position body in
        let x2 = pos.x *. 15. in
        let y2 = pos.y *. 15. in
        let cmp_trans = Transform.make 
          ~x:(Int.of_float x2)
          ~y:(Int.of_float y2) () in

        let ent = Entity.create () in
        Entity.add_sprite ent cmp_sprite;
        Entity.add_physics2 ent cmp_physics2;
        Entity.add_transform ent cmp_trans
      done
    done;

    let projectile = init_phy_2 space in
    let ent = Entity.create () in
    let pos = Cp.body_get_position projectile in
    let x2 = pos.x *. 15. in
    let y2 = pos.y *. 15. in
    let cmp_trans = Transform.make 
          ~x:(Int.of_float x2)
          ~y:(Int.of_float y2) () in
    Entity.add_sprite ent cmp_sprite;
    Entity.add_physics2 ent projectile;
    Entity.add_transform ent cmp_trans;

    ticks_elapsed := Sdl.get_ticks ();

    loop !Entity.data rdr space !quit;

    free_phy space;

    Utils.Screen.destroy (win, rdr)
end
