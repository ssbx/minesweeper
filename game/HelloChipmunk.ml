open CamlSDL2
open Core
open Gamekit
open Chipmunk

let imgdir = Option.value_exn (List.nth AssetFiles.Sites.images 0)
let imgpath name = Filename.concat imgdir name

let init_phy () =
  let space = Cp.space_new () in
  print_endline "Cp.spaceNew success";
  let gravity = Cp.Vect.make ~x:0. ~y:(-100.) in
  Cp.space_set_gravity space gravity;
  print_endline "Cp.spaceSetGravity success";
  let body = Cp.space_get_static_body space in
  print_endline "Cp.spaceGetStaticBody success";
  let va = Cp.Vect.make ~x:(-20.) ~y:5.
  and vb = Cp.Vect.make ~x:20. ~y:(-5.) in
  let ground = Cp.segment_shape_new body va vb 0. in
  print_endline "Cp.segmentShapeNew success";
  Cp.shape_set_friction ground 1.;
  print_endline "Cp.shapeSetFriction success";
  Cp.space_add_shape space ground;
  print_endline "Cp.spaceAddShape success";
  let radius = 5.
  and mass = 1. in
  let moment = Cp.moment_for_circle mass 0. radius Cp.Vect.zero in
  print_endline "Cp.momentForCircle success";
  Printf.printf "moment is %f\n" moment;
  let ball_body = Cp.body_new mass moment in
  print_endline "Cp.bodyNew success";
  Cp.space_add_body space ball_body;
  print_endline "Cp.spaceAddBody success";
  let position = Cp.Vect.make ~x:0. ~y:15. in
  Cp.body_set_position ball_body position;
  print_endline "Cp.bodySetPosition success";
  let ball_shape = Cp.circle_shape_new ball_body radius Cp.Vect.zero in
  print_endline "Cp.circleShapeNew success";
  Cp.space_add_shape space ball_shape;
  print_endline "Cp.spaceAddShape success";
  Cp.shape_set_friction ball_shape 0.7;
  print_endline "Cp.shapeSetFriction success";
  let timestep = 1. /. 144. in
  timestep, ball_shape, ball_body, ground, space
;;

let free_phy (_timestep, ball_shape, ball_body, ground, space) =
  Cp.shape_free ball_shape;
  print_endline "Cp.shapeFree success";
  Cp.body_free ball_body;
  print_endline "Cp.bodyFree success";
  Cp.shape_free ground;
  print_endline "Cp.shapeFree success";
  Cp.space_free space;
  print_endline "Cp.spaceFree success"
;;

module Game = struct
  let ticks_elapsed : int ref = ref 0
  let quit : bool ref = ref false

  let get_step () =
    let ticks_current = Sdl.get_ticks () in
    let step = ticks_current - !ticks_elapsed in
    ticks_elapsed := ticks_current;
    step
  ;;

  let handle_event = function
    | Sdl.Event.SDL_QUIT _ -> quit := true
    | Sdl.Event.SDL_KEYDOWN { scancode = ESCAPE; _ } -> quit := true
    | _ -> ()
  ;;

  let update ms entities phy =
    PhysicsSystem.update ms phy entities;
    TransformSystem.update entities;
    MovementSystem.update ms entities;
    MouseInSystem.update ms entities;
    ColorRectSystem.update entities;
    SpriteSystem.update entities
  ;;

  let render entities rdr =
    Sdl.set_render_draw_color rdr ~r:100 ~g:100 ~b:100 ~a:255;
    Sdl.render_clear rdr;
    ColorRectSystem.render rdr entities;
    SpriteSystem.render rdr entities;
    Sdl.render_present rdr
  ;;

  let rec loop entities rdr phy = function
    | true -> print_endline "quit"
    | false ->
      Events.poll ();
      let ms = get_step () in
      update ms entities phy;
      render entities rdr;
      loop entities rdr phy !quit
  ;;

  let main () =
    let scr_w = 1280
    and scr_h = 720 in
    let middle_w = 1280 / 2 in
    let win, rdr = Utils.Screen.init ~w:scr_w ~h:scr_h in
    (*Sdl.render_set_logical_size rdr ~width:w ~height:h;*)
    Sdl.show_cursor ~toggle:false;
    let phydata = init_phy () in
    Events.add handle_event;
    Events.add MovementSystem.handle_event;
    Events.add MouseInSystem.handle_event;
    let _mh = scr_h / 2 in
    let spritey_path = imgpath "circle.png" in
    let cmp_spritey = Sprite.make ~filename:spritey_path rdr in
    let cmp_transy = Transform.make ~scale:1. ~x:middle_w ~y:150 () in
    let cmp_physic = Physics.make ~x:middle_w ~y:150 in
    let enemy1 = Entity.create () in
    Entity.add_sprite enemy1 cmp_spritey;
    Entity.add_transform enemy1 cmp_transy;
    Entity.add_physics enemy1 cmp_physic;
    ticks_elapsed := Sdl.get_ticks ();
    loop !Entity.data rdr phydata !quit;
    free_phy phydata;
    Sprite.destroy cmp_spritey;
    Utils.Screen.destroy (win, rdr)
  ;;
end
