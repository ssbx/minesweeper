open CamlSDL2
open Gamekit
open Core

let imgdir = Option.value_exn (List.nth AssetFiles.Sites.images 0)
let imgpath name = Filename.concat imgdir name


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
    let sprite_path = imgpath "ship9_x2.png" in
    let cmp_sprite = Sprite.make ~filename:sprite_path rdr in
    let cmp_trans = Transform.make ~scale:1. ~x:(w / 2) ~y:(h / 2) () in
    let cmp_keyin = KeyIn.make () in

    let vessel = Entity.create () in
    Entity.add_sprite vessel cmp_sprite;
    Entity.add_transform vessel cmp_trans;
    Entity.add_keyin vessel cmp_keyin;

    (* grab vivant *)
    let spritex_path = imgpath "mine.png" in
    let cmp_spritex = Sprite.make ~filename:spritex_path rdr in
    let cmp_transx = Transform.make ~scale:0.1 ~x:(w / 2) ~y:(h / 2) () in
    let cmp_mouseinx = MouseIn.make () in

    let grab = Entity.create () in
    Entity.add_sprite grab cmp_spritex;
    Entity.add_transform grab cmp_transx;
    Entity.add_mousein grab cmp_mouseinx;

    (* enemi 1 *)
    let spritey_path = imgpath "ship2_x2.png" in
    let cmp_spritey = Sprite.make ~filename:spritey_path rdr in
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

