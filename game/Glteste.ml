open CamlSDL2
open Gamekit
open Core


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

  let update _ms _entities = ()
    (*TransformSystem.update entities;
    MovementSystem.update ms entities;
    MouseInSystem.update ms entities;
    MouseOverSystem.update ms entities;
    ColorRectSystem.update entities;
    SpriteSystem.update entities*)

  let render _entities _rdr = ()
  (*
    Sdl.set_render_draw_color rdr ~r:40 ~g:40 ~b:255 ~a:255;
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
        *)

  let main () =
    Random.self_init ();
    let scr_w = 1280 and scr_h = 720 in
    let _w = 1280 and _h = 720 in
    let win, ctx = Utils.Screen2.init ~w:scr_w ~h:scr_h in
    (* TODO: voir https://sibras.github.io/OpenGL4-Tutorials/docs/Tutorials/01-Tutorial1/*)
    (*Sdl.render_set_logical_size rdr ~width:w ~height:h;
    Sdl.show_cursor ~toggle:false;
    Events.add handle_event;
    Events.add MovementSystem.handle_event;
    Events.add MouseInSystem.handle_event;
    (* board *)
    Board.init w h;*)
    (* grab vivant *)
    (*let spritex = imgpath "mine.png" in
    let cmp_spritex = Sprite.make ~filename:spritex rdr in
    let cmp_transx = Transform.make ~scale:0.1 ~x:(w / 2) ~y:(h / 2) () in
    let cmp_mouseinx = MouseIn.make () in
    let grab = Entity.create () in
    Entity.add_sprite grab cmp_spritex;
    Entity.add_transform grab cmp_transx;
    Entity.add_mousein grab cmp_mouseinx;
    ticks_elapsed := Sdl.get_ticks ();
    loop !Entity.data rdr !quit;
    *)
    Utils.Screen2.destroy (win, ctx)
end
