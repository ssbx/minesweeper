open CamlSDL2
open Gamekit
open Core

let imgdir = Option.value_exn (List.nth AssetFiles.Sites.images 0)
let imgpath name = Filename.concat imgdir name

module Board = struct

  let grid_size = 30
  let border = 20
  let orig_x : int ref = ref 0
  let orig_y : int ref = ref 0

  let randcol () = Random.int 100

  let rec gen_cells ~left ~top ~h ~border = function
  | 0 -> ()
  | n ->
    let crect = ColorRect.make ~w:(h - border) ~h:(h - border)
      ~r:(randcol ())
      ~g:(randcol ())
      ~b:(140 + (randcol ()))
      ~a:255 in
    let trans = Transform.make ~scale:1. ~x:left ~y:top () in
    let id = Entity.create () in
    Entity.add_transform id trans;
    Entity.add_color_rect id crect;
    gen_cells ~left:(left + h) ~top ~h ~border (n - 1)

  let rec gen_rows ~left ~top ~h ~border ~w ~ncols = function
  | 0 -> ()
  | n ->
    gen_cells ~left ~top ~h ~border ncols;
    gen_rows ~left ~top:(top + h) ~h ~border ~w ~ncols (n - 1)

  let init w h =
    let border  = 2 in
    let board_w = w - (2 * border) in
    let board_h = h - (2 * border) in
    let center_x = w / 2 in
    let center_y = h / 2 in

    let board_h' = board_h - ( ( mod ) board_h grid_size ) in
    let cell_w   = board_h / grid_size in
    let ncols = board_w / cell_w in
    let board_w' = board_w - ( ( mod ) board_w ncols ) in

(*
    let cmp_colrect =  ColorRect.make
      ~w:(board_w' + border) ~h:(board_h' + border)
      ~r:0 ~g:0 ~b:155 ~a:0 in
    let cmp_trans   = Transform.make
      ~scale:1.
      ~x:center_x ~y:center_y () in

    let board = Entity.create () in
    Entity.add_transform board cmp_trans;
    Entity.add_color_rect board cmp_colrect;
*)

    orig_x := center_x - (board_w' / 2);
    orig_y := center_y - (board_h' / 2);
    gen_rows
      ~left:(!orig_x + cell_w / 2)
      ~top:(!orig_y + cell_w / 2)
      ~h:cell_w ~border ~w:(board_w - border) ~ncols grid_size

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
    MouseOverSystem.update ms entities;
    ColorRectSystem.update entities;
    SpriteSystem.update entities

  let render entities rdr =
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


  let main () =
    Random.self_init ();
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

    (* board *)
    Board.init w h;


    (* grab vivant *)
    let spritex = imgpath "mine.png" in
    let cmp_spritex = Sprite.make ~filename:spritex rdr in
    let cmp_transx = Transform.make ~scale:0.1 ~x:(w / 2) ~y:(h / 2) () in
    let cmp_mouseinx = MouseIn.make () in

    let grab = Entity.create () in
    Entity.add_sprite grab cmp_spritex;
    Entity.add_transform grab cmp_transx;
    Entity.add_mousein grab cmp_mouseinx;

    ticks_elapsed := Sdl.get_ticks ();
    loop !Entity.data rdr !quit;

    Utils.Screen.destroy (win, rdr)

end


