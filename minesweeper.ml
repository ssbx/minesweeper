module GameLoop = struct
  type next_t = Continue | Exit

  type state_t = {
    mutable on_ready_fun : Sdl.Window.t -> Sdl.Renderer.t -> unit;
    mutable on_event_fun : Sdl.Event.t -> unit;
    mutable on_update_fun : unit -> next_t;
    mutable on_draw_fun : unit -> unit;
    mutable on_close_fun : unit -> unit;
    mutable _renderer : Sdl.Renderer.t option;
    mutable _window : Sdl.Window.t option;
  }

  let state =
    {
      on_ready_fun = (fun _ _ -> ());
      on_event_fun = (fun _ -> ());
      on_update_fun = (fun () -> Continue);
      on_draw_fun = (fun () -> ());
      on_close_fun = (fun () -> ());
      _renderer = None;
      _window = None;
    }

  let rec poll_events () =
    match Sdl.poll_event () with
    | None -> ()
    | Some evt ->
        state.on_event_fun evt;
        poll_events ()

  let rec loop _w r =
    poll_events ();
    match state.on_update_fun () with
    | Exit -> ()
    | Continue ->
        Sdl.render_clear r;
        state.on_draw_fun ();
        Sdl.render_present r;
        loop _w r

  let start ~on_ready ~on_event ~on_update ~on_draw ~on_close ~win_name =
    Sdl.init [ `SDL_INIT_VIDEO; `SDL_INIT_EVENTS; `SDL_INIT_TIMER ];
    Sdl.set_hint "SDL_RENDER_SCALE_QUALITY" "2";
    Sdl.set_hint "SDL_RENDER_VSYNC" "1";
    let window =
      Sdl.create_window ~title:win_name ~pos:(`centered, `centered)
        ~dims:(1240, 780)
        ~flags:[ Sdl.WindowFlags.Resizable; Sdl.WindowFlags.OpenGL ]
    in
    let renderer =
      Sdl.create_renderer ~win:window ~index:(-1)
        ~flags:
          [
            Sdl.RendererFlags.Accelerated;
            Sdl.RendererFlags.TargetTexture;
            Sdl.RendererFlags.PresentVSync;
          ]
    in
    state.on_ready_fun <- on_ready;
    state.on_event_fun <- on_event;
    state.on_update_fun <- on_update;
    state.on_draw_fun <- on_draw;
    state.on_close_fun <- on_close;
    state._window <- Some window;
    state._renderer <- Some renderer;
    state.on_ready_fun window renderer;
    loop window renderer;
    state.on_close_fun ();
    Sdl.destroy_window window
end

module Minesweeper = struct
  let ncells_x = 30
  let ncells_y = 30
  let nmines = ncells_y * ncells_x / 5
  let text_ratio = Float.of_int ncells_x /. Float.of_int ncells_y
  let bg_col_won = (0, 255, 0, 255)
  let bg_col = (100, 100, 100, 255)
  let cell_col = (215, 215, 215, 255)
  let shade_col = (175, 175, 175, 255)
  let cell_width = 72
  let cell_padding = 4
  let img_id_none = -1
  let img_id_mine = 0
  let img_id_flag = 9
  let img_id_exploded = 10
  let quit : bool ref = ref false
  let has_won : bool ref = ref false
  let boom : bool ref = ref false

  type exposed_t = bool
  type cell_content_t = Mine | Empty of exposed_t | Hint of (int * exposed_t)

  type mines_data_t = {
    _anim_disco_buffer : (int * int) array;
    mutable _anim_disco_buffer_id : int;
    mutable _anim_disco_buffer_end : int;
    mutable _anim_disco_running : bool;
    renderer : Sdl.Renderer.t;
    cells : cell_content_t array array;
    cells_flagged : bool array array;
    texture_board : Sdl.Texture.t;
    texture_shade : Sdl.Texture.t;
    texture_flags : Sdl.Texture.t;
    texture_images : Sdl.Texture.t list;
    mutable scr_width : int;
    mutable scr_height : int;
    mutable scr_x : int;
    mutable scr_y : int;
  }

  let mines_data : mines_data_t option ref = ref None
  let data () = match !mines_data with None -> failwith "noinit" | Some t -> t

  (*
   * Returns a list of tuple (x,y) neighbors of at_x at_y excluding out of bound
   * coordonaites and (at_x,at_y) himself
   *)
  let get_neighbors at_x at_y =
    (* create a (x,y) list of all neighbors, this include (at_x,at_y) for now...*)
    let c1 =
      List.fold_left
        (fun acc mx ->
          List.append [ (mx, at_y - 1); (mx, at_y); (mx, at_y + 1) ] acc)
        []
        [ at_x - 1; at_x; at_x + 1 ]
    in
    (* remove out of bounds access *)
    let c2 =
      List.filter
        (fun (nx, ny) ->
          (nx = -1 || ny = -1 || nx = ncells_x || ny = ncells_y) = false)
        c1
    in
    (* remove cell (at_x,at_y) itself *)
    List.filter (fun (nx, ny) -> (nx = at_x && ny = at_y) = false) c2

  let draw_cell_at x y ?(color = cell_col) ?(image = img_id_none) rdr =
    let x2 = (x * cell_width) + cell_padding
    and y2 = (y * cell_width) + cell_padding
    and w = cell_width - (cell_padding * 2)
    and r, g, b, a = color in
    let rect = Sdl.Rect.make4 ~x:x2 ~y:y2 ~w ~h:w in
    Sdl.set_render_draw_color rdr ~rgb:(r, g, b) ~a;
    Sdl.render_fill_rect rdr rect;
    match image = img_id_none with
    | true -> ()
    | false ->
        let img_texture = List.nth (data ()).texture_images image in
        Sdl.render_copy rdr ~texture:img_texture ~dst_rect:rect ()

  let gen_game_texture () =
    let d = data () in
    let text = d.texture_board and board = d.cells and rdr = d.renderer in
    let r, g, b, a = bg_col in
    Sdl.set_renderer_target rdr (Some text);
    Sdl.set_render_draw_color rdr ~rgb:(r, g, b) ~a;
    Sdl.render_clear rdr;
    Sdl.set_render_draw_color rdr ~rgb:(0, 0, 0) ~a:255;
    let iter_fun x y v =
      match v with
      | Hint (n, _) -> draw_cell_at x y ~image:n rdr
      | Mine -> draw_cell_at x y ~image:img_id_mine rdr
      | Empty _ -> draw_cell_at x y rdr
    in
    Array.iteri
      (fun x line -> Array.iteri (fun y cell -> iter_fun x y cell) line)
      board;
    Sdl.set_renderer_target rdr None

  let rec gen_game_loop coords board num_mines =
    match num_mines = 0 with
    | true -> gen_game_texture ()
    | false ->
        let rd = Random.int (List.length coords) in
        let mx, my = List.nth coords rd in
        board.(mx).(my) <- Mine;
        let neigh = get_neighbors mx my in
        List.iter
          (fun (x, y) ->
            match board.(x).(y) with
            | Mine -> ()
            | Empty _ -> board.(x).(y) <- Hint (1, false)
            | Hint (n, _) -> board.(x).(y) <- Hint (n + 1, false))
          neigh;
        let new_coords = List.filteri (fun i _ -> i != rd) coords in
        gen_game_loop new_coords board (num_mines - 1)

  let gen_game () =
    let board = (data ()).cells and flaggs = (data ()).cells_flagged in
    Array.iteri
      (fun x l -> Array.iteri (fun y _ -> board.(x).(y) <- Empty false) l)
      board;
    Array.iteri
      (fun x l -> Array.iteri (fun y _ -> flaggs.(x).(y) <- false) l)
      flaggs;
    let coords =
      List.flatten
        (List.init ncells_x (fun x -> List.init ncells_y (fun y -> (x, y))))
    in
    match List.length coords > nmines with
    | true -> gen_game_loop coords board nmines
    | false -> failwith "to many mines"

  let update_flags_texture () =
    let d = data () in
    let board = d.cells_flagged and rdr = d.renderer in
    Sdl.set_renderer_target rdr (Some d.texture_flags);
    Sdl.set_render_draw_color rdr ~rgb:(0, 0, 0) ~a:0;
    Sdl.render_clear rdr;
    let iter_fun x y v =
      match v with
      | true -> draw_cell_at x y ~color:(0, 0, 0, 0) ~image:img_id_flag rdr
      | false -> ()
    in
    Array.iteri
      (fun x line -> Array.iteri (fun y cell -> iter_fun x y cell) line)
      board;
    Sdl.set_renderer_target rdr None

  let update_shade_texture () =
    let d = data () in
    let board = d.cells and rdr = d.renderer in
    Sdl.set_renderer_target rdr (Some d.texture_shade);
    Sdl.set_render_draw_color rdr ~rgb:(0, 0, 0) ~a:0;
    Sdl.render_clear rdr;
    let iter_fun x y v =
      match v with
      | Hint (_, true) -> ()
      | Hint (_, false) -> draw_cell_at x y ~color:shade_col rdr
      | Empty true -> ()
      | Empty false -> draw_cell_at x y ~color:shade_col rdr
      | Mine -> draw_cell_at x y ~color:shade_col rdr
    in
    Array.iteri
      (fun x line -> Array.iteri (fun y cell -> iter_fun x y cell) line)
      board;
    Sdl.set_renderer_target rdr None

  let cell_from_cursor (e : Sdl.MouseButtonEvent.t) =
    let d = data () in
    let scr_w = d.scr_width
    and cur_x = e.mb_x - d.scr_x
    and cur_y = e.mb_y - d.scr_y in
    match cur_x < scr_w && cur_y < scr_w with
    | false -> None
    | true -> (
        let cw = scr_w / ncells_x in
        let cell_x = cur_x / cw in
        let cell_y = cur_y / cw in
        match cell_x < ncells_x && cell_y < ncells_y with
        | false ->
            Printf.printf
              "cursor scr:%i scr_cell:%i cursorx:%i cursory:%i cell_x:%i \
               cell_y:%i"
              scr_w cw cur_x cur_y cell_x cell_y;
            None
        | true -> Some (cell_x, cell_y))

  let _reset_anim_disco () =
    let d = data () in
    d._anim_disco_buffer_id <- 0;
    d._anim_disco_buffer_end <- 0;
    d._anim_disco_running <- false

  let _start_anim_disco () =
    let d = data () in
    d._anim_disco_buffer_id <- 0;
    d._anim_disco_running <- true

  let _add_anim_disco x y =
    let d = data () in
    d._anim_disco_buffer.(d._anim_disco_buffer_end) <- (x, y);
    d._anim_disco_buffer_end <- d._anim_disco_buffer_end + 1

  let _anim_disco () =
    let d = data () in
    let rdr = d.renderer in
    assert (d._anim_disco_running = true);
    let x, y = Array.get d._anim_disco_buffer d._anim_disco_buffer_id in
    Sdl.set_renderer_target rdr (Some d.texture_shade);
    draw_cell_at x y ~color:(0, 0, 255, 255) d.renderer;
    Sdl.set_renderer_target rdr None;
    d._anim_disco_buffer_id <- d._anim_disco_buffer_id + 1;
    match d._anim_disco_buffer_id > d._anim_disco_buffer_end with
    | true -> _reset_anim_disco ()
    | false -> ()

  let rec propagate_visible self_x self_y =
    let d = data () in
    let cells = d.cells and flags = d.cells_flagged in
    match cells.(self_x).(self_y) with
    | Mine -> assert false
    | Hint _ -> assert false
    | Empty true -> ()
    | Empty false ->
        cells.(self_x).(self_y) <- Empty true;
        flags.(self_x).(self_y) <- false;
        (*_add_anim_disco self_x self_y;*)
        let neg1 = get_neighbors self_x self_y in
        let neg2 =
          List.filter
            (fun (nx, ny) ->
              match cells.(nx).(ny) with
              | Mine -> false
              | Hint (_, true) -> false
              | Hint (n, false) ->
                  cells.(nx).(ny) <- Hint (n, true);
                  flags.(nx).(ny) <- false;
                  (*_add_anim_disco nx ny;*)
                  false
              | Empty true -> false
              | _ -> true)
            neg1
        in
        (*list_iter_rand (fun (nnx, nny) -> propagate_visible nnx nny) neg2*)
        List.iter (fun (nnx, nny) -> propagate_visible nnx nny) neg2

  let game_loosed bombs =
    let d = data () in
    let rdr = d.renderer in
    boom := true;
    Sdl.set_renderer_target rdr (Some d.texture_board);
    List.iter
      (fun (x, y) ->
        draw_cell_at x y ~color:(255, 0, 0, 255) ~image:img_id_exploded rdr)
      bombs;
    Sdl.set_renderer_target rdr None

  let check_winning_cond () =
    let d = data () in
    (* all but mines should be exposed *)
    let invis v =
      match v with Hint (_, false) -> true | Empty false -> true | _ -> false
    in
    let has_invisible_cells =
      Array.exists (fun l -> Array.exists (fun v -> invis v) l) d.cells
    in
    match has_invisible_cells with
    | true -> ()
    | false ->
        let all_flags_set =
          Array.for_all2
            (fun cell_l flags_l ->
              Array.for_all2
                (fun c f ->
                  match (c, f) with
                  | Mine, true -> true
                  | _, true -> false
                  | Mine, false -> false
                  | _, false -> true)
                cell_l flags_l)
            d.cells d.cells_flagged
        in
        if all_flags_set then has_won := true else ()

  let guess_from_visible_hint n x y =
    let d = data () in
    let neighs = get_neighbors x y in
    let num_flags =
      List.fold_left
        (fun acc (x, y) ->
          match d.cells_flagged.(x).(y) with true -> acc + 1 | false -> acc)
        0 neighs
    in
    match num_flags = n with
    | false -> ()
    | true -> (
        let no_flag_neighs =
          List.filter (fun (x, y) -> d.cells_flagged.(x).(y) = false) neighs
        in
        let boom =
          List.fold_left
            (fun acc (x, y) ->
              match d.cells.(x).(y) with
              | Mine -> List.append [ (x, y) ] acc
              | Empty true -> acc
              | Empty false ->
                  propagate_visible x y;
                  acc
              | Hint (_, true) -> acc
              | Hint (n, false) ->
                  d.cells.(x).(y) <- Hint (n, true);
                  acc)
            [] no_flag_neighs
        in
        match boom with
        | [] ->
            update_shade_texture ();
            update_flags_texture ();
            check_winning_cond ()
        | mines -> game_loosed mines)

  let click_left (x, y) =
    let d = data () in
    match d.cells.(x).(y) with
    | Empty false ->
        (*_reset_anim_disco ();*)
        propagate_visible x y;
        (*_start_anim_disco ();*)
        update_shade_texture ();
        update_flags_texture ();
        check_winning_cond ()
    | Empty true -> ()
    | Hint (n, false) ->
        d.cells.(x).(y) <- Hint (n, true);
        d.cells_flagged.(x).(y) <- false;
        update_shade_texture ();
        update_flags_texture ();
        check_winning_cond ()
    | Hint (n, true) -> guess_from_visible_hint n x y
    | Mine -> game_loosed [ (x, y) ]

  let click_right (x, y) =
    let d = data () in
    match d.cells.(x).(y) with
    | Hint (_, true) -> ()
    | Empty true -> ()
    | _ ->
        (match d.cells_flagged.(x).(y) with
        | true -> d.cells_flagged.(x).(y) <- false
        | false -> d.cells_flagged.(x).(y) <- true);
        update_flags_texture ();
        check_winning_cond ()

  let handle_mouse_button_down (e : Sdl.MouseButtonEvent.t) =
    if !has_won || !boom then (
      gen_game ();
      update_shade_texture ();
      update_flags_texture ();
      has_won := false;
      boom := false)
    else
      match cell_from_cursor e with
      | None -> ()
      | Some v ->
          if e.mb_button = 1 then click_left v
          else if e.mb_button = 3 then click_right v

  let update_size w h =
    let d = data () in
    if Float.of_int w /. Float.of_int h < text_ratio then (
      d.scr_width <- w - (w mod (ncells_x * 2));
      d.scr_height <- d.scr_width / ncells_x * ncells_y;
      d.scr_x <- (w - d.scr_width) / 2;
      d.scr_y <- (h - d.scr_height) / 2)
    else (
      d.scr_height <- h - (h mod (ncells_y * 2));
      d.scr_width <- d.scr_height / ncells_y * ncells_x;
      d.scr_x <- (w - d.scr_width) / 2;
      d.scr_y <- (h - d.scr_height) / 2)

  let handle_window_event = function
    | Sdl.WindowEventID.SDL_WINDOWEVENT_RESIZED e
    | Sdl.WindowEventID.SDL_WINDOWEVENT_SIZE_CHANGED e ->
        update_size e.x e.y
    | _ ->
        let d = data () in
        let w, h = Sdl.get_renderer_output_size d.renderer in
        update_size w h

  let restart_game () =
    if !has_won || !boom then (
      gen_game ();
      update_shade_texture ();
      update_flags_texture ();
      has_won := false;
      boom := false)

  (*
     input/rendering/events things
  *)

  let create_texture rdr (r, g, b, a) =
    let w = ncells_x * cell_width and h = ncells_y * cell_width in
    let texture =
      Sdl.render_create_texture rdr Sdl.PixelFormat.RGBA8888
        Sdl.TextureAccess.Target w h
    in
    Sdl.set_texture_blend_mode texture Sdl.BlendMode.SDL_BLENDMODE_BLEND;
    Sdl.set_renderer_target rdr (Some texture);
    Sdl.set_render_draw_color rdr ~rgb:(r, g, b) ~a;
    Sdl.render_clear rdr;
    Sdl.set_renderer_target rdr None;
    texture

  let on_ready _w r =
    Sdl_image.init [ `PNG ];
    let img_dir = List.nth Assets.Sites.images 0 in
    let fp fname = Filename.concat img_dir fname in
    let d =
      {
        _anim_disco_buffer = Array.make (ncells_x * ncells_y) (0, 0);
        _anim_disco_buffer_id = 0;
        _anim_disco_buffer_end = 0;
        _anim_disco_running = false;
        renderer = r;
        cells = Array.make_matrix ncells_x ncells_y (Empty false);
        cells_flagged = Array.make_matrix ncells_x ncells_y false;
        texture_board = create_texture r (0, 0, 0, 0);
        texture_shade = create_texture r (0, 0, 0, 0);
        texture_flags = create_texture r (0, 0, 0, 0);
        texture_images =
          [
            Sdl_image.load_texture r ~filename:(fp "mine.png");
            Sdl_image.load_texture r ~filename:(fp "1mines.png");
            Sdl_image.load_texture r ~filename:(fp "2mines.png");
            Sdl_image.load_texture r ~filename:(fp "3mines.png");
            Sdl_image.load_texture r ~filename:(fp "4mines.png");
            Sdl_image.load_texture r ~filename:(fp "5mines.png");
            Sdl_image.load_texture r ~filename:(fp "6mines.png");
            Sdl_image.load_texture r ~filename:(fp "7mines.png");
            Sdl_image.load_texture r ~filename:(fp "8mines.png");
            Sdl_image.load_texture r ~filename:(fp "flag.png");
            Sdl_image.load_texture r ~filename:(fp "exploded.png");
          ];
        scr_width = 100;
        scr_height = 100;
        scr_x = 0;
        scr_y = 0;
      }
    in
    mines_data := Some d;
    Random.self_init ();
    gen_game ();
    update_shade_texture ()

  let on_event = function
    | Sdl.Event.SDL_QUIT _ -> quit := true
    | Sdl.Event.SDL_KEYDOWN _ -> restart_game ()
    | Sdl.Event.SDL_MOUSEBUTTONDOWN e -> handle_mouse_button_down e
    | Sdl.Event.SDL_WINDOWEVENT e -> handle_window_event e.kind
    | _ -> ()

  let on_update () =
    (*
       (match d._anim_disco_running with
       | true -> _anim_disco ()
       | false -> ());*)
    if !quit then GameLoop.Exit else GameLoop.Continue

  let on_draw () =
    let d = data () in
    let rend = d.renderer
    and w = d.scr_width
    and h = d.scr_height
    and rx = d.scr_x
    and ry = d.scr_y in
    let br, bg, bb, ba = if !has_won then bg_col_won else bg_col in
    Sdl.set_render_draw_color rend ~rgb:(br, bg, bb) ~a:ba;
    Sdl.render_clear rend;
    let rect = Sdl.Rect.make4 ~x:rx ~y:ry ~w ~h in
    Sdl.render_copy rend ~texture:d.texture_board ~dst_rect:rect ();
    if !boom = false then
      Sdl.render_copy rend ~texture:d.texture_shade ~dst_rect:rect ();
    Sdl.render_copy rend ~texture:d.texture_flags ~dst_rect:rect ()

  let on_close () =
    let d = data () in
    Sdl.destroy_texture d.texture_board;
    Sdl.destroy_texture d.texture_shade;
    Sdl.destroy_texture d.texture_flags;
    List.iter (fun tex -> Sdl.destroy_texture tex) d.texture_images
end

let () =
  Sys.chdir (Filename.dirname Sys.executable_name);
  GameLoop.start ~win_name:"Démineur" ~on_ready:Minesweeper.on_ready
    ~on_event:Minesweeper.on_event ~on_update:Minesweeper.on_update
    ~on_draw:Minesweeper.on_draw ~on_close:Minesweeper.on_close;
  Gc.print_stat stdout;
  exit 0
