module GameLoop = struct
  type gdq_next_t =
    | Continue
    | Exit

  type gdq_state_t =
    { mutable fps_counter_start : int
    ; mutable fps_counter : int
    ; mutable on_ready_fun : Sdl.Window.t -> Sdl.Renderer.t -> unit
    ; mutable on_event_fun : Sdl.Event.t -> unit
    ; mutable on_update_fun : unit -> gdq_next_t
    ; mutable on_draw_fun : unit -> unit
    ; mutable on_close_fun : unit -> unit
    ; mutable _renderer : Sdl.Renderer.t option
    ; mutable _window : Sdl.Window.t option
    }

  let gdq_state =
    { fps_counter_start = 0
    ; fps_counter = 0
    ; on_ready_fun = (fun _ _ -> ())
    ; on_event_fun = (fun _ -> ())
    ; on_update_fun = (fun () -> Continue)
    ; on_draw_fun = (fun () -> ())
    ; on_close_fun = (fun () -> ())
    ; _renderer = None
    ; _window = None
    }
  ;;

  let rec poll_events () =
    match Sdl.poll_event () with
    | None -> ()
    | Some evt ->
      gdq_state.on_event_fun evt;
      poll_events ()
  ;;

  let rec loop _w r =
    poll_events ();
    match gdq_state.on_update_fun () with
    | Exit -> ()
    | Continue ->
      Sdl.render_clear r;
      gdq_state.on_draw_fun ();
      Sdl.render_present r;
      loop _w r
  ;;

  let start ~on_ready ~on_event ~on_update ~on_draw ~on_close ~win_name =
    Sdl.init [ `SDL_INIT_VIDEO; `SDL_INIT_EVENTS; `SDL_INIT_TIMER ];
    Sdl.set_hint "SDL_RENDER_SCALE_QUALITY" "2";
    Sdl.set_hint "SDL_RENDER_VSYNC" "1";
    let window =
      Sdl.create_window
        ~title:win_name
        ~pos:(`centered, `centered)
        ~dims:(1240, 780)
        ~flags:[ Sdl.WindowFlags.Resizable; Sdl.WindowFlags.OpenGL ]
    in
    let renderer =
      Sdl.create_renderer
        ~win:window
        ~index:(-1)
        ~flags:
          [ Sdl.RendererFlags.Accelerated
          ; Sdl.RendererFlags.TargetTexture
          ; Sdl.RendererFlags.PresentVSync
          ]
    in
    gdq_state.on_ready_fun <- on_ready;
    gdq_state.on_event_fun <- on_event;
    gdq_state.on_update_fun <- on_update;
    gdq_state.on_draw_fun <- on_draw;
    gdq_state.on_close_fun <- on_close;
    gdq_state._window <- Some window;
    gdq_state._renderer <- Some renderer;
    gdq_state.on_ready_fun window renderer;
    gdq_state.fps_counter_start <- Sdl.get_ticks ();
    loop window renderer;
    gdq_state.on_close_fun ();
    Sdl.destroy_window window
  ;;
end
