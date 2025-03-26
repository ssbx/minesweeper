open CamlSDL2

type t = Sdl.Window.t * Sdl.Renderer.t

let init ~w ~h =
  Sdl.init [ `VIDEO; `EVENTS; `TIMER ];
  Sdl.set_hint "SDL_RENDER_SCALE_QUALITY" "0";
  Sdl.set_hint "SDL_RENDER_VSYNC" "1";
  let w = Sdl.create_window
    ~title:"Minesweeper"
    ~x:`centered
    ~y:`centered
    ~width:w
    ~height:h
    ~flags:[ (*Sdl.WindowFlags.Resizable*)
            Sdl.WindowFlags.OpenGL ] in
  let r = Sdl.create_renderer
    ~win:w
    ~index:(-1)
    ~flags:[ Sdl.RendererFlags.Accelerated
           ; Sdl.RendererFlags.TargetTexture
           ; Sdl.RendererFlags.PresentVSync ] in
  Sdl.set_render_draw_color r ~r:0 ~g:0 ~b:0 ~a:255;
  Sdl.render_clear r;
  Sdl.render_present r;
  w, r

let present (_w, r) =
  Sdl.set_render_target r None;
  Sdl.render_present r

let clear (_w, r) =
  Sdl.set_render_target r None;
  Sdl.set_render_draw_color r ~r:100 ~g:0 ~b:0 ~a:255;
  Sdl.render_clear r

let destroy (w, r) =
  Sdl.destroy_renderer r;
  Sdl.destroy_window w

