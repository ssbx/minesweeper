open CamlSDL2

type t = { w : Sdl.Window.t
         ; r : Sdl.Renderer.t }

let init ~w ~h =
  Sdl.init [ `VIDEO; `EVENTS; `TIMER ];
  Sdl.set_hint "SDL_RENDER_SCALE_QUALITY" "2";
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
  { w = w; r = r; }

let present scr =
  Sdl.set_render_target scr.r None;
  Sdl.render_present scr.r

let clear scr =
  Sdl.set_render_target scr.r None;
  Sdl.set_render_draw_color scr.r ~r:100 ~g:0 ~b:0 ~a:255;
  Sdl.render_clear scr.r

let destroy scr =
  Sdl.destroy_renderer scr.r;
  Sdl.destroy_window scr.w

