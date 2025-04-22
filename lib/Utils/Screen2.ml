open CamlSDL2

type t = Sdl.Window.t * Sdl.Renderer.t

let init ~w ~h =
  Sdl.init [ `VIDEO; `EVENTS; `TIMER ];
  Sdl.set_hint "SDL_RENDER_SCALE_QUALITY" "0";
  Sdl.set_hint "SDL_RENDER_VSYNC" "1";
  Sdl.gl_set_attribute Sdl.GLattr.SDL_GL_CONTEXT_MAJOR_VERSION 4;
  Sdl.gl_set_attribute Sdl.GLattr.SDL_GL_CONTEXT_MINOR_VERSION 3;
  (*Sdl.gl_set_attribute Sdl.GLattr.SDL_GL_CONTEXT_PROFILE_MASK 
                        Sdl.GLattr.SDL_GL_CONTEXT_PROFILE_CORE;*)
  Sdl.gl_set_attribute Sdl.GLattr.SDL_GL_DOUBLEBUFFER 1;
  Sdl.gl_set_attribute Sdl.GLattr.SDL_GL_DEPTH_SIZE 24;

  let w =
    Sdl.create_window ~title:"Minesweeper" ~x:`centered ~y:`centered ~width:w
      ~height:h
      ~flags:[ Sdl.WindowFlags.OpenGL ]
  in
  let c = Sdl.gl_create_context ~win:w in
  Sdl.gl_set_swap_interval ~interval:1;

  print_endline "hello gl";
  (w, c)

let present (w, _c) = Sdl.gl_swap_window w

let clear (_w, _r) = Sdl.glClear ()

let destroy (w, c) =
  Sdl.gl_delete_context c;
  Sdl.destroy_window w
