open CamlSDL2

type t = {
  w                 : Sdl.Window.t;
  r                 : Sdl.Renderer.t;
  background_tex    : Sdl.Texture.t;
  hiden_cells_tex   : Sdl.Texture.t;
  flagged_cells_tex : Sdl.Texture.t;
}

let scr_data : t option ref = ref None

let get () = Option.get !scr_data

let create_tex ~w ~h r =
  let t = Sdl.create_texture r
    ~fmt:Sdl.PixelFormat.RGBA8888
    ~access:Sdl.TextureAccess.Target
    ~width:w
    ~height:h in
  Sdl.set_texture_blend_mode t Sdl.BlendMode.BLEND;
  Sdl.set_render_target r (Some t);
  Sdl.set_render_draw_color r ~r:255 ~g:255 ~b:255 ~a:255;
  Sdl.render_clear r;
  Sdl.set_render_target r None;
  t

let init () =
  Sdl.init [ `VIDEO; `EVENTS; `TIMER ];
  Sdl.set_hint "SDL_RENDER_SCALE_QUALITY" "2";
  Sdl.set_hint "SDL_RENDER_VSYNC" "1";
  let w = Sdl.create_window
    ~title:"Minesweeper"
    ~x:`centered
    ~y:`centered
    ~width:1240
    ~height:780
    ~flags:[ (*Sdl.WindowFlags.Resizable*)
            Sdl.WindowFlags.OpenGL ] in
  let r = Sdl.create_renderer
    ~win:w
    ~index:(-1)
    ~flags:[ Sdl.RendererFlags.Accelerated
           ; Sdl.RendererFlags.TargetTexture
           ; Sdl.RendererFlags.PresentVSync ] in
  Textures.load r;
  scr_data :=
    Some { w = w
         ; r = r
         ; background_tex = create_tex ~w:1240 ~h:780 r
         ; hiden_cells_tex   = create_tex ~w:1240 ~h:780 r
         ; flagged_cells_tex = create_tex ~w:1240 ~h:780 r }


let draw2 scr =
  Sdl.set_render_draw_color scr.r ~r:0 ~g:0 ~b:255 ~a:255;
  Sdl.render_clear scr.r

let draw () = get () |> draw2


let destroy () =
  let scr = get () in
  Sdl.destroy_texture scr.background_tex;
  Sdl.destroy_texture scr.hiden_cells_tex;
  Sdl.destroy_texture scr.flagged_cells_tex;
  Textures.destroy ();
  Sdl.destroy_renderer scr.r;
  Sdl.destroy_window scr.w

