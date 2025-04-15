open CamlSDL2
open CamlSDL2_image

type t = Sdl.Texture.t

let clear_white : Sdl.Color.t = { r = 255; g = 255; b = 255; a = 255 }

let set_color rdr tex ~r ~g ~b ~a =
  Sdl.set_render_target rdr (Some tex);
  Sdl.set_render_draw_color rdr ~r ~g ~b ~a;
  Sdl.render_clear rdr;
  Sdl.set_render_target rdr None

let clear rdr tex =
  set_color rdr tex ~r:clear_white.r ~g:clear_white.g ~b:clear_white.b
    ~a:clear_white.a

let create ~w ~h rdr =
  let tex =
    Sdl.create_texture rdr ~fmt:Sdl.PixelFormat.RGBA8888
      ~access:Sdl.TextureAccess.Target ~width:w ~height:h
  in
  Sdl.set_texture_blend_mode tex Sdl.BlendMode.BLEND;
  clear rdr tex;
  tex

let init () = Img.init [ `PNG ]
let of_png rdr ~filename = Img.load_texture rdr ~filename
let draw_begin rdr tex = Sdl.set_render_target rdr (Some tex)
let draw_end rdr _tex = Sdl.set_render_target rdr None

let copy rdr tex dstrect =
  Sdl.render_copy rdr ~texture:tex ~srcrect:None ~dstrect:(Some dstrect)

let destroy tex = Sdl.destroy_texture tex
