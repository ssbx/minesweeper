open CamlSDL2

type t = {
  mutable src_rect : Sdl.Rect.t;
  mutable dst_rect : Sdl.Rect.t;
  mutable w : int;
  mutable h : int;
  texture : Sdl.Texture.t;
}

let make ~filename rdr =
  let tex = Utils.Texture2D.of_png rdr ~filename in
  let _, _, w, h = Sdl.query_texture tex in
  {
    src_rect = Sdl.Rect.make ~x:0 ~y:0 ~w ~h;
    dst_rect = Sdl.Rect.make ~x:0 ~y:0 ~w ~h;
    w;
    h;
    texture = tex;
  }

let destroy sprite = Sdl.destroy_texture sprite.texture
