open CamlSDL2
open Engine.BackEnd

let cell_width = 10
let level_width = cell_width * 500
let level_height = cell_width * 30

type t = {
  background_tex   : Texture2D.t;
  foreground_tex   : Texture2D.t;
}

let bgrect = ref (Sdl.Rect.make ~x:0 ~y:0 ~w:0 ~h:0)
let fgrect = ref (Sdl.Rect.make ~x:0 ~y:0 ~w:0 ~h:0)

let init ~w ~h r =

  bgrect := Sdl.Rect.make ~x:0 ~y:0 ~w ~h;

  let fgh = h - 100
  and fgx = 50 in
  fgrect := Sdl.Rect.make ~x:0 ~y:fgx ~w:5000 ~h:fgh;

  let bg_tex = Texture2D.create r ~w ~h
  and fg_tex = Texture2D.create r ~w:level_width ~h:level_height in
  Texture2D.set_color r bg_tex ~r:100 ~g:100 ~b:100 ~a:255;
  Texture2D.set_color r fg_tex ~r:150 ~g:150 ~b:150 ~a:255;
  { background_tex = bg_tex
  ; foreground_tex = fg_tex }

let destroy view =
  Texture2D.destroy view.background_tex;
  Texture2D.destroy view.foreground_tex

let draw r view =
  Sdl.render_copy r
    ~texture:view.background_tex
    ~srcrect:None
    ~dstrect:(Some !bgrect);
  Sdl.render_copy r
    ~texture:view.foreground_tex
    ~srcrect:None
    ~dstrect:(Some !fgrect);

