open CamlSDL2
open CamlSDL2_image

type t = Mine | Neigh1 | Neigh2 | Neigh3 | Neigh4 | Neigh5
              | Neigh6 | Neigh7 | Neigh8 | Flag | Exploded

type texture_set_t = {
  mine :   Sdl.Texture.t;
  neigh1 : Sdl.Texture.t;
  neigh2 : Sdl.Texture.t;
  neigh3 : Sdl.Texture.t;
  neigh4 : Sdl.Texture.t;
  neigh5 : Sdl.Texture.t;
  neigh6 : Sdl.Texture.t;
  neigh7 : Sdl.Texture.t;
  neigh8 : Sdl.Texture.t;
  flag   : Sdl.Texture.t;
  exploded : Sdl.Texture.t;
}

let texture_set = ref None

let get = function
  | Mine      -> (Option.get !texture_set).mine
  | Neigh1    -> (Option.get !texture_set).neigh1
  | Neigh2    -> (Option.get !texture_set).neigh2
  | Neigh3    -> (Option.get !texture_set).neigh3
  | Neigh4    -> (Option.get !texture_set).neigh4
  | Neigh5    -> (Option.get !texture_set).neigh5
  | Neigh6    -> (Option.get !texture_set).neigh6
  | Neigh7    -> (Option.get !texture_set).neigh7
  | Neigh8    -> (Option.get !texture_set).neigh8
  | Flag      -> (Option.get !texture_set).flag
  | Exploded  -> (Option.get !texture_set).exploded


let load  r =
  Img.init [ `PNG ];
  let img_dir = List.nth Assets.Sites.images 0 in
  let fp fname = Filename.concat img_dir fname in
  texture_set :=
    Some { mine     = Img.load_texture r ~filename:(fp "mine.png")
         ; neigh1   = Img.load_texture r ~filename:(fp "1mines.png")
         ; neigh2   = Img.load_texture r ~filename:(fp "2mines.png")
         ; neigh3   = Img.load_texture r ~filename:(fp "3mines.png")
         ; neigh4   = Img.load_texture r ~filename:(fp "4mines.png")
         ; neigh5   = Img.load_texture r ~filename:(fp "5mines.png")
         ; neigh6   = Img.load_texture r ~filename:(fp "6mines.png")
         ; neigh7   = Img.load_texture r ~filename:(fp "7mines.png")
         ; neigh8   = Img.load_texture r ~filename:(fp "8mines.png")
         ; flag     = Img.load_texture r ~filename:(fp "flag.png")
         ; exploded = Img.load_texture r ~filename:(fp "exploded.png") }

let destroy () =
  match !texture_set with
  | Some s ->
    Sdl.destroy_texture s.mine;
    Sdl.destroy_texture s.neigh1;
    Sdl.destroy_texture s.neigh2;
    Sdl.destroy_texture s.neigh3;
    Sdl.destroy_texture s.neigh4;
    Sdl.destroy_texture s.neigh5;
    Sdl.destroy_texture s.neigh6;
    Sdl.destroy_texture s.neigh7;
    Sdl.destroy_texture s.neigh8;
    Sdl.destroy_texture s.flag;
    Sdl.destroy_texture s.exploded
  | None -> ()

