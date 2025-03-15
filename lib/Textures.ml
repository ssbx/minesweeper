open CamlSDL2
open CamlSDL2_image

type t = Mine | Neigh1 | Neigh2 | Neigh3 | Neigh4 | Neigh5 
              | Neigh6 | Neigh7 | Neigh8 | Flag | Exploded

type data_t = {
  mine :   Sdl.Texture.t;
  neigh1 : Sdl.Texture.t;
  neigh2 : Sdl.Texture.t;
  neigh3 : Sdl.Texture.t;
  neigh4 : Sdl.Texture.t;
  neigh5 : Sdl.Texture.t;
  neigh6 : Sdl.Texture.t;
  neigh7 : Sdl.Texture.t;
  neigh8 : Sdl.Texture.t;
  flag :   Sdl.Texture.t;
  exploded : Sdl.Texture.t;
}

let datas = ref None

let get = function
  | Mine      -> (Option.get !datas).mine
  | Neigh1    -> (Option.get !datas).neigh1
  | Neigh2    -> (Option.get !datas).neigh2
  | Neigh3    -> (Option.get !datas).neigh3
  | Neigh4    -> (Option.get !datas).neigh4
  | Neigh5    -> (Option.get !datas).neigh5
  | Neigh6    -> (Option.get !datas).neigh6
  | Neigh7    -> (Option.get !datas).neigh7
  | Neigh8    -> (Option.get !datas).neigh8
  | Flag      -> (Option.get !datas).flag
  | Exploded  -> (Option.get !datas).exploded


let load  r =
  Img.init [ `PNG ];
  let img_dir = List.nth Assets.Sites.images 0 in
  let fp fname = Filename.concat img_dir fname in
  datas := 
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

