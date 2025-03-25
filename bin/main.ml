open Minesweeper

let () =
  (*let g = Grid.create ~w:30 ~h:30 in*)
  let grid_dir = List.nth AssetFiles.Sites.grids 0 in
  let grid_txt = Filename.concat grid_dir "grid.txt" in
  let grid = Model.Grid.of_file grid_txt in
  Model.Grid.print grid;

  (*Sys.chdir (Filename.dirname Sys.executable_name);*)
  Game.start ();
  exit 0
