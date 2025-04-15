(* XXX: voir ~/src/box2d/build/bin/testbed,
  joints->cantilever et joints->chain *)

let () =
  (*let g = Grid.create ~w:30 ~h:30 in*)
  (*
  let grid_dir = Option.value_exn (List.nth AssetFiles.Sites.grids 0) in
  let grid_txt = Filename.concat grid_dir "grid.txt" in
  let grid = Level.Grid.of_file grid_txt in
  Level.Grid.print grid;
*)

  (*Sys.chdir (Filename.dirname Sys.executable_name);*)
  ColorWall.Game.main ();

  (*
     HelloChipmunk.Game.main ();
  Shmup.Game.main ();
  *)

  (*
     (*Gc.tune ~minor_heap_size:(262144 * 2) ();*)
  Gc.minor ();
  Gc.print_stat stdout;

  let _ = _loop 100 in

  Gc.minor ();
  Gc.print_stat stdout;

  Printf.printf "x is %i\n" !x;
  *)
  exit 0
