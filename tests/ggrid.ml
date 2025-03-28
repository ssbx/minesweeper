type mine_group_t = {
  hint_cell : int * int (* from this hint cell ... *);
  mine_pos : (int * int) list (* ... mine must be one of these coords *);
}

type cell_content_t = Mine | Empty | Hint

type cell_info_t = {
  content : cell_content_t;
  flagged : bool;
  exposed : bool;
  hint : int;
  groups : mine_group_t list;
}

let new_mine_cell =
  { content = Mine; flagged = false; exposed = false; hint = 0; groups = [] }

let new_empty_cell =
  { content = Empty; flagged = false; exposed = false; hint = 0; groups = [] }

let new_hint_cell n =
  { content = Hint; flagged = false; exposed = false; hint = n; groups = [] }

type grid_cond_t = {
  nmines : int;
  mines_flagged : int;
  ncells : int;
  cells_exposed : int;
  completed : bool;
}

(** ITERATIONS over grids *)

(** [iter_ext fn0 fn grid] applies function [fn] to all elements of [grid] and
    appies [fn0] after each lines of the grid have been proceced *)
let iter_ext fn0 fn grid =
  Array.iter
    (fun l ->
      Array.iter (fun v -> fn v) l;
      fn0 ())
    grid

(** [iterxy fn grid] applies function [fn x y v] to all elements of [grid] *)
let iterxy fn grid =
  Array.iteri (fun y l -> Array.iteri (fun x v -> fn x y v) l) grid

(** PRETTY PRINTING *)

(** default to stdout *)
let pretty_print ?(chan = stdout) grid =
  iter_ext
    (fun () -> Printf.fprintf chan "\n")
    (fun v ->
      match v with
      | { content = Mine; flagged = false } -> Printf.fprintf chan "(*) "
      | { content = Mine; flagged = true } -> Printf.fprintf chan "!*! "
      | { content = Empty; exposed = false } -> Printf.fprintf chan "(.) "
      | { content = Empty; exposed = true } -> Printf.fprintf chan " .  "
      | { content = Hint; exposed = false; hint = h } ->
          Printf.fprintf chan "(%i) " h
      | { content = Hint; exposed = true; hint = h } ->
          Printf.fprintf chan " %i  " h)
    grid

let print ?(chan = stdout) grid =
  iter_ext
    (fun () -> Printf.fprintf chan "\n")
    (fun v ->
      match v with
      | { content = Mine } -> Printf.fprintf chan "* "
      | { content = Empty } -> Printf.fprintf chan ". "
      | { content = Hint; hint = h } -> Printf.fprintf chan "%i " h)
    grid

(** RANDOM coords generation utils *)

(** generate a list of (x,y) coordinates *)
let rand_bag_make x y =
  List.flatten (List.init x (fun xi -> List.init y (fun yi -> (xi, yi))))

(** returns (newbag, (x,y)) *)
let rand_bag_pick bag =
  let nth = Random.int (List.length bag) in
  let coord = List.nth bag nth in
  let newbag = List.filteri (fun i _ -> i <> nth) bag in
  (newbag, coord)

(** LOADING AND EXPORTS *)

(** from file *)
let from_file fname =
  let chan = Stdlib.open_in fname in
  let str =
    String.trim
      (Stdlib.really_input_string chan (Stdlib.in_channel_length chan))
  in
  Stdlib.close_in chan;
  let lines = String.split_on_char '\n' str in
  let w =
    List.length (String.split_on_char ' ' (String.trim (List.nth lines 0)))
  and h = List.length lines in
  let grid = Array.make_matrix h w new_empty_cell in
  List.iteri
    (fun y line ->
      let cells = String.split_on_char ' ' (String.trim line) in
      List.iteri
        (fun x ch ->
          match ch with
          | "." -> grid.(y).(x) <- new_empty_cell
          | "*" -> grid.(y).(x) <- new_mine_cell
          | n -> grid.(y).(x) <- new_hint_cell (Stdlib.int_of_string n))
        cells)
    lines;
  grid

(** to file *)
let dump grid fname =
  let chan = Stdlib.open_out fname in
  print ~chan grid;
  Stdlib.close_out chan

(** GRID GENERATION

    La grille du jeux apparaitra comme très large, défillant vers la gauche.

    # solution génération complète

    La grille est générée en entier avec echelonnement de niveaux, et assurance que
    tout est resoluble, en partant du principe qu'une zone de départ découverte est
    connue à l'avance.

    Au premier click de lutilisateur, une minigrille et générée pour découvrir
    cette première zone connue. *)

(** HINTS GENERATION *)

(** the simple grid generation recursive function *)
let rec generate_simple randbag grid = function
  | 0 -> ()
  | mines ->
      let randbag2, (x, y) = rand_bag_pick randbag in
      grid.(y).(x) <- new_mine_cell;
      generate_simple randbag2 grid (mines - 1)

let neighbors x y w h =
  let ns =
    List.flatten
      (List.init 3 (fun xi ->
           List.init 3 (fun xy ->
               let rx = x - 1 + xi and ry = y - 1 + xy in
               (rx, ry))))
  in
  List.filter
    (fun (nx, ny) ->
      (nx <> x || ny <> y) && nx < w && ny < h && nx >= 0 && ny >= 0)
    ns

let generate_hints width height grid =
  iterxy
    (fun x y c ->
      match c with
      | { content = Mine } ->
          let neighs = neighbors x y width height in
          List.iter
            (fun (nx, ny) ->
              match grid.(ny).(nx) with
              | { content = Mine } -> ()
              | { content = Empty } -> grid.(ny).(nx) <- new_hint_cell 1
              | { content = Hint } as h ->
                  grid.(ny).(nx) <- { h with hint = h.hint + 1 })
            neighs
      | _ -> ())
    grid

(** RESOLVER
    - Expose all possible empty from an empty click
    - for all "hints":
    - nothing to do
    - set it as "processed" (so there is no infinite loop)
      (or increment a "pass" id)
    - continue
    - one or more "question flag"s  can be set ("question flags" contains
      enoughthbe info to be related so "one exclude the other":
    - guess mined cells (one question flag)
    - guess empty cells
    - supress question flags
    - reset the "processed" state for all of the grid

    This should generate all the possible solutions? *)

(** discover from empty cell *)
let rec discover_from grid x y w h =
  match grid.(y).(x) with
  | { content = Mine }
  | { content = Empty; exposed = true }
  | { content = Hint; exposed = true } ->
      ()
  | { content = Hint } as v -> grid.(y).(x) <- { v with exposed = true }
  | { content = Empty } as v ->
      grid.(y).(x) <- { v with exposed = true };
      let neighs = neighbors x y w h in
      List.iter (fun (x, y) -> discover_from grid x y w h) neighs

(** STEP 1 disco all possible empty cells *)
let discover_possible grid w h =
  iterxy
    (fun x y v ->
      match v with
      | { content = Empty; exposed = true } ->
          let n = neighbors x y w h in
          List.iter (fun (x, y) -> discover_from grid x y w h) n
      | _ -> ())
    grid

(** STEP 2 use simple hints to discover obvious mines *)
let expose_hints grid coords w h =
  List.iter
    (fun (x, y) ->
      match grid.(y).(x) with
      | { content = Hint; hint = n } ->
          let n1 = neighbors x y w h in
          let n2 =
            List.filter
              (fun (x, y) ->
                match grid.(y).(x) with
                | { content = Mine; flagged = true } -> true
                | _ -> false)
              n1
          in
          if List.length n2 = n then
            let n3 =
              List.filter
                (fun (x, y) ->
                  match grid.(y).(x) with
                  | { content = Mine } -> false
                  | _ -> true)
                n1
            in
            List.iter
              (fun (x, y) ->
                match grid.(y).(x) with
                | { content = Mine } -> ()
                | { content = Hint } as v ->
                    grid.(y).(x) <- { v with exposed = true }
                | { content = Empty } as v ->
                    grid.(y).(x) <- { v with exposed = true })
              n3
      | _ -> ())
    coords

let flag_mines grid coords =
  List.iter
    (fun (x, y) -> grid.(y).(x) <- { (grid.(y).(x)) with flagged = true })
    coords

(** *)
let check_hints grid w h =
  iterxy
    (fun x y c ->
      match c with
      | { content = Hint; exposed = true; hint = n } ->
          let n1 = neighbors x y w h in
          let n2 =
            List.filter
              (fun (x, y) ->
                match grid.(y).(x) with
                | { content = Empty; exposed = true } -> false
                | { content = Hint; exposed = true } -> false
                | _ -> true)
              n1
          in
          if n = List.length n2 then (
            flag_mines grid n2;
            let hts =
              List.filter
                (fun (x, y) ->
                  match grid.(y).(x) with
                  | { content = Hint } -> true
                  | _ -> false)
                n1
            in
            expose_hints grid hts w h)
      | _ -> ())
    grid

(** STEP 3 use hints to discover two cells forced adjacent posibilities,
    and search common hints of these two cells, and if one mine is missing on
    this hint, discover adjascent cells *)

(** STEP 4 same as 3 with 3 mines *)

(** STEP 5 , steps 2,3 and 4 could be joined in the same algorithm *)

let get_state grid =
  let c =
    {
      nmines = 0;
      mines_flagged = 0;
      ncells = 0;
      cells_exposed = 0;
      completed = false;
    }
  in
  let c2 =
    Array.fold_left
      (fun acc1 l ->
        Array.fold_left
          (fun acc c ->
            match c with
            | { content = Hint; exposed = true }
            | { content = Empty; exposed = true } ->
                {
                  acc with
                  ncells = acc.ncells + 1;
                  cells_exposed = acc.cells_exposed + 1;
                }
            | { content = Hint } | { content = Empty } ->
                { acc with ncells = acc.ncells + 1 }
            | { content = Mine; flagged = true } ->
                {
                  acc with
                  nmines = acc.nmines + 1;
                  mines_flagged = acc.mines_flagged + 1;
                }
            | { content = Mine } -> { acc with nmines = acc.nmines + 1 })
          acc1 l)
      c grid
  in
  if c2.nmines = c2.mines_flagged && c2.ncells = c2.cells_exposed then
    { c2 with completed = true }
  else c2

let rec resolve cond grid w h =
  check_hints grid w h;
  discover_possible grid w h;
  match get_state grid with
  | { cells_exposed = exp; mines_flagged = flagged } as v
    when exp = cond.cells_exposed && flagged = cond.mines_flagged ->
      v
  | cond2 -> resolve cond2 grid w h

(** INTERFACE functions *)

let make ?(width = 40) ?(height = 10) difficulty =
  Random.self_init ();
  let grid = Array.make_matrix height width new_empty_cell
  and nmines = width * height * difficulty / 100
  and randbag = rand_bag_make width height in
  generate_simple randbag grid nmines;
  generate_hints width height grid;
  grid

let test () =
  (*let difficulty = 10 in
    let grid = make ~width:20 ~height:10 difficulty in
    dump grid "mygrid.gr";*)
  let w = 20 and h = 10 in
  let grid = from_file "mygrid2.gr" in
  pretty_print grid;
  Printf.printf "got %i exposed\n" (get_state grid).cells_exposed;
  discover_from grid 9 5 w h;
  let cond = resolve (get_state grid) grid w h in
  if cond.completed then print_endline "completed!"
  else print_endline "uncompleted!";
  pretty_print grid
