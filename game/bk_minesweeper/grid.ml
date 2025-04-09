type question_mark_t =
  { from_hint : int * int
  ; num_mines : int
  }

type cell_content_t =
  | Mine
  | Empty
  | Hint

type cell_t =
  { content : cell_content_t
  ; flagged : bool
  ; exposed : bool
  ; hint : int
  ; qmarks : question_mark_t list
  }

type grid_t =
  { g : cell_t array array
  ; h : int
  ; w : int
  }

let new_mine_cell =
  { content = Mine; flagged = false; exposed = false; hint = 0; qmarks = [] }
;;

let new_empty_cell =
  { content = Empty; flagged = false; exposed = false; hint = 0; qmarks = [] }
;;

let new_hint_cell n =
  { content = Hint; flagged = false; exposed = false; hint = n; qmarks = [] }
;;

type grid_cond_t =
  { nmines : int
  ; mines_flagged : int
  ; ncells : int
  ; cells_exposed : int
  ; completed : bool
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
;;

(** [iterxy fn grid] applies function [fn x y v] to all elements of [grid] *)
let iterxy fn grid = Array.iteri (fun y l -> Array.iteri (fun x v -> fn x y v) l) grid

(** PRETTY PRINTING *)

(** default to stdout *)
let pretty_print ?(chan = stdout) grid =
  iter_ext
    (fun () -> Printf.fprintf chan "\n")
    (fun v ->
       match v with
       | { content = Mine; flagged = false; _ } -> Printf.fprintf chan "(*) "
       | { content = Mine; flagged = true; _ } -> Printf.fprintf chan "!*! "
       | { content = Empty; exposed = false; _ } -> Printf.fprintf chan "(.) "
       | { content = Empty; exposed = true; _ } -> Printf.fprintf chan " .  "
       | { content = Hint; exposed = false; hint = h; _ } -> Printf.fprintf chan "(%i) " h
       | { content = Hint; exposed = true; hint = h; _ } -> Printf.fprintf chan " %i  " h)
    grid.g
;;

let print ?(chan = stdout) grid =
  iter_ext
    (fun () -> Printf.fprintf chan "\n")
    (fun v ->
       match v with
       | { content = Mine; _ } -> Printf.fprintf chan "* "
       | { content = Empty; _ } -> Printf.fprintf chan ". "
       | { content = Hint; hint = h; _ } -> Printf.fprintf chan "%i " h)
    grid
;;

(** RANDOM coords generation utils *)

(** generate a list of (x,y) coordinates *)
let rand_bag_make x y =
  List.flatten (List.init x (fun xi -> List.init y (fun yi -> xi, yi)))
;;

(** returns (newbag, (x,y)) *)
let rand_bag_pick bag =
  let nth = Random.int (List.length bag) in
  let coord = List.nth bag nth in
  let newbag = List.filteri (fun i _ -> i <> nth) bag in
  newbag, coord
;;

(** LOADING AND EXPORTS *)

(** from file *)
let from_file fname =
  let chan = Stdlib.open_in fname in
  let str =
    String.trim (Stdlib.really_input_string chan (Stdlib.in_channel_length chan))
  in
  Stdlib.close_in chan;
  let lines = String.split_on_char '\n' str in
  let w = List.length (String.split_on_char ' ' (String.trim (List.nth lines 0)))
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
  ({ g = grid; h; w } : grid_t)
;;

(** to file *)
let dump grid fname =
  let chan = Stdlib.open_out fname in
  print ~chan grid;
  Stdlib.close_out chan
;;

(** GRID GENERATION

    La grille du jeux apparaitra comme très large, défillant vers la gauche.

    # solution génération complète

    La grille est générée en entier avec echelonnement de niveaux, et assurance
    que tout est resoluble, en partant du principe qu'une zone de départ
    découverte est connue à l'avance.

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
;;

let neighbors x y w h =
  let neigh_list = [ -1, -1; 0, -1; 1, -1; -1, 0; 1, 0; -1, 1; 0, 1; 1, 1 ] in
  List.filter_map
    (fun (x_rel, y_rel) ->
       let nx = x + x_rel
       and ny = y + y_rel in
       if nx < w && nx >= 0 && ny < h && ny >= 0 then Some (nx, ny) else None)
    neigh_list
;;

let generate_hints width height grid =
  iterxy
    (fun x y c ->
       match c with
       | { content = Mine; _ } ->
         let neighs = neighbors x y width height in
         List.iter
           (fun (nx, ny) ->
              match grid.(ny).(nx) with
              | { content = Mine; _ } -> ()
              | { content = Empty; _ } -> grid.(ny).(nx) <- new_hint_cell 1
              | { content = Hint; _ } as h ->
                grid.(ny).(nx) <- { h with hint = h.hint + 1 })
           neighs
       | _ -> ())
    grid
;;

let get_unexposed x y { g; h; w } =
  List.filter (fun (x, y) -> g.(y).(x).exposed <> true) (neighbors x y w h)
;;

let update_qmarks cells from_x from_y nmines grid =
  List.iter
    (fun (x2, y2) ->
       let v = grid.g.(y2).(x2) in
       grid.g.(y2).(x2)
       <- { v with
            qmarks = { from_hint = from_x, from_y; num_mines = nmines } :: v.qmarks
          })
    cells
;;

let gen_mines_groups2 uhint grid =
  List.iter
    (fun (x, y) ->
       let unex = get_unexposed x y grid
       and cell = grid.g.(y).(x) in
       update_qmarks unex x y cell.hint grid)
    uhint
;;

(** RESOLVER
    - Expose all possible empty from an empty click
    - for all "hints":
    - nothing to do
    - set it as "processed" (so there is no infinite loop) (or increment a
      "pass" id)
    - continue
    - one or more "question flag"s can be set ("question flags" contains
      enoughthbe info to be related so "one exclude the other":
    - guess mined cells (one question flag)
    - guess empty cells
    - supress question flags
    - reset the "processed" state for all of the grid

    This should generate all the possible solutions? *)

(** discover from empty cell *)
let rec discover_from grid x y =
  match grid.g.(y).(x) with
  | { content = Mine; _ }
  | { content = Empty; exposed = true; _ }
  | { content = Hint; exposed = true; _ } -> ()
  | { content = Hint; _ } as v -> grid.g.(y).(x) <- { v with exposed = true }
  | { content = Empty; _ } as v ->
    grid.g.(y).(x) <- { v with exposed = true };
    let neighs = neighbors x y grid.w grid.h in
    List.iter (fun (x, y) -> discover_from grid x y) neighs
;;

(** STEP 1 disco all possible empty cells *)
let discover_possible grid =
  iterxy
    (fun x y v ->
       match v with
       | { content = Empty; exposed = true; _ } ->
         let n = neighbors x y grid.w grid.h in
         List.iter (fun (x, y) -> discover_from grid x y) n
       | _ -> ())
    grid.g
;;

(** STEP 2 use simple hints to discover obvious mines *)
let expose_hints grid coords =
  List.iter
    (fun (x, y) ->
       match grid.g.(y).(x) with
       | { content = Hint; hint = n; _ } ->
         let n1 = neighbors x y grid.w grid.h in
         let n2 =
           List.filter
             (fun (x, y) ->
                match grid.g.(y).(x) with
                | { content = Mine; flagged = true; _ } -> true
                | _ -> false)
             n1
         in
         if List.length n2 = n
         then (
           let n3 =
             List.filter
               (fun (x, y) ->
                  match grid.g.(y).(x) with
                  | { content = Mine; _ } -> false
                  | _ -> true)
               n1
           in
           List.iter
             (fun (x, y) ->
                match grid.g.(y).(x) with
                | { content = Mine; _ } -> ()
                | { content = Hint; _ } as v ->
                  grid.g.(y).(x) <- { v with exposed = true }
                | { content = Empty; _ } as v ->
                  grid.g.(y).(x) <- { v with exposed = true })
             n3)
       | _ -> ())
    coords
;;

let flag_mines { g = grid; _ } coords =
  List.iter (fun (x, y) -> grid.(y).(x) <- { (grid.(y).(x)) with flagged = true }) coords
;;

(** *)
let check_hints grid =
  iterxy
    (fun x y c ->
       match c with
       | { content = Hint; exposed = true; hint = n; _ } ->
         let n1 = neighbors x y grid.w grid.h in
         let n2 =
           List.filter
             (fun (x, y) ->
                match grid.g.(y).(x) with
                | { content = Empty; exposed = true; _ } -> false
                | { content = Hint; exposed = true; _ } -> false
                | _ -> true)
             n1
         in
         if n = List.length n2
         then (
           flag_mines grid n2;
           let hts =
             List.filter
               (fun (x, y) ->
                  match grid.g.(y).(x) with
                  | { content = Hint; _ } -> true
                  | _ -> false)
               n1
           in
           expose_hints grid hts)
       | _ -> ())
    grid.g
;;

(** STEP 3 use hints to discover two cells forced adjacent posibilities, and
    search common hints of these two cells, and if one mine is missing on this
    hint, discover adjascent cells *)

(** STEP 4 same as 3 with 3 mines *)

(** STEP 5 , steps 2,3 and 4 could be joined in the same algorithm *)

let get_state { g = grid; _ } =
  let c =
    { nmines = 0; mines_flagged = 0; ncells = 0; cells_exposed = 0; completed = false }
  in
  let c2 =
    Array.fold_left
      (fun acc1 l ->
         Array.fold_left
           (fun acc c ->
              match c with
              | { content = Hint; exposed = true; _ }
              | { content = Empty; exposed = true; _ } ->
                { acc with
                  ncells = acc.ncells + 1
                ; cells_exposed = acc.cells_exposed + 1
                }
              | { content = Hint; _ } | { content = Empty; _ } ->
                { acc with ncells = acc.ncells + 1 }
              | { content = Mine; flagged = true; _ } ->
                { acc with
                  nmines = acc.nmines + 1
                ; mines_flagged = acc.mines_flagged + 1
                }
              | { content = Mine; _ } -> { acc with nmines = acc.nmines + 1 })
           acc1
           l)
      c
      grid
  in
  if c2.nmines = c2.mines_flagged && c2.ncells = c2.cells_exposed
  then { c2 with completed = true }
  else c2
;;

(*
   let gen_mines_groups2 grid =
  iterxy (fun x y v -> grid.g.(y).(x) <- {v with qmarks = []}) grid.g
*)

(*
   let flag_m grid x y =
  let new_mine = {grid.g.(y).(x) with flagged=true} in
  grid.g.(y).(x) <- new_mine;
  List.iter (fun (x,y) ->

  ) new_mine.groups;
*)
let deduce_mines grid =
  iterxy
    (fun _x _y v ->
       match v with
       | { content = Hint; hint = _n; _ } ->
         ()
         (*if (List.length v) = n then
            List.iter (fun (x,y) ->
            flag_m grid x y
            ) v.relevant*)
       | _ -> ())
    grid.g
;;

let get_unresolved_hints grid =
  let res : (int * int) list ref = ref [] in
  Array.iteri
    (fun y l ->
       Array.iteri
         (fun x v ->
            match v with
            | { exposed = false; _ } -> res := (x, y) :: !res
            | _ -> ())
         l)
    grid.g;
  print_endline "lkjlkj";
  !res
;;

let rec resolve2 cond grid =
  let hts = get_unresolved_hints grid in
  gen_mines_groups2 hts grid;
  deduce_mines grid;
  let new_cond = get_state grid in
  if
    new_cond.cells_exposed <> cond.cells_exposed
    || new_cond.mines_flagged <> cond.mines_flagged
  then resolve2 new_cond grid
  else new_cond
;;

let rec resolve cond grid =
  check_hints grid;
  discover_possible grid;
  let new_cond = get_state grid in
  if
    new_cond.cells_exposed <> cond.cells_exposed
    || new_cond.mines_flagged <> cond.mines_flagged
  then resolve new_cond grid
  else new_cond
;;

(** INTERFACE functions *)

let make ?(width = 40) ?(height = 10) difficulty =
  Random.self_init ();
  let grid = Array.make_matrix height width new_empty_cell
  and nmines = width * height * difficulty / 100
  and randbag = rand_bag_make width height in
  generate_simple randbag grid nmines;
  generate_hints width height grid;
  grid
;;

let test () =
  (*let difficulty = 10 in
    let grid = make ~width:20 ~height:10 difficulty in
    dump grid "mygrid.gr";*)
  let _w = 20
  and _h = 10 in
  let grid = from_file "tests/mygrid2.gr" in
  pretty_print grid;
  Printf.printf "got %i exposed\n" (get_state grid).cells_exposed;
  discover_from grid 9 5;
  let cond = resolve2 (get_state grid) grid in
  if cond.completed then print_endline "completed!" else print_endline "uncompleted!";
  pretty_print grid
;;
