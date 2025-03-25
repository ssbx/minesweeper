
module Vector = struct

  type t = {
    x : int;
    y : int;
  }

  let create ~x ~y = ({ x = x; y = y}: t)

end

module Cell = struct

  type t = {
    mutable flagged    : bool;
    mutable mined      : bool;
    mutable discovered : bool;
    mutable minesclose : int;
    mutable c_east  : t option ref;
    mutable c_west  : t option ref;
    mutable c_north : t option ref;
    mutable c_south : t option ref;
  }

  let empty = ({
    flagged    = false;
    mined      = false;
    discovered = false;
    minesclose = 0;
    c_east     = ref None;
    c_west     = ref None;
    c_north    = ref None;
    c_south    = ref None;
  }: t)

  let link grid x y (cell:t) =
    if y <> 0 then (
      cell.c_north            := Some grid.(x).(y - 1);
      grid.(x).(y-1).c_south  := Some cell;
    );
    if x <> 0 then (
      cell.c_west             := Some grid.(x - 1).(y);
      grid.(x - 1).(y).c_east := Some cell;
    )

  let of_char = function
    | ' ' -> None
    |  _  -> Some empty

  let to_char = function
    | None -> ' '
    | Some c when c.mined <> true -> '*'
    | Some _ -> '.'

end

module Grid = struct

  type t = {
    dimentions : Vector.t;
    cells      : Cell.t option array array;
  }

  let print grid =
    for y = 0 to grid.dimentions.y - 1 do
      for x = 0 to grid.dimentions.x - 1 do
        let c = grid.cells.(x).(y) in
        Printf.printf "%c" (Cell.to_char c)
      done;
      print_endline ""
    done

  (**
   *
   *)
  let create ~w ~h =
    ({ dimentions = Vector.create ~x:w ~y:h
     ; cells      = Array.make_matrix w h None } : t)

  (**
   *
   *)
  let of_file fname =

    (* load lines from file *)
    let c = Stdlib.open_in fname in
    let lines =
       Stdlib.in_channel_length c
    |> Stdlib.really_input_string c
    |> String.trim
    |> String.split_on_char '\n'
    in
    Stdlib.close_in c;

    (* create matrix *)
    let w = List.length lines in
    let h = String.length (List.nth lines 0) in
    let m = create ~w ~h in

    (* fill it with lines *)
    List.iteri (fun x row ->
      String.iteri (fun y ch ->
        m.cells.(x).(y) <- Cell.of_char ch;
      ) row
    ) lines;

    m

end
