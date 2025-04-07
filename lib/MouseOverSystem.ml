open Core

let pixel_radius = 360
let pixel_radius_f = Int.to_float pixel_radius
let col_move = 10
type direction_t = Up | Down

let set_intensity intensity bt = (Int.to_float bt) *. intensity |> Int.of_float

let gencol i = function
| b, Up when b > (255 - col_move) ->
  b - ((Random.int col_move) |> set_intensity i)
| b, Up ->
  b + ((Random.int col_move) |> set_intensity i)
| b, Down when b < col_move ->
  b + ((Random.int col_move) |> set_intensity i)
| b, Down ->
  b - ((Random.int col_move) |> set_intensity i)

let rand_cols i r g b =
  let changebyte = Random.int 3 in
  let direction = if Random.bool () then Up else Down in
  match changebyte with
  | 0 -> (gencol i (r, direction), g, b)
  | 1 -> (r, gencol i (g, direction), b)
  | 2 -> (r, g, gencol i (b, direction))
  | _ -> failwith "nononon"


let rec update ms = function
| ({ transform_cmp = Some t
   ; mousein_cmp = None
   ; color_rect = Some r; _} : Entity.t) :: tail ->
  let dist_x = Int.to_float (!MouseInSystem.pos_x - t.x |> abs)
  and dist_y = Int.to_float (!MouseInSystem.pos_y - t.y |> abs) in
  let dist_f = sqrt (dist_x *. dist_x +. dist_y *. dist_y) in
  let dist = Int.of_float dist_f in
  if dist <= pixel_radius then (
    let intensity = (Float.cos (dist_f /. pixel_radius_f)) in
    let (r', g', b') = rand_cols intensity r.r r.g r.b in
    r.r <- r';
    r.g <- g';
    r.b <- b';
    r.a <- 255
  );
  update ms tail
| _ :: tail -> update ms tail
| [] -> ()

