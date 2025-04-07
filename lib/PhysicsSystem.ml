open Chipmunk
open Core

let ttime : float ref = ref 0.

let rec update ms phy = function
  | ({ physics_cmp = Some (orig_x, orig_y)
     ; transform_cmp = Some trans
     ; _} : Entity.t) :: tail ->
    if Float.(<=.) !ttime 2. then (
      let (timestep,_,ball_body,_,space) = phy in
      let pos = Cp.body_get_position ball_body in
      let vel = Cp.body_get_velocity ball_body in
      Printf.printf "Time is %f. ball is at (%f %f) with velocity (%f %f)\n"
      !ttime pos.x pos.y vel.x vel.y;
      ttime := !ttime +. timestep;
      Cp.space_step space timestep;
      trans.x <- orig_x + (Int.of_float (pos.y *. 15.));
      trans.y <- orig_y + (Int.of_float (pos.x *. 15.));
    );
    update ms phy tail
  | _ :: tail -> update ms phy tail
  | [] -> ()

