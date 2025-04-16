open Chipmunk
open Core

let update ms entities space =
    let dt = (Float.of_int ms) in
    Cp.space_step space  dt;
    List.iter ~f:(fun (e:Entity.t) -> 
      match e.physics_cmp2, e.transform_cmp with
      | Some body, Some trans ->
        let pos = Cp.body_get_position body in
        trans.x <- Int.of_float (pos.y *. 15.);
        trans.y <- Int.of_float (pos.x *. 15.)
      | _ -> ()
    ) entities
    
