open Chipmunk
open Core

let update ms entities space =
    let dt = (Float.of_int ms) in

    (*Printf.printf "physys2 before update!!!\n";
    Out_channel.flush stdout;*)
    Cp.space_step space  (dt /. 50000.);
    (*Printf.printf "physys2 after update!!!\n";
    Out_channel.flush stdout;*)

    List.iter ~f:(fun (e:Entity.t) -> 
      (*Printf.printf "physys2 iter entity !!!\n";
      Out_channel.flush stdout;*)
      match e.physics_cmp2, e.transform_cmp with
      | Some body, Some trans ->
        let pos = Cp.body_get_position body in
        (*Printf.printf "pos is %f %f\n" pos.x pos.y;
        Out_channel.flush stdout;*)
        trans.x <- Int.of_float (pos.y);
        trans.y <- Int.of_float (pos.x)
      | _ -> ()
    ) entities
    
