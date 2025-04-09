let rec update = function
  | ({ transform_cmp = Some _trans; _ } : Entity.t) :: tail -> update tail
  | _ :: tail -> update tail
  | [] -> ()
;;
