open CamlSDL2
open Core

let scale i f = Int.to_float i |> ( *. ) f |> Int.of_float

let rec update = function
  | ({ sprite_cmp = Some sprite; transform_cmp = Some trans; _ } : Entity.t) :: tail ->
    let scaled_w = scale sprite.src_rect.w trans.scale
    and scaled_h = scale sprite.src_rect.h trans.scale in
    sprite.dst_rect
    <- Sdl.Rect.make
         ~x:(trans.x - (scaled_w / 2))
         ~y:(trans.y - (scaled_h / 2))
         ~w:scaled_w
         ~h:scaled_h;
    update tail
  | _ :: tail -> update tail
  | [] -> ()
;;

let rec render rdr = function
  | ({ sprite_cmp = Some sprite; _ } : Entity.t) :: tail ->
    Sdl.render_copy
      rdr
      ~srcrect:(Some sprite.src_rect)
      ~dstrect:(Some sprite.dst_rect)
      ~texture:sprite.texture;
    render rdr tail
  | _ :: tail -> render rdr tail
  | [] -> ()
;;
