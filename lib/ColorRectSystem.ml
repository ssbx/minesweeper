open CamlSDL2

let scale i f = Int.to_float i |> ( *. ) f |> Int.of_float

let rec update = function
  | ({ color_rect = Some crect; transform_cmp = Some trans; _ } : Entity.t) :: tail ->
    let scaled_w = scale crect.src_rect.w trans.scale
    and scaled_h = scale crect.src_rect.h trans.scale in
    crect.dst_rect
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
  | ({ color_rect = Some crect; _ } : Entity.t) :: tail ->
    Sdl.set_render_draw_color rdr ~r:crect.r ~g:crect.g ~b:crect.b ~a:crect.a;
    Sdl.render_fill_rect rdr crect.dst_rect;
    render rdr tail
  | _ :: tail -> render rdr tail
  | [] -> ()
;;
