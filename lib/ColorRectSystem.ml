open CamlSDL2
open Core

let scale i f = Int.to_float i |> ( *. ) f |> Int.of_float

let update_entity (e : Entity.t) =
  match (e.color_rect, e.transform_cmp) with
  | Some crect, Some trans ->
      let scaled_w = scale crect.src_rect.w trans.scale
      and scaled_h = scale crect.src_rect.h trans.scale in
      crect.dst_rect <-
        Sdl.Rect.make
          ~x:(trans.x - (scaled_w / 2))
          ~y:(trans.y - (scaled_h / 2))
          ~w:scaled_w ~h:scaled_h
  | _ -> ()

let update (entities : Entity.t list) = List.iter ~f:update_entity entities

let render_entity rdr (e : Entity.t) =
  match e.color_rect with
  | None -> ()
  | Some crect ->
      Sdl.set_render_draw_color rdr ~r:crect.r ~g:crect.g ~b:crect.b ~a:crect.a;
      Sdl.render_fill_rect rdr crect.dst_rect

let render rdr entities = List.iter ~f:(fun e -> render_entity rdr e) entities
