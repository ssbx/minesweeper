open CamlSDL2
module Easing = EasingCurve

type anim_t = {
  easing : float -> float;
  pt_start : int;
  pt_end : int;
  vector : float;
  mutable ticks_start : float;
  ticks_span : float;
  at_update : int -> unit;
  at_end : unit -> unit;
  at_start : unit -> unit;
}

type anim_handle_t = anim_t list

let anim_empty : anim_t =
  {
    easing = (fun _ -> 0.);
    pt_start = 0;
    pt_end = 0;
    vector = 0.;
    ticks_start = 0.;
    ticks_span = 0.;
    at_update = (fun _ -> ());
    at_end = (fun () -> ());
    at_start = (fun () -> ());
  }

let anims_queue : anim_t list ref = ref []
let anims_wait_queue : anim_t list ref = ref []
let anims_waiting : bool ref = ref false
let length () = List.length !anims_queue + List.length !anims_wait_queue

let create ~pt_start ~pt_end ~span ~at_update ?(at_end = fun () -> ())
    ?(at_start = fun () -> ()) curve =
  [
    {
      easing = Easing.get_anim curve;
      pt_start;
      pt_end;
      vector = Float.of_int (pt_end - pt_start);
      ticks_start = 0.;
      ticks_span = Float.of_int span;
      at_update;
      at_end;
      at_start;
    };
  ]

let create_v2 ~pt1_start ~pt1_end ~at1_update ~pt2_start ~pt2_end ~at2_update
    ~span ?(at_start = fun () -> ()) ?(at_end = fun () -> ()) curve =
  [
    {
      easing = Easing.get_anim curve;
      pt_start = pt1_start;
      pt_end = pt1_end;
      vector = Float.of_int (pt1_end - pt1_start);
      ticks_start = 0.;
      ticks_span = Float.of_int span;
      at_update = at1_update;
      at_end = (fun () -> ());
      at_start;
    };
    {
      easing = Easing.get_anim curve;
      pt_start = pt2_start;
      pt_end = pt2_end;
      vector = Float.of_int (pt2_end - pt2_start);
      ticks_start = 0.;
      ticks_span = Float.of_int span;
      at_update = at2_update;
      at_end;
      at_start = (fun () -> ());
    };
  ]

let start anim_handle =
  List.iter
    (fun anim ->
      anim.ticks_start <- Float.of_int (Sdl.get_ticks ());
      anim.at_start ();
      anim.at_update anim.pt_start)
    anim_handle;
  anims_wait_queue := anim_handle @ !anims_wait_queue;
  anims_waiting := true

let create_start ~pt_start ~pt_end ~span ~at_update ?(at_end = fun () -> ())
    ?(at_start = fun () -> ()) curve =
  let a = create ~pt_start ~pt_end ~span ~at_update ~at_end ~at_start curve in
  start a

(*let animate anim int_ticks =*)
let rec update_all ticks del = function
  | [] -> del
  | a :: tail ->
      let prog = (ticks -. a.ticks_start) /. a.ticks_span in
      if prog > 1.0 then (
        a.at_update a.pt_end;
        a.at_end ();
        update_all ticks (a :: del) tail)
      else
        let dist = a.easing prog in
        let curr = a.pt_start + Float.to_int (dist *. a.vector) in
        a.at_update curr;
        update_all ticks del tail

let update int_ticks =
  if !anims_waiting then (
    anims_queue := !anims_queue @ !anims_wait_queue;
    anims_wait_queue := [];
    anims_waiting := false);
  if List.length !anims_queue > 0 then
    let del = update_all (Float.of_int int_ticks) [] !anims_queue in
    anims_queue :=
      List.filter
        (fun a -> List.exists (fun d -> a == d) del = false)
        !anims_queue
