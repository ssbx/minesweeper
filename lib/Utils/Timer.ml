open CamlSDL2

type job_t =
  { at : int
  ; fn : unit -> unit
  }

let jobs_queue : job_t list ref = ref []
let jobs_wait_queue : job_t list ref = ref []
let jobs_waiting : bool ref = ref false

let fire_at ms f =
  jobs_wait_queue := { at = ms; fn = f } :: !jobs_wait_queue;
  jobs_waiting := true
;;

let fire_in ms f = fire_at (Sdl.get_ticks () + ms) f

let rec update_all ticks del = function
  | [] -> del
  | j :: tail ->
    if ticks > j.at
    then (
      j.fn ();
      update_all ticks (j :: del) tail)
    else update_all ticks del tail
;;

let length () = List.length !jobs_queue + List.length !jobs_wait_queue

let update ticks =
  if !jobs_waiting
  then (
    jobs_queue := !jobs_queue @ !jobs_wait_queue;
    jobs_wait_queue := [];
    jobs_waiting := false);
  if List.length !jobs_queue > 0
  then (
    let del = update_all ticks [] !jobs_queue in
    jobs_queue
    := List.filter (fun a -> List.exists (fun d -> a == d) del = false) !jobs_queue)
;;
