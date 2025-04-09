open CamlSDL2
open Core

let callbacks : (Sdl.Event.t -> unit) list ref = ref []
let add cb = callbacks := cb :: !callbacks

let rec poll () =
  match Sdl.poll_event () with
  | None -> ()
  | Some evt ->
    List.iter ~f:(fun cb -> cb evt) !callbacks;
    poll ()
;;
