type easing_func_t =
  | Linear
  | Quadratic_in
  | Quadratic_out
  | Quadratic_inout
  | Cubic_in
  | Cubic_out
  | Cubic_inout
  | Quartic_in
  | Quartic_out
  | Quartic_inout
  | Quintic_in
  | Quintic_out
  | Quintic_inout
  | Sin_in
  | Sin_out
  | Sin_inout
  | Circular_in
  | Circular_out
  | Circular_inout
  | Exponential_in
  | Exponential_out
  | Exponential_inout
  | Elastic_in
  | Elastic_out
  | Elastic_inout
  | Back_in
  | Back_out
  | Back_inout
  | Bounce_out
  | Bounce_in
  | Bounce_inout

let pi = Float.pi
let half_pi = pi /. 2.
let linear f = f
let quadratic_in f = f *. f
let quadratic_out f = -.(f *. (f -. 2.))

let quadratic_inout f =
  if f < 0.5 then 2. *. f *. f else (-2. *. f *. f) +. (4. *. f) -. 1.
;;

let cubic_in f = f *. f *. f

let cubic_out f =
  let t = f -. 1. in
  (t *. t *. t) +. 1.
;;

let cubic_inout f =
  if f < 0.5
  then 4. *. f *. f *. f
  else (
    let t = (2. *. f) -. 2. in
    (0.5 *. t *. t *. t) +. 1.)
;;

let quartic_in f = f *. f *. f *. f

let quartic_out f =
  let t = f -. 1. in
  (t *. t *. t *. (1. -. f)) +. 1.
;;

let quartic_inout f =
  if f < 0.5
  then 8. *. f *. f *. f *. f
  else (
    let t = f -. 1. in
    (-8. *. t *. t *. t *. t) +. 1.)
;;

let quintic_in f = f *. f *. f *. f *. f

let quintic_out f =
  let t = f -. 1. in
  (t *. t *. t *. t *. t) +. 1.
;;

let quintic_inout f =
  if f < 0.5
  then 16. *. f *. f *. f *. f *. f
  else (
    let t = (2. *. f) -. 2. in
    (0.5 *. t *. t *. t *. t *. t) +. 1.)
;;

let sin_in f = sin ((f -. 1.) *. half_pi) +. 1.
let sin_out f = sin (f *. half_pi)
let sin_inout f = 0.5 *. (1. -. cos (f *. pi))
let circular_in f = 1. -. sqrt (1. -. (f *. f))
let circular_out f = sqrt ((2. -. f) *. f)

let circular_inout f =
  if f < 0.5
  then 0.5 *. (1. -. sqrt (1. -. (4. *. (f *. f))))
  else 0.5 *. (sqrt (-.((2. *. f) -. 3.) *. ((2. *. f) -. 1.)) +. 1.)
;;

let exponential_in f = if f = 0.0 then f else Float.pow 2. (10. *. (f -. 1.))
let exponential_out f = if f = 1.0 then f else 1. -. Float.pow 2. (-10. *. f)

let exponential_inout f =
  if f = 0.0 || f = 1.0
  then f
  else if f < 0.5
  then 0.5 *. Float.pow 2. ((20. *. f) -. 10.)
  else (-0.5 *. Float.pow 2. ((-20. *. f) +. 10.)) +. 1.
;;

let elastic_in f = sin (13. *. half_pi *. f) *. Float.pow 2. (10. *. (f -. 1.))
let elastic_out f = (sin (-13. *. half_pi *. (f +. 1.)) *. Float.pow 2. (-10. *. f)) +. 1.

let elastic_inout f =
  if f < 0.5
  then 0.5 *. sin (13. *. half_pi *. (2. *. f)) *. Float.pow 2. (10. *. ((2. *. f) -. 1.))
  else
    0.5
    *. ((sin (-13. *. half_pi *. ((2. *. f) -. 1. +. 1.))
         *. Float.pow 2. (-10. *. ((2. *. f) -. 1.)))
        +. 2.)
;;

let back_in f = (f *. f *. f) -. (f *. sin (f *. pi))

let back_out f =
  let t = 1. -. f in
  1. -. ((t *. t *. t) -. (t *. sin (t *. pi)))
;;

let back_inout f =
  if f < 0.5
  then (
    let t = 2. *. f in
    0.5 *. ((t *. t *. t) -. (t *. sin (t *. pi))))
  else (
    let t = 1. -. ((2. *. f) -. 1.) in
    (0.5 *. (1. -. ((t *. t *. t) -. (t *. sin (t *. pi))))) +. 0.5)
;;

let bounce_out f =
  if f < 4. /. 11.
  then 121. *. f *. f /. 16.
  else if f < 8. /. 11.
  then (363. /. 40. *. f *. f) -. (99. /. 10. *. f) +. (17. /. 5.)
  else if f < 9. /. 10.
  then (4356. /. 361. *. f *. f) -. (35442. /. 1805. *. f) +. (16061. /. 1805.)
  else (54. /. 5. *. f *. f) -. (513. /. 25. *. f) +. (268. /. 25.)
;;

let bounce_in f = 1. -. bounce_out (1. -. f)

let bounce_inout f =
  if f < 0.5
  then 0.5 *. bounce_in (f *. 2.)
  else (0.5 *. bounce_out ((f *. 2.) -. 1.)) +. 0.5
;;

let rec easing_test func step f =
  match f with
  | x when x < 0. || x > 1. -> ()
  | x ->
    let v = func x in
    let nindent = Float.to_int (v *. 58.) + 22 in
    let indent = String.make nindent ' ' in
    Printf.printf "%s*\n" indent;
    easing_test func step (f +. step)
;;

let get_anim = function
  | Linear -> linear
  | Quadratic_in -> quadratic_in
  | Quadratic_out -> quadratic_out
  | Quadratic_inout -> quadratic_inout
  | Cubic_in -> cubic_in
  | Cubic_out -> cubic_out
  | Cubic_inout -> cubic_inout
  | Quartic_in -> quartic_in
  | Quartic_out -> quartic_out
  | Quartic_inout -> quartic_inout
  | Quintic_in -> quintic_in
  | Quintic_out -> quintic_out
  | Quintic_inout -> quintic_inout
  | Sin_in -> sin_in
  | Sin_out -> sin_out
  | Sin_inout -> sin_inout
  | Circular_in -> circular_in
  | Circular_out -> circular_out
  | Circular_inout -> circular_inout
  | Exponential_in -> exponential_in
  | Exponential_out -> exponential_out
  | Exponential_inout -> exponential_inout
  | Elastic_in -> elastic_in
  | Elastic_out -> elastic_out
  | Elastic_inout -> elastic_inout
  | Back_in -> back_in
  | Back_out -> back_out
  | Back_inout -> back_inout
  | Bounce_out -> bounce_out
  | Bounce_in -> bounce_in
  | Bounce_inout -> bounce_inout
;;

let demo () =
  let step = 1. /. 30. in
  print_endline "linear";
  easing_test linear step 0.;
  print_endline "quadratic_in";
  easing_test quadratic_in step 0.;
  print_endline "quadratic_out";
  easing_test quadratic_out step 0.;
  print_endline "quadratic_inout";
  easing_test quadratic_inout step 0.;
  print_endline "cubic_in";
  easing_test cubic_in step 0.;
  print_endline "cubic_out";
  easing_test cubic_out step 0.;
  print_endline "cubic_inout";
  easing_test cubic_inout step 0.;
  print_endline "quartic_in";
  easing_test quartic_in step 0.;
  print_endline "quartic_out";
  easing_test quartic_out step 0.;
  print_endline "quartic_inout";
  easing_test quartic_inout step 0.;
  print_endline "quintic_in";
  easing_test quintic_in step 0.;
  print_endline "quintic_out";
  easing_test quintic_out step 0.;
  print_endline "quintic_inout";
  easing_test quintic_inout step 0.;
  print_endline "sin_in";
  easing_test sin_in step 0.;
  print_endline "sin_out";
  easing_test sin_out step 0.;
  print_endline "sin_inout";
  easing_test sin_inout step 0.;
  print_endline "circular_in";
  easing_test circular_in step 0.;
  print_endline "circular_out";
  easing_test circular_out step 0.;
  print_endline "circular_inout";
  easing_test circular_inout step 0.;
  print_endline "exponential_in";
  easing_test exponential_in step 0.;
  print_endline "exponential_out";
  easing_test exponential_out step 0.;
  print_endline "exponential_inout";
  easing_test exponential_inout step 0.;
  print_endline "elastic_in";
  easing_test elastic_in step 0.;
  print_endline "elastic_out";
  easing_test elastic_out step 0.;
  print_endline "elastic_inout";
  easing_test elastic_inout step 0.;
  print_endline "back_in";
  easing_test back_in step 0.;
  print_endline "back_out";
  easing_test back_out step 0.;
  print_endline "back_inout";
  easing_test back_inout step 0.;
  print_endline "bounce_out";
  easing_test bounce_out step 0.;
  print_endline "bounce_in";
  easing_test bounce_in step 0.;
  print_endline "bounce_inout";
  easing_test bounce_inout step 0.;
  print_endline ""
;;
