(*
   Copyright (c) 2023, Leandro Ostera

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
*)
let epsilon = 0.001

type t =
  { pos_pos_coef : float
  ; pos_vel_coef : float
  ; vel_pos_coef : float
  ; vel_vel_coef : float
  }

type snapshot =
  { position : float
  ; velocity : float
  }

let zero_snapshot = { position = 0.; velocity = 0. }

let identity =
  { pos_pos_coef = 1.; pos_vel_coef = 0.; vel_pos_coef = 0.; vel_vel_coef = 1. }
;;

let make_overdamped ~delta_time ~angular_freq ~damping_ratio =
  let za = -.angular_freq *. damping_ratio in
  let zb = angular_freq *. sqrt ((damping_ratio *. damping_ratio) -. 1.0) in
  let z1 = za -. zb in
  let z2 = za +. zb in
  let e1 = exp (z1 *. delta_time) in
  let e2 = exp (z2 *. delta_time) in
  let inv_two_zb = 1.0 /. (2.0 *. zb) in
  (* = 1 / (z2 - z1) *)
  let e1_over_two_zb = e1 *. inv_two_zb in
  let e2_over_two_zb = e2 *. inv_two_zb in
  let z1e1_over_two_zb = z1 *. e1_over_two_zb in
  let z2e2_over_two_zb = z2 *. e2_over_two_zb in
  { pos_pos_coef = (e1_over_two_zb *. z2) -. z2e2_over_two_zb +. e2
  ; pos_vel_coef = -.e1_over_two_zb +. e2_over_two_zb
  ; vel_pos_coef = (z1e1_over_two_zb -. z2e2_over_two_zb +. e2) *. z2
  ; vel_vel_coef = -.z1e1_over_two_zb +. z2e2_over_two_zb
  }
;;

let make_underdamped ~delta_time ~angular_freq ~damping_ratio =
  let omega_zeta = angular_freq *. damping_ratio in
  let alpha = angular_freq *. sqrt (1.0 -. (damping_ratio *. damping_ratio)) in
  let exp_term = exp (-.omega_zeta *. delta_time) in
  let cos_term = cos (alpha *. delta_time) in
  let sin_term = sin (alpha *. delta_time) in
  let inv_alpha = 1.0 /. alpha in
  let exp_sin = exp_term *. sin_term in
  let exp_cos = exp_term *. cos_term in
  let exp_omega_zeta_sin_over_alpha = exp_term *. omega_zeta *. sin_term *. inv_alpha in
  { pos_pos_coef = exp_cos +. exp_omega_zeta_sin_over_alpha
  ; pos_vel_coef = exp_sin *. inv_alpha
  ; vel_pos_coef = (-.exp_sin *. alpha) -. (omega_zeta *. exp_omega_zeta_sin_over_alpha)
  ; vel_vel_coef = exp_cos -. exp_omega_zeta_sin_over_alpha
  }
;;

let make_critically_damped ~delta_time ~angular_freq =
  let exp_term = exp (-.angular_freq *. delta_time) in
  let time_exp = delta_time *. exp_term in
  let time_exp_freq = time_exp *. angular_freq in
  { pos_pos_coef = time_exp_freq +. exp_term
  ; pos_vel_coef = time_exp
  ; vel_pos_coef = -.angular_freq *. time_exp_freq
  ; vel_vel_coef = -.time_exp_freq +. exp_term
  }
;;

let make ~delta_time ~angular_freq ~damping_ratio =
  let angular_freq = Float.max 0. angular_freq in
  let damping_ratio = Float.max 0. damping_ratio in
  if angular_freq < epsilon
  then identity
  else if damping_ratio > 1. +. epsilon
  then make_overdamped ~delta_time ~angular_freq ~damping_ratio
  else if damping_ratio < 1. -. epsilon
  then make_underdamped ~delta_time ~angular_freq ~damping_ratio
  else make_critically_damped ~delta_time ~angular_freq
;;

let update spring snapshot ~target_pos =
  let old_pos = snapshot.position -. target_pos in
  (* update in equilibrium relative space *)
  let old_vel = snapshot.velocity in
  let position =
    (old_pos *. spring.pos_pos_coef) +. (old_vel *. spring.pos_vel_coef) +. target_pos
  in
  let velocity = (old_pos *. spring.vel_pos_coef) +. (old_vel *. spring.vel_vel_coef) in
  { position; velocity }
;;
