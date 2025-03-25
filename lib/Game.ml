open Engine
let rec loop view (scr : BackEnd.Screen.t) n =
  if n <> 0 then (
    BackEnd.Screen.clear scr;
    View.draw scr.r view;
    BackEnd.Screen.present scr;
    loop view scr (n - 1)
  )

let start () =
  let w = 1240
  and h = 780 in
  let scr  = BackEnd.Screen.init ~w ~h in
  let imgs = Assets.Images.init scr.r in
  let view = View.init ~w ~h scr.r in
  loop view scr 100;
  Unix.sleep 1;
  Assets.Images.destroy imgs;
  View.destroy view;
  BackEnd.Screen.destroy scr


