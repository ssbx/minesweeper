
let start () =
  Screen.init ();
  Screen.draw ();
  Unix.sleep 2;
  Screen.draw ();
  Screen.destroy ()


