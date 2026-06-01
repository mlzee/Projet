open Graphics

let continue = ref true

(* Lecture résolution utilisateur *)
let l = float_of_int (int_of_string (read_line ()))
let h = float_of_int (int_of_string (read_line ()))

(* Scale functions (base 700x500) *)
let sx x =
  int_of_float ((float_of_int x /. 700.) *. l)

let sy y =
  int_of_float ((float_of_int y /. 500.) *. h)

(* Taille des cases en base *)
let cell_w = 100
let cell_h = 100

(* Dessine une case complète *)
let draw_case bx by n =
  let x = sx bx in
  let y = sy by in
  let w = sx (bx + cell_w) - sx bx in
  let h = sy (by + cell_h) - sy by in

  (* fond *)
  set_color green;
  fill_rect x y w h;

  (* bord *)
  set_color black;
  draw_rect x y w h;

  (* texte *)
  set_color red;
  moveto (x + w / 3) (y + h / 3);
  draw_string (string_of_int n)

(* test clic dans une case *)
let inside x y bx by =
  let x0 = sx bx in
  let y0 = sy by in
  let w = sx (bx + cell_w) - sx bx in
  let h = sy (by + cell_h) - sy by in

  x >= x0 && x <= x0 + w &&
  y >= y0 && y <= y0 + h

(* dessin global *)
let dessin () =
  set_color black;

  moveto (sx 300) (sy 420);
  draw_string "SNAKE";

  draw_case 100 250 1;
  draw_case 300 250 2;
  draw_case 500 250 3;
  draw_case 200 100 4;
  draw_case 400 100 5

(* gestion clic *)
let whereclick () =
  let e = wait_next_event [Button_down] in
  let x = e.mouse_x in
  let y = e.mouse_y in

  if inside x y 100 250 then (print_int 1; continue := false)
  else if inside x y 300 250 then (print_int 2; continue := false)
  else if inside x y 500 250 then (print_int 3; continue := false)
  else if inside x y 200 100 then (print_int 4; continue := false)
  else if inside x y 400 100 then (print_int 5; continue := false)

(* gestion clavier *)
let test () =
  if key_pressed () then
    match read_key () with
    | 'c' -> continue := false
    | _ -> ()

(* main *)
let () =
  open_graph (" " ^ string_of_int (int_of_float l) ^ "x" ^ string_of_int (int_of_float h));
  dessin ();

  while !continue do
    test ();
    whereclick ()
  done;

  close_graph ()
