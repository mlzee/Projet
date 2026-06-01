open Graphics
let continue = ref true

let l = int_of_string(read_line())
let h= int_of_string(read_line())

let resizey y =
   int_of_float((float_of_int(y)/.500.)*. float_of_int(h))

let resizex x =
   int_of_float((float_of_int(x)/.700.)*. float_of_int(l))


let affichniveau niveau x y =
  set_color red;
  set_text_size 5;
  moveto ((resizex x) + (resizex 47)) ((resizey y) + (resizey 20));
  draw_string (string_of_int(niveau));
  moveto ((resizex x) + (resizex 23)) ((resizey y) + (resizey 60));
  draw_string ("Niveau")



let dessin () =
  set_color black;
  (* titre snake *)
  moveto (resizex 326) (resizey 400);
  set_text_size 1;
  draw_string "SNAKE";

  (* Cases et niveaux *)
  draw_rect (resizex 100) (resizey 250) (resizex 100) (resizey 100);
  draw_rect (resizex 300) (resizey 250) (resizex 100) (resizey 100);
  draw_rect (resizex 500) (resizey 250) (resizex 100) (resizey 100);
  draw_rect (resizex 200) (resizey 100) (resizex 100) (resizey 100);
  draw_rect (resizex 400) (resizey 100) (resizex 100) (resizey 100);
  set_color green;
  fill_rect (resizex 101) (resizey 251) (resizex 98) (resizey 98);
  fill_rect (resizex 301) (resizey 251) (resizex 98) (resizey 98);
  fill_rect (resizex 501) (resizey 251) (resizex 98) (resizey 98);
  fill_rect (resizex 201) (resizey 101) (resizex 98) (resizey 98);
  fill_rect (resizex 401) (resizey 101) (resizex 98) (resizey 98);
  affichniveau 1 (100) (250);
  affichniveau 2 (300) (250);
  affichniveau 3 (500) (250);
  affichniveau 4 (200) (100);
  affichniveau 5 (400) (100)




let whereclick () =
  if button_down () then
    let (x,y) = mouse_pos () in
    if x >= resizex 100 && x <= resizex 200 && y >= resizey 250 && y <= resizey 350 then
      begin
        continue := false;
        print_int(1)
      end;
    if x >= resizex 300 && x <= resizex 400 && y >= resizey 250 && y <= resizey 350 then
      begin
        continue := false;
        print_int(2)
      end;
    if x >= resizex 500 && x <= resizex 600 && y >= resizey 250 && y <= resizey 350 then
      begin
        continue := false;
        print_int(3)
      end;
    if x >= resizex 200 && x <= resizex 300 && y  >= resizey 100 && y <= resizey 200 then begin
        continue := false;
        print_int(4)
      end;
    if x >= resizex 400 && x <= resizex 500 && y >= resizey 100 && y <= resizey 200 then
      begin
        continue := false;
        print_int(5)
      end

let test () =
  if key_pressed () then
    match read_key () with
    | 'c' -> continue := false
    | _ -> ()

let str = " " ^ string_of_int(l) ^ "x" ^ string_of_int(h)

let () =
  open_graph str;
  dessin();
  while !continue do
    test ();
    whereclick ()
  done;
  close_graph ();
