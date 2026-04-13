open Graphics
let continue = ref true

let affichniveau niveau x y =
  set_color red;
  set_text_size 5;
  moveto (x+47) (y+20);
  draw_string (string_of_int(niveau));
  moveto (x+23) (y+60);
  draw_string ("Niveau")


let dessin () =
  set_color black;
  (* titre snake *)
  moveto (326) (400);
  set_text_size 1;
  draw_string "SNAKE";

  (* Cases et niveaux *)
  draw_rect (100) (250) (100) (100);
  draw_rect (300) (250) (100) (100);
  draw_rect (500) (250) (100) (100);
  draw_rect (200) (100) (100) (100);
  draw_rect (400) (100) (100) (100);
  set_color green;
  fill_rect (101) (251) (98) (98);
  fill_rect (301) (251) (98) (98);
  fill_rect (501) (251) (98) (98);
  fill_rect (201) (101) (98) (98);
  fill_rect (401) (101) (98) (98);
  affichniveau 1 (100) (250);
  affichniveau 2 (300) (250);
  affichniveau 3 (500) (250);
  affichniveau 4 (200) (100);
  affichniveau 5 (400) (100)




let whereclick () =
  if button_down () then
    let (x,y) = mouse_pos () in
    if x >= 100 && x <= 200 && y >= 250 && y <= 350 then
      begin
        continue := false;
        print_int(1)
      end;
    if x >= 300 && x <= 400 && y >= 250 && y <= 350 then
      begin
        continue := false;
        print_int(2)
      end;
    if x >= 500 && x <= 600 && y >= 250 && y <= 350 then
      begin
        continue := false;
        print_int(3)
      end;
    if x >= 200 && x <= 300 && y >= 100 && y <= 200 then begin
        continue := false;
        print_int(4)
      end;
    if x >= 400 && x <= 500 && y >= 100 && y <= 200 then 
      begin
        continue := false;
        print_int(5)
      end

let test () =
  if key_pressed () then
    match read_key () with
    | 'c' -> continue := false
    | _ -> ()

let () =
  open_graph " 700x500";
  dessin();
  while !continue do
    test ();
    whereclick ()
  done;
  close_graph ();
