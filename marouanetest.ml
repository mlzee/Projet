open Graphics

type page = Accueil | Niveau | Quitter
type direction = Up | Down | Left | Right
type case = Pomme | Vide | Snake | Obstacle

let niv = ref 0
let actual = ref Accueil

let () = print_endline "Entrez la largeur de la fenêtre (ex: 800) :"
let l = int_of_string (read_line ())
let () = print_endline "Entrez la hauteur de la fenêtre (ex: 600) :"
let h = int_of_string (read_line ())
let str = " " ^ string_of_int l ^ "x" ^ string_of_int h

let rec pgcd a b =
  if b = 0 then a
  else pgcd b (a mod b)

let resizey y =
  int_of_float ((float_of_int y /. 500.) *. float_of_int h)

let resizex x =
  int_of_float ((float_of_int x /. 700.) *. float_of_int l)

let page () =
  clear_graph ();
  let continue = ref true in

  let affichniveau niveau x y =
    set_color red;
    set_text_size 5;
    moveto (resizex x + resizex 47) (resizey y + resizey 20);
    draw_string (string_of_int niveau);
    moveto (resizex x + resizex 23) (resizey y + resizey 60);
    draw_string "Niveau"
  in
  
  let dessin () =
    set_color black;
    moveto (resizex 326) (resizey 400);
    set_text_size 1;
    draw_string "SNAKE";
    
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
    
    affichniveau 1 100 250;
    affichniveau 2 300 250;
    affichniveau 3 500 250;
    affichniveau 4 200 100;
    affichniveau 5 400 100
  in

  dessin ();
  
  while !continue do
    let st = wait_next_event [Button_down] in
    let x, y = st.mouse_x, st.mouse_y in
    
    if x >= resizex 100 && x <= resizex 200 && y >= resizey 250 && y <= resizey 350 then begin
      niv := 1; continue := false; actual := Niveau
    end else if x >= resizex 300 && x <= resizex 400 && y >= resizey 250 && y <= resizey 350 then begin
      niv := 2; continue := false; actual := Niveau
    end else if x >= resizex 500 && x <= resizex 600 && y >= resizey 250 && y <= resizey 350 then begin
      niv := 3; continue := false; actual := Niveau
    end else if x >= resizex 200 && x <= resizex 300 && y >= resizey 100 && y <= resizey 200 then begin
      niv := 4; continue := false; actual := Niveau
    end else if x >= resizex 400 && x <= resizex 500 && y >= resizey 100 && y <= resizey 200 then begin
      niv := 5; continue := false; actual := Niveau
    end
  done

let jeu () =
  clear_graph ();
  Random.self_init ();

  let vitesse n =
    match n with
    | 1 -> Unix.sleepf 0.1
    | 2 -> Unix.sleepf 0.07
    | 3 -> Unix.sleepf 0.06
    | 4 -> Unix.sleepf 0.05
    | 5 -> Unix.sleepf 0.04
    | _ -> ()
  in
  let remplircase grille (a, b) quoi =
    grille.(a).(b) <- quoi
  in
  let cell = (pgcd l h) / 10 in
  let width = l / cell in
  let height = h / cell in
  let score = ref 0 in
  let snake = Queue.create () in
  let start_x, start_y = width / 2, height / 2 in
  Queue.push (start_x, start_y) snake;

  let dir = ref Right in
  let food = ref (Random.int width, Random.int height) in
  let grille = Array.make_matrix width height Vide in

  remplircase grille (start_x, start_y) Snake;
  remplircase grille !food Pomme;

  let rec game_loop (hx, hy) =
    if key_pressed () then begin
      match read_key () with
      | 'z' when !dir <> Down  -> dir := Up
      | 's' when !dir <> Up    -> dir := Down
      | 'q' when !dir <> Right -> dir := Left
      | 'd' when !dir <> Left  -> dir := Right
      | 'm' -> actual := Accueil
      | _ -> ()
    end;
    
    let new_head =
      match !dir with
      | Up    -> (hx, hy + 1)
      | Down  -> (hx, hy - 1)
      | Left  -> (hx - 1, hy)
      | Right -> (hx + 1, hy)
    in
    let (nx, ny) = new_head in
    
    if nx < 0 || ny < 0 || nx >= width || ny >= height || (!actual = Accueil) then begin
      actual := Accueil
    end
    else if grille.(nx).(ny) = Snake || grille.(nx).(ny) = Obstacle then begin
      actual := Accueil
    end
    else if grille.(nx).(ny) = Pomme then begin
      score := !score + 1;
      set_color white;
      fill_rect 10 (h - 20) (cell * 10) (cell * 2);
      Queue.push new_head snake;
      grille.(nx).(ny) <- Snake;
      food := (Random.int width, Random.int height);
      remplircase grille !food Pomme;
      set_color black;
      moveto 10 (h - 20);
      draw_string ("Score : " ^ string_of_int !score);
      
      set_color green;
      fill_rect (nx * cell) (ny * cell) cell cell;
      set_color red;
      let (fx, fy) = !food in
      fill_rect (fx * cell) (fy * cell) cell cell;
      
      vitesse !niv;
      game_loop new_head
    end
    else begin
      Queue.push new_head snake;
      grille.(nx).(ny) <- Snake;

      let (qx, qy) = Queue.pop snake in
      grille.(qx).(qy) <- Vide;

      set_color white;
      fill_rect (qx * cell) (qy * cell) cell cell;

      set_color green;
      fill_rect (nx * cell) (ny * cell) cell cell;
      set_color red;
      let (fx, fy) = !food in
      fill_rect (fx * cell) (fy * cell) cell cell;

      vitesse !niv;
      game_loop new_head
    end
  in

  let rec attente () =
    if key_pressed () then begin
      match read_key () with
      | 'd' -> 
         begin
           clear_graph ();
           game_loop (start_x, start_y)
         end
      | _ -> attente ()
    end else begin
      Unix.sleepf 0.05;
      attente ()
    end
  in
  
  set_color black;
  moveto (l / 2 - (resizex 50)) (h / 2 + (resizey 50));
  draw_string "Appuyez sur 'd' pour commencer";
  attente ()

let () =
  open_graph str;

  while !actual <> Quitter do
    match !actual with
    | Accueil -> page ()
    | Niveau -> jeu ()
    | Quitter -> ()
  done;

  close_graph ()
