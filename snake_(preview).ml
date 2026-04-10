open Graphics

let width = 800
let height = 600
let cell_size = 20

type direction = Up | Down | Left | Right
type point = { x : int; y : int }

let () =
  open_graph (Printf.sprintf " %dx%d" width height);
  set_window_title "Snake Game";
  
  let snake = ref [{ x = 10; y = 10 }; { x = 9; y = 10 }; { x = 8; y = 10 }] in
  let direction = ref Right in
  let next_direction = ref Right in
  let food = ref { x = 20; y = 15 } in
  let score = ref 0 in
  let game_over = ref false in
  
  let draw_cell x y color =
    set_color color;
    fill_rect (x * cell_size) (y * cell_size) cell_size cell_size;
    set_color black;
    draw_rect (x * cell_size) (y * cell_size) cell_size cell_size
  in
  
  let draw_snake () =
    List.iter (fun p -> draw_cell p.x p.y green) !snake
  in
  
  let draw_food () =
    draw_cell !food.x !food.y red
  in
  
  let draw_score () =
    set_color black;
    moveto 10 580;
    draw_string ("Score : " ^ string_of_int !score)
  in
  
  let move_snake () =
    direction := !next_direction;
    let head = List.hd !snake in
    let new_head = match !direction with
      | Up -> { x = head.x; y = head.y + 1 }
      | Down -> { x = head.x; y = head.y - 1 }
      | Left -> { x = head.x - 1; y = head.y }
      | Right -> { x = head.x + 1; y = head.y }
    in
    
    if new_head.x < 0 || new_head.x >= width / cell_size ||
       new_head.y < 0 || new_head.y >= height / cell_size then
      game_over := true
    else if List.mem new_head !snake then
      game_over := true
    else (
      snake := new_head :: !snake;
      if new_head = !food then (
        incr score;
        food := { x = Random.int (width / cell_size); y = Random.int (height / cell_size) }
      ) else
        snake := List.rev (List.tl (List.rev !snake))
    )
  in
  
 (* let handle_input () =
    if key_pressed () then
      match read_key () with
      | 'z' | 'Z' when !direction <> Down -> next_direction := Up
      | 's' | 'S' when !direction <> Up -> next_direction := Down
      | 'q' | 'Q' when !direction <> Right -> next_direction := Left
      | 'd' | 'D' when !direction <> Left -> next_direction := Right
      | 'a' | 'A' -> game_over := true
      | 'p' | 'P' -> close_graph ()
      | 'r' | 'R' -> game_loop ()
      | _ -> ()
  in *)
  
  let rec game_loop () =
    clear_graph ();
    set_color white;
    fill_rect 0 0 width height;
    
    let handle_input () =
    if key_pressed () then
      match read_key () with
      | 'z' | 'Z' when !direction <> Down -> next_direction := Up
      | 's' | 'S' when !direction <> Up -> next_direction := Down
      | 'q' | 'Q' when !direction <> Right -> next_direction := Left
      | 'd' | 'D' when !direction <> Left -> next_direction := Right
      | 'a' | 'A' -> game_over := true
      | 'p' | 'P' -> close_graph ()
      | _ -> ()
  in
    
    handle_input ();
    move_snake ();
    
    draw_snake ();
    draw_food ();
    draw_score ();
    
    if not !game_over then 
    (
      Unix.sleepf 0.1;
      game_loop ()
    )
    else 
    (
      set_color black;
      moveto 300 300;
      draw_string ("Game Over! Final Score  : " ^ string_of_int !score);
      ignore (read_key ())
    )
  in
  
  game_loop ();
