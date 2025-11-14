open Notty_unix
open Ui
open Utils
open Effect
open Effect.Deep
open Engine
open World

let number_of_turn_played = ref 0



(** [keyboard_direction ()] attend un évènement dans le terminal. Si ECHAP est
    pressée, arrête le jeu. Si une touche directionnelle est pressée, renvoie le
    changement à appliquer sur les coordonnées du chameau pour aller dans la
    direction correspondante.*)
let keyboard_direction () : dir =
  match Term.event terminal with
  | `Key (`Escape, _) -> exit 0 (* press <escape> to quit *)
  | `Key (`Arrow `Left, _) -> Left
  | `Key (`Arrow `Right, _) -> Right
  | `Key (`Arrow `Down, _) -> Down
  | `Key (`Arrow `Up, _) -> Up
  | _ -> Stay

let end_of_game score =
  print_string
    "\n\
    \    █████▀█████████████████████\n\
    \    █─▄▄▄▄██▀▄─██▄─▀█▀─▄█▄─▄▄─█\n\
    \    █─██▄─██─▀─███─█▄█─███─▄█▀█\n\
    \    ▀▄▄▄▄▄▀▄▄▀▄▄▀▄▄▄▀▄▄▄▀▄▄▄▄▄▀\n\
    \    ████████████████████████\n\
    \    █─▄▄─█▄─█─▄█▄─▄▄─█▄─▄▄▀█\n\
    \    █─██─██▄▀▄███─▄█▀██─▄─▄█\n\
    \    ▀▄▄▄▄▀▀▀▄▀▀▀▄▄▄▄▄▀▄▄▀▄▄▀\n\
    \    \n
    \ You lost ! Your score is ";
  print_int score;
  print_string "\n\n\n\n";
  exit 0


let power_up = ref 0 

(** [caml current_pos] effectue tous les prochains tours du chameau à partir de
    la position [current_pos] (attendre une entrée, se déplacer en conséquence,
    recommencer)*)
let rec camel (current_position : int * int) (id : int) : unit =

  number_of_turn_played := !number_of_turn_played + 1;
  let dir1 = keyboard_direction() in
    if (get_content (current_position ++ dir_to_couple(dir1))) = Boost then begin
      set current_position (SuperCamel,id); 
      power_up := 20
      end;
    if !power_up <> 0 then power_up := !power_up -1;
    if !power_up = 0 then set current_position (Camel,id);
    update_power_up !power_up;

  let new_position = move_dir current_position (dir1) in
  camel_pos := new_position;
  if safe_perform id then camel new_position id
  else end_of_game !number_of_turn_played

let spawn_camel pos =
  camel_pos := pos;
  camel_is_alive := true;
  let id = prochain_id () in
  set pos (Camel, id);
  Queue.add (fun () -> player (fun () -> camel pos id)) queue
