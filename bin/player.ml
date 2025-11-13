open Notty_unix
open Ui
open Utils
open Effect
open Effect.Deep
open Engine

let number_of_turn_played = ref 0;;

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
;;

let end_of_game score =
  print_string("
    █████▀█████████████████████\n
    █─▄▄▄▄██▀▄─██▄─▀█▀─▄█▄─▄▄─█\n
    █─██▄─██─▀─███─█▄█─███─▄█▀█\n
    ▀▄▄▄▄▄▀▄▄▀▄▄▀▄▄▄▀▄▄▄▀▄▄▄▄▄▀\n
    ████████████████████████\n
    █─▄▄─█▄─█─▄█▄─▄▄─█▄─▄▄▀█\n
    █─██─██▄▀▄███─▄█▀██─▄─▄█\n
    ▀▄▄▄▄▀▀▀▄▀▀▀▄▄▄▄▄▀▄▄▀▄▄▀\n
    \n
    You lost ! Your score is ") ;
  print_int(score)
;;
(** [caml current_pos] effectue tous les prochains tours du chameau à partir de
    la position [current_pos] (attendre une entrée, se déplacer en conséquence,
    recommencer)*)
let rec camel (current_position : int * int) (id : int) : unit =
  number_of_turn_played := !number_of_turn_played + 1 ;
  let new_position = move_dir current_position (keyboard_direction ()) in
  camel_pos := new_position;
  render ();
  if safe_perform id then camel new_position id else end_of_game !number_of_turn_played 
