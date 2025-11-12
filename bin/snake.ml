open Notty_unix
open Ui
open Utils
open Effect
open Effect.Deep
open Engine

let () = Random.self_init ()

let rec snake (current_position : int *int):unit =
  let new_position = move_dir current_position random_dir() in
  render ();
  perform End_of_turn;
  snake new_position