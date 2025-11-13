open Notty_unix
open Ui
open Utils
open Effect
open Effect.Deep
open Engine

let () = Random.self_init ()

(** snake deplace le serpent dans une direction aleatoire à chaque tour*)
let rec snake (current_position : int * int) (id : int) : unit =
  let new_position = move_dir Snake current_position (random_dir ()) in
  render ();
  if safe_perform id then snake new_position id
