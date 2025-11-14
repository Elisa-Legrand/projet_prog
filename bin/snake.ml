open Notty_unix
open Ui
open Utils
open Effect
open Effect.Deep
open Engine
open World

let () = Random.self_init ()

(** snake deplace le serpent dans une direction aleatoire à chaque tour*)
let rec snake (current_position : int * int) (id : int) : unit =
  let new_position = move_dir current_position (random_dir ()) in
  if safe_perform id then snake new_position id

(*fait apparaitre un serpent*)
let spawn_snake pos =
  let id = prochain_id () in
  set pos (Snake, id);
  Queue.add (fun () -> player (fun () -> snake pos id)) queue
