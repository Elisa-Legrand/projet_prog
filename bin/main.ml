open World
open Ui
open Utils
open Player
open Engine
open Snake
open Elephant
open Spider

(* Initialisation du monde *)

(* Initialisation du module Random*)
let () = Random.self_init ()

(** [random_position ()] renvoie une position aléatoire dans le monde*)
let random_position () : int * int = (Random.int width, Random.int height)

let number_of_cacti = 200

(* Place les cactus et le chameau initialement.*)

let () =
  for _ = 0 to number_of_cacti - 1 do
    set (random_position ()) (Cactus, invalid_id)
  done

let spider_initial_position = random_position ()
let () = set spider_initial_position (Spider, prochain_id ())

let () =
  let id = id_courant () in
  Queue.add
    (fun () ->
      player (fun () -> spider spider_initial_position (id)))
    queue
(* La file contient deux chameaux pour tester *)

let snake_initial_position = random_position ()
let () = set snake_initial_position (Snake, prochain_id ())

let () =
  let id = id_courant () in
  Queue.add
    (fun () -> player (fun () -> snake snake_initial_position (id)))
    queue

let elephant_initial_position = random_position ()
let () = set elephant_initial_position (Elephant, prochain_id ())

(* La file contient deux chameaux pour tester *)

let () =
  let id = id_courant () in
  Queue.add
    (fun () ->
      player (fun () -> elephant elephant_initial_position Calm (id)))
    queue

let camel_initial_position = random_position ()
let () = camel_pos := camel_initial_position
let () = set camel_initial_position (Camel, prochain_id ())

let () =
  let id = id_courant () in
  Queue.add
    (fun () -> player (fun () -> camel camel_initial_position (id)))
    queue

(* Début du jeu *)
let () = run_queue ()
