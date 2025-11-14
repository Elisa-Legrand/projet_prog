open World
open Ui
open Utils
open Player
open Engine
open Snake
open Elephant
open Spider
open Robot

(* Initialisation du monde *)

(* Initialisation du module Random *)
let () = Random.self_init ()

(** [random_position ()] renvoie une position aléatoire dans le monde*)
let random_position () : int * int = (Random.int width, Random.int height)

let number_of_cacti = 200

(* Place les cactus initialement. *)

let () =
  for _ = 0 to number_of_cacti - 1 do
    set (random_position ()) (Cactus, invalid_id)
  done

let () = spawn_spider (random_position ())
let () = spawn_snake (random_position ())
let () = spawn_elephant (random_position ())
let () = spawn_robot (random_position ())
let () = spawn_camel (random_position ())

(* Début du jeu *)
let () = run_queue ()



(*Fin du jeu*)
