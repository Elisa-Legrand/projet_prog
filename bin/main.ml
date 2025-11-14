open Roguelib
open World
open Ui
open Utils
open Player
open Engine
open Snake
open Elephant
open Spider
open Robot
open Vague

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

(* Place le mur de cactus qui sépare des variables*)
let () =
  for k = 0 to height - 1 do
    spawn_pos Cactus (width, k)
  done

(*lance le système de vague, fait apparaitre le chameau et les noms des variables(type Wave:)*)
let () = Queue.add (fun () -> player (fun () -> systeme_vague ())) queue
let () = spawn_camel (random_position ())
let () = init_nom_var ()

(* Début du jeu *)
let () = run_queue ()

(*Fin du jeu*)
