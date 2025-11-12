open World
open Ui
open Utils
open Player
open Engine
open Snake
open Elephant

(* Initialisation du monde *)

(* Initialisation du module Random*)
let () = Random.self_init ()

(** [random_position ()] renvoie une position aléatoire dans le monde*)
let random_position () : int * int = (Random.int width, Random.int height)

(* Place les cactus et le chameau initialement.*)

let () =
  for _ = 0 to 200 do set (random_position ()) (Cactus,0)   done 


let camel_initial_position = random_position ()
let () = camel_pos := camel_initial_position

let () = set camel_initial_position (Camel,prochain_id())
let () = Queue.add (fun () -> player (fun () -> camel camel_initial_position(id_courant() ) )) queue
let snake_initial_position = random_position ()
let () =set snake_initial_position (Snake,prochain_id())

(* La file contient deux chameaux pour tester *)


let () = Queue.add (fun () -> player (fun () -> snake snake_initial_position (id_courant()))) queue
let elephant_initial_position = random_position ()
let () =set elephant_initial_position (Elephant,prochain_id())

(* La file contient deux chameaux pour tester *)


let () = Queue.add (fun () -> player (fun () -> elephant elephant_initial_position Calm (id_courant()))) queue
(* Début du jeu *)
let () = run_queue ()


