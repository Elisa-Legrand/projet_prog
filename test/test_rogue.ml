(*test que je n'ai pas réussi à faire marcher à temps*)

open Roguelib
open Ui

(** Type du contenu d'une case du monde. *)
type creature =
  | Empty
  | Cactus


type cell = creature * int

let invalid_id = -1
let width, height = (5, 5)

(* Le système de coordonnées est le suivant :

(0,0)-------------------> + x (width)
  |
  |
  |
  |
  |
  |
  v
 + y (height)

*)

(** Le monde [world] est un tableau mutable. *)
let world : cell array array = Array.make_matrix (width) height (Empty, invalid_id)


(** [set (x,y) v] remplit la case en position [x,y] du monde avec l'entité [v].
    Lève [Exception: Invalid_argument] si la position est hors du monde.*)
let set ((x, y) : int * int) (v : cell) : unit = world.(x).(y) <- v

let () =
for k = 0 to 4 do 
  set (k,0) (Cactus,invalid_id);
  set (k,4) (Cactus,invalid_id);
done;
for k = 1 to 3 do 
  set (0,k) (Cactus,invalid_id);
  set (4,k) (Cactus,invalid_id);
done

