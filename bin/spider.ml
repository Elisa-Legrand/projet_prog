open Utils
open Effect
open Effect.Deep
open Engine

let spider_egg_cooldown = 20 (* en nombre de tours *)
let spider_egg_lifetime = 60

let spawn_egg_nearby position =
  let pos_egg = get_random_empty_adjacent_cell in

;;

let spawn_spider_nearby position =
  let pos_baby = get_random_empty_adjacent_cell in
  
;;

(** [spider_egg current_position cooldown lifetime] effectue tous les prochains tours du sac d'oeufs
    depuis [current_pos] (poser une araignée sur une case adjacente si possible, ou disparaître, ou rien faire).
    Si [lifetime] is a multiple of [spider_egg_cooldown], une araignée apparaît si possible à côté.
    Si [lifetime = spider_egg_lifetime], le sac d'oeufs disparaît. *)
let rec spider_egg (current_position : int * int) (lifetime : int) : unit = 
  if lifetime mod 20 = 0 then spawn_egg_nearby current_position;
  if lifetime >= 60 then kill current_position;
  spider_egg current_position (lifetime + 1)
;;

let rec spider (current_position :int * int) : unit =
  move_dir current_position random_dir;
  let spawn_egg_bool = (Random.int 100 = 0) in
  if spawn_egg_bool then spawn_egg_nearby;
  render ();
  perform End_of_turn
;;
