open Utils 
open Effect 
open Effect.Deep 
open Engine
let spider_egg_cooldown = 20 (* en nombre de tours *) 
let spider_egg_lifetime = 60
let spawn_egg_nearby position = 
   let pos_egg = get_random_empty_adjacent_cell position in
   spawn pos_egg Spider_Egg ;
;;

let spawn_spider_nearby position = 
   let pos_baby = get_random_empty_adjacent_cell position in
   spawn pos_baby Spider
;;

(* [spider_egg current_position cooldown lifetime] effectue tous les
prochains tours du sac d'oeufs depuis [current_pos] (poser une araignée sur
une case adjacente si possible, ou disparaître, ou rien faire). Si [lifetime]
est un multiple de [spider_egg_cooldown], une araignée apparaît si possible à
côté. Si [lifetime = spider_egg_lifetime], le sac d'oeufs disparaît. *) 
let rec spider_egg (current_position : int * int) (lifetime : int) id : unit = 
   if lifetime mod 20 = 0 
      then spawn_egg_nearby current_position; 
   if lifetime >= 60
      then kill id; 
   if safe_perform(id) then
   spider_egg current_position (lifetime + 1) id
;;

let rec spider (current_position :int * int) id : unit = 
   let (new_position : int * int) = move_dir Spider current_position (random_dir()) in
   let spawn_egg_bool = (Random.int 100 = 0) in 
   if spawn_egg_bool 
      then spawn_egg_nearby new_position;
   if safe_perform(id)then
      spider new_position id
;;
         