open Utils
open Effect
open Effect.Deep
open Engine
open World

let spider_egg_cooldown = 20 (* en nombre de tours *)
let spider_egg_lifetime = 60

(* [spider_egg current_position cooldown lifetime] effectue tous les
prochains tours du sac d'oeufs depuis [current_pos] (poser une araignée sur
une case adjacente si possible, ou disparaître, ou rien faire). Si [lifetime]
est un multiple de [spider_egg_cooldown], une araignée apparaît si possible à
côté. Si [lifetime = spider_egg_lifetime], le sac d'oeufs disparaît. *)
let rec spider_egg (current_position : int * int) (lifetime : int) id : unit =
  if lifetime mod 20 = 0 then (
      let pos_baby = get_random_empty_adjacent_cell current_position in
  (set pos_baby (Spider, prochain_id ()) ;
  Queue.add
    (fun () -> player (fun () -> spider pos_baby (id_courant ())))
    queue)
  ) ;
  if lifetime >= 60 then kill id ;
  if safe_perform id then spider_egg current_position (lifetime + 1) id
    
and spider (current_position : int * int) id : unit =
  let (new_position : int * int) =
    move_dir current_position (random_dir ())
  in
  let spawn_egg_bool = Random.int 100 = 0 in
  if spawn_egg_bool then (
    let pos_egg = get_random_empty_adjacent_cell current_position in
    set pos_egg (Spider_Egg, prochain_id ()) ;
    Queue.add
      (fun () -> player (fun () -> spider_egg pos_egg 1 (id_courant ())))
      queue
  );
  if safe_perform id then spider new_position id

  