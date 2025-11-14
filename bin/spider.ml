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
  try
    if lifetime mod spider_egg_cooldown = 0 then begin
      let pos_baby = get_random_empty_adjacent_cell current_position in
      spawn_spider pos_baby
    end
  else ();
    if lifetime >= spider_egg_lifetime then begin kill id;
                                      set current_position (Empty,invalid_id)
                                      end;
    if safe_perform id then spider_egg current_position (lifetime+1) id
  with |No_adjacent_space ->
    if lifetime >= spider_egg_lifetime then begin kill id;
                                        set current_position (Empty,invalid_id);
                                        end;
    if safe_perform id then spider_egg current_position (lifetime) id
    

and spider (current_position : int * int) id : unit =
  let (new_position : int * int) = move_dir current_position (random_dir ()) in
  let spawn_egg_bool = Random.int 100 = 0 in
  (if spawn_egg_bool then
     try
       let pos_egg = get_random_empty_adjacent_cell new_position in
       spawn_spider_egg pos_egg
     with No_adjacent_space -> ());
  if safe_perform id then spider new_position id

and spawn_spider pos =
  let id = prochain_id () in
  set pos (Spider, id);
  Queue.add (fun () -> player (fun () -> spider pos id)) queue

and spawn_spider_egg pos =
  let id = prochain_id () in
  set pos (Spider_Egg, id);
  Queue.add (fun () -> player (fun () -> spider_egg pos 1 id)) queue
