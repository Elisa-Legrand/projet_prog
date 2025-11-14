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
  let x, y = current_position in
  (* Printf.eprintf
    "[SPIDER_EGG] I am a spider_egg with id %d in position (%d, %d) and \
     lifetime %d\n"
    id x y lifetime;   *)
  assert(lifetime <= spider_egg_lifetime);
  begin try
    if lifetime mod spider_egg_cooldown = 0 then
      let pos_baby = get_random_empty_adjacent_cell current_position in
      spawn_spider pos_baby
  with No_adjacent_space -> ()
  end;
  (* Printf.eprintf
    "[SPIDER_EGG] I am a spider_egg with id %d and \
     lifetime %d, and I check if I need to die.\n" id lifetime;  *)
  if lifetime >= spider_egg_lifetime then begin
    (* Printf.eprintf
    "[SPIDER_EGG] I am a spider_egg with id %d and \
     lifetime %d, and I want to kill myself\n" id lifetime;  *)
    assert (get_content current_position = Spider_Egg);
    kill_and_clean current_position
  end
  else if safe_perform id then spider_egg current_position (lifetime + 1) id

and spider (current_position : int * int) id : unit =
  let x, y = current_position in
  (* Printf.eprintf "[SPIDER] I am a spider with id %d in position (%d, %d)\n" id x
    y; *)
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
