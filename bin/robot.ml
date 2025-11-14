(**open Utils
open Engine
open World
let rec robot current_position id =
  let move_randomly () =
    move_dir current_position (random_dir ()) in

  let next_position = if !camel_is_alive then
      match a_star_get_next_cell Robot current_position !camel_pos with
      | next_position -> move current_position next_position
      | exception No_path_found -> move_randomly ()
    else move_randomly () in
  if safe_perform id then robot next_position id

let spawn_robot pos =
  let id = prochain_id () in
  set pos (Robot, id);
  Queue.add (fun () -> player (fun () -> robot pos id)) queue*)