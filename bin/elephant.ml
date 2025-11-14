open Notty_unix
open Ui
open Utils
open Effect
open Effect.Deep
open Engine
open World

type state = Calm | Charge of int * dir | Stunned of int

let time_charging = 10
let cooldown_cactus = 20
(* let () = Random.self_init () *)

(**renvoye|x-y|*)
let dist (x : int) (y : int) : int = if x > y then x - y else y - x

(**renvoie vrai si il y'a un cactus entre x,y1 et x,y2 si xco1 
                                          y1,x et y2,x sinon*)
let cactus_entre (x_co1:bool) (x:int) (y1:int) (y2:int) :bool=
    let b =ref false in
    let start = ref 0 in
    (if y1>y2 then 
      start := y2
    else
      start := y1);
    for k = !start to !start +(dist y1 y2) do
        let (crea,_)= if x_co1 then get (x, k) else get (k,x) in
        b:=!b || crea = Cactus
    done;
    !b

(*detects whether the camel and elephant are on the same line or column,
returns Some 'the direction the elephant should run toward' if the Camel is in sight,
and None if it's not*)
let straight_line_to_camel elephant_pos =
  match (!camel_pos, elephant_pos) with
  | (x1, y1), (x2, y2) when x1 = x2 && (dist y1 y2) <=10 && not(cactus_entre true x1 y1 y2)-> if y1 < y2 then Some Up else Some Down
  | (x1, y1), (x2, y2) when y1 = y2 && (dist x1 x2) <=10 && not(cactus_entre false y1 x1 x2)-> if x1 < x2 then Some Left else Some Right
  | _ -> None

(*defines what the elephant will do depending on its state*)
let rec elephant (current_position : int * int) (current_state : state)
    (id : int) : unit =
  match current_state with
  | Calm -> begin
      match straight_line_to_camel current_position with
      | None ->
          let new_pos = move_dir current_position (random_dir ()) in
          if safe_perform id then elephant new_pos Calm id
      | Some direction ->
          set current_position (Angry_Elephant, id);
          elephant current_position (Charge (time_charging, direction)) id
    end
  | Charge (n, direction) when n = 1 ->
      let new_pos, stun = move_elephant_charge current_position direction in
      if stun then begin
        set current_position (Stunned_Elephant, id);
        elephant current_position (Stunned (cooldown_cactus + 1)) id
      end
      else if safe_perform id then begin
        set new_pos (Elephant, id);
        elephant new_pos Calm id
      end
  | Charge (n, direction) when n > 1 ->
      let new_pos, stun = move_elephant_charge current_position direction in
      if stun then begin
        set current_position (Stunned_Elephant, id);
        elephant current_position (Stunned (cooldown_cactus + 1)) id
      end
      else if safe_perform id then
        elephant new_pos (Charge (n - 1, direction)) id
  | Stunned n when n = 1 ->
      if safe_perform id then begin
        set current_position (Elephant, id);
        elephant current_position Calm id
      end
  | Stunned n when n > 1 ->
      if safe_perform id then elephant current_position (Stunned (n - 1)) id
  | _ -> failwith "strange"

(*moves the charging elephant where it needs to be if possible.
If an entity other than a Cactus is on its way, it will kill it.*)
and move_elephant_charge (elephant_pos : int * int) (direction : dir) :
    (int * int) * bool =
  match elephant_pos ++ dir_to_couple direction with
  | x, y -> (
      match get_content (x, y) with
      | Cactus | Invalid -> (elephant_pos, true)
      | _ ->
          let new_pos = move_dir elephant_pos direction in
          (new_pos, false))

let spawn_elephant pos =
  let id = prochain_id () in
  set pos (Elephant, id);
  Queue.add (fun () -> player (fun () -> elephant pos Calm id)) queue
