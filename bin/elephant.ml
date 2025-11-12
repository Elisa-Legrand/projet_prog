open Notty_unix
open Ui
open Utils
open Effect
open Effect.Deep
open Engine
open World

type state = Calm | Charge of int * dir | Stunned of int;;

let time_charging = 10;;
let cooldown_cactus = 20;;

let () = Random.self_init ()

(*detects whether the camel and elephant are on the same line or column,
returns Some 'the direction the elephant should run toward' if the Camel is in sight,
and None if it's not*)
let straight_line camel_pos elephant_pos = 
  match !camel_pos, elephant_pos with
  |(x1,y1),(x2,y2) when x1=x2 -> if y1<y2 then Some Left else Some Right
  |(x1,y1),(x2,y2) when y1=y2 -> if x1<x2 then Some Up else Some Down
  |_ -> None
;;

(*defines what the elephant will do depending on its state*)
let rec elephant (current_position : int * int) (current_state : state) =
  match current_state with
  |Calm -> begin match straight_line camel_pos current_position with
    |None -> let new_pos = move_dir current_position (random_dir()) in
      render () ; perform End_of_turn ;
      elephant new_pos Calm
    |Some direction -> elephant current_position (Charge(time_charging, direction))
    end
  |Charge(n, direction) when n = 1 -> elephant (move_elephant_charge current_position direction) Calm
  |Charge(n, direction) when n > 1 -> elephant (move_elephant_charge current_position direction) (Charge(n-1, direction))
  |Stunned(n) when n = 1 -> perform End_of_turn ; elephant current_position Calm
  |Stunned(n) when n > 1 -> perform End_of_turn ; elephant current_position (Stunned(n-1))
(*moves the charging elephant where it needs to be if possible.
If an entity other than a Cactus is on its way, it will kill it.*)
and move_elephant_charge elephant_pos direction =
  match elephant_pos ++ dir_to_couple direction with
  |(x,y)-> match get (x, y) with
    |(Cactus,)
  
  
  (* get(x,y) = (Cactus, i) -> elephant elephant_pos (Stunned(cooldown_cactus+1))
  |(x,y) -> kill (x,y)  ; let new_position = move_dir elephant_pos direction in
  render ();
  perform End_of_turn;
  new_position
;;

;;