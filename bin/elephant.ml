open Notty_unix
open Ui
open Utils
open Effect
open Effect.Deep
open Engine
open World

type state = Calm | Charge of int * dir | Stunned of int;;

(*detects whether the camel and elephant are on the same line or column, returns Some 'the direction the elephant should run toward' if the Camel is in sight, and None if it's not*)
let straight_line camel_pos elephant_pos = 
  match camel_pos, elephant_pos with
  |(x1,y1),(x2,y2) when x1=x2 -> if y1<y2 then Some Up else Some Down
  |(x1,y1),(x2,y2) when y1=y2 -> if x1<x2 then Some Left else Some Right
  |_ -> None
;;

(*moves the calm elephant according to a direction if possible*)
let move_elephant_calm elephant_pos direction =
  match elephant_pos ++ dir_to_couple direction with
  |(x,y) when get(x,y) = Empty -> move_dir elephant_pos direction
  |(x,y) -> elephant_pos
;;

(*moves the charging elephant where it needs to be if possible. If an entity other than a Cactus is on its way, it will kill it.*)
let move_elephant_charge elephant_pos direction =
  match elephant_pos ++ dir_to_couple direction with
  |(x,y) when get(x,y) = Cactus -> elephant current_position Stunned(21)
  |(x,y) -> squash(x,y) ; new_position = move_dir elephant_pos direction ;
  render ();
  perform End_of_turn;
  new_position
;;

(*defines what the elephant will do depending on its state*)
let rec elephant (current_position : int * int) (current_state : state) =
  match state with
  |Calm -> begin match straight_line camel_pos current_position with
    |None -> elephant (move_elephant current_position random_dir() Calm) Calm
    |Some direction -> elephant current_position Charge(10, direction)
    end
  |Charge(n, direction) when n = 1 -> elephant (move_elephant_charge current_position direction) Calm
  |Charge(n, direction) when n > 1 -> elephant (move_elephant_charge current_position direction) Charge(n-1, direction)
  |Stunned(n) when n = 1 -> perform End_of_turn ; elephant current_position Calm
  |Stunned(n) when n > 1 -> perform End_of_turn ; elephant current_position Stunned(n-1)
;;