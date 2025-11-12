open Notty_unix
open Ui
open Utils
open Effect
open Effect.Deep
open Engine
open World

type state = Calm | Charge of int * dir | Stunned of int;;

(*detects whether the camel and elephant are on the same line or column, returns Some 'the direction the elephant should run toward if need be', and None*)
let straight_line camel_pos elephant_pos = 
  match camel_pos, elephant_pos with
  |(x1,y1),(x2,y2) when x1=x2 -> if y1<y2 then Some Up else Some Down
  |(x1,y1),(x2,y2) when y1=y2 -> if x1<x2 then Some Left else Some Right
  |_ -> None
;;

(*moves the elephant where it needs to be*)
let moving_charge current_position direction =
  let new_position = move_dir current_position direction ;
  match new_position with
  |(x,y) when world.(x).(y) = cactus -> elephant current_position Stunned(21)
  render ();
  perform End_of_turn;
  new_position
;;

let rec elephant (current_position : int * int) (current_state : state) =
  match state with
  |Calm -> begin match straight_line camel_pos current_position with
    |None ->
    |Some direction -> elephant current_position Charge(10, direction)
    end
  |Charge(n, direction) when n = 1 -> elephant (moving_charge current_position direction) Calm
  |Charge(n, direction) when n > 1 -> elephant (moving_charge current_position direction) Charge(n-1, direction)
  |Stunned(n) when n = 1 -> perform End_of_turn ; elephant current_position Calm
  |Stunned(n) when n > 1 -> perform End_of_turn ; elephant current_position Stunned(n-1)
;;