open Utils
open Effect
open Effect.Deep
open Engine

let rec spider (current_position :int * int) :unit =
  let 