
open Roguelib
open Pqueue

let _pq_print pq = 
  let rec deep_print l = match l with
  | [] -> print_newline ()
  | (p, integer)::q -> Printf.printf "(%d, %d)::" p integer; deep_print q in
  deep_print pq.l;
  Printf.printf "Size is %d !\n" pq.size

let _pqueue_add_print pq prio elt =
  Printf.printf "Adding (%d, %d) to pq... Now pq is :\n" prio elt;
  pqueue_add pq prio elt;
  _pq_print pq

let _pqueue_pop_print pq =
  Printf.printf "Popping from pq... Now pq is :\n";
  let popped = pqueue_pop pq in
    (_pq_print pq; popped)

let () =
  let pq = pqueue_create () in
  assert(pqueue_is_empty pq);
  _pqueue_add_print pq 3 12;
  _pqueue_add_print pq 2 15;
  _pqueue_add_print pq 1 12;
  assert(_pqueue_pop_print pq = 12);
  _pqueue_add_print pq 6 17;
  _pqueue_add_print pq 0 5;
  assert(_pqueue_pop_print pq = 5);
  assert(_pqueue_pop_print pq = 15);
  assert(_pqueue_pop_print pq = 17);
  assert(pqueue_is_empty pq);
  print_endline "PQUEUE Tests passed !";
  print_endline "***********************************************************************************"

