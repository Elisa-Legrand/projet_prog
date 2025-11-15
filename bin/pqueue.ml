exception Empty_priority_queue

type 'a pqueue = { mutable size : int; mutable l : (int * 'a) list }
(** Implémentation d'une file de priorité naïve avec une liste *)

(** Renvoie une file de priorité vide. *)
let pqueue_create () = { size = 0; l = [] }

let pqueue_is_empty pq = pq.size = 0
let pqueue_mem pq elt = List.exists (fun (_, e) -> e = elt) pq.l

let _pqueue_update_priority pq prio elt =
  let rec explore l =
    match l with
    | [] -> failwith "Element to update isn't there"
    | (_, e) :: q when e = elt -> (prio, e) :: q
    | (p, e) :: q -> (p, e) :: explore q
  in
  pq.l <- explore pq.l

(** [pqueue_add pq prio elt] ajoute l'élément [elt] à la file avec priorité
    [prio]. Si l'élément est déjà dans la file, la priorité est mise à jour à
    [prio].*)
let pqueue_add pq prio elt =
  if pqueue_mem pq elt then (* update priority *)
    _pqueue_update_priority pq prio elt
  else begin
    pq.size <- pq.size + 1;
    pq.l <- (prio, elt) :: pq.l
  end

(** [_pqueue_list_find_min l] donne le couple de priorité minimale le plus à
    gauche dans [l] *)
let rec _pqueue_list_find_min (l : (int * 'a) list) : int * 'a =
  match l with
  | [] -> failwith "Priotity queue list is empty but size != 0"
  | [ (p, elt) ] -> (p, elt)
  | (p, elt1) :: q ->
      let q, elt2 = _pqueue_list_find_min q in
      if p <= q then (p, elt1) else (q, elt2)

(** [pqueue_pop pq] renvoie l'élément de priorité minimale de [pq] et le retire
    de [pq]. Lève [Empty_priority_queue] si la file est vide.*)
let pqueue_pop pq =
  if pq.size = 0 then raise Empty_priority_queue
  else
    let smallest_prio, min_elt = _pqueue_list_find_min pq.l in
    let rec remove_found_smallest l =
      match l with
      | [] ->
          failwith "Did not find the found element from _pqueue_list_find_min"
      | (p, elt) :: q ->
          if p = smallest_prio then begin
            assert (elt = min_elt);
            q
          end
          else (p, elt) :: remove_found_smallest q
    in
    pq.l <- remove_found_smallest pq.l;
    pq.size <- pq.size - 1;
    min_elt
