open World
open Engine

type dir = Up | Down | Right | Left
exception No_adjacent_space

let () = Random.self_init ()
(** Déplacement d'une entité *)

(** Opérateur somme pour les paires d'entiers*)
let ( ++ ) (x, y : int * int) (dx, dy : int * int) : int * int = 
  (x + dx, y + dy)

(** [kill id] tue le processus de l'objet d'identifiant [id], 
    et remplace le contenu par [Empty].
    Entre autres, la fonction place [id] dans [dead_set]. *)
let kill (id : int) : unit =
  dead_ids := IntSet.add id !dead_ids
(** [move old_pos new_pos] déplace le contenu de la case en [old_pos] vers la case [new_pos].
    Si la case [new_pos] est occupé, laisse le monde inchangé.
    Renvoie [new_pos] si le mouvement a eu lieu, et [old_pos] sinon.*)


(**fonction de comparaison entre les differentes creatures pour savoir qui 
sachant qu'on peux ecraser les creatures plus faibles que nous*)
let liste_plus_au_moins_fort = [Invalid;Cactus;Elephant;Spider;Snake;Camel;Spider_Egg;]
let weaker crea1 crea2 =
  let rec aux l =
  match l with
  |[]->failwith "comparaison de deux empty"
  |h::_ when h=crea1 ->false
  |h::_ when h=crea2 -> true
  |_::t -> aux t
  in
  aux liste_plus_au_moins_fort

let move (crea : creature) (old_position : int * int) (new_position : int * int) : int * int =
  match get new_position with
  | Empty,_ ->
      let character = get old_position in
      set old_position (Empty,0) ;
      set new_position character ;
      new_position   
  |crea_renc,_ when weaker crea crea_renc-> old_position
  |_-> let x,y =new_position in
       let (_,id_tue) = world.(x).(y) in 
       kill id_tue;
       let character = get old_position in
       set old_position (Empty,0) ;
       set new_position character ;
       new_position  


(** [random_dir ()] renvoie une direction cardinale au hasard.*)
let random_dir () : dir =
  let int_new_dir = (Random.int 4) in
  match int_new_dir with
  |0 -> Up
  |1 -> Down
  |2-> Right
  |_->Left

(** [dir_to_couple direc] convertit une direction cardinale en son couple d'entiers correspondant. *)
let dir_to_couple (direc : dir) : (int * int) =
  match direc with
  |Up ->(-1,0)
  |Down -> (1,0)
  |Right -> (0,1)
  |Left -> (0,-1)

(** [move_dir old_pos direc] déplace le contenu de la case en [old_pos] une case vers la direction [direc].
    Si la case atteinte est occupée, laisse le monde inchangé.
    Renvoie la nouvelle position si le mouvement a eu lieu, et [old_pos] sinon.*)
let move_dir (crea :creature)(old_pos : int * int) (direc : dir) : int * int = 
  move crea old_pos (old_pos ++ (dir_to_couple direc))


let _id = ref 0

(**renvoie le prochian identifiant libre*)
let prochain_id():int =
  _id := !_id+1;
  !_id

let id_courant() :int =
  !_id

let is_empty (position : int * int) : bool = (get position) = (Empty,0)

let get_adjacent_cells (x, y : int * int) : (int * int) list =
  List.filter (fun (x, y) -> 0 <= x && x < height && 0 <= y && y < width)
  [(x+1,y); (x, y+1); (x-1,y); (x, y-1)]

(** [get_random_empty_adjacent_cell position] renvoie une case adjacente aléatoire vide à côté si elle existe.
    Sinon elle lève l'exception [No_adjacent_space].*)
let get_random_empty_adjacent_cell (position : int * int) : int * int =
  let adjacent_cells = Array.of_list (get_adjacent_cells position) in
  let len = Array.length adjacent_cells in
  if len = 0 then raise No_adjacent_space 
  else
    let idx = Random.int len in adjacent_cells.(idx)



