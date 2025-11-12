open World

type dir = Up | Down | Right | Left;;

let () = Random.self_init ()
(** Déplacement d'une entité *)

(** Opérateur somme pour les paires d'entiers*)
let ( ++ ) (x, y : int * int) (dx, dy : int * int) : int * int = 
  (x + dx, y + dy)

(** [move old_pos new_pos] déplace le contenu de la case en [old_pos] vers la case [new_pos].
    Si la case [new_pos] est occupé, laisse le monde inchangé.
    Renvoie [new_pos] si le mouvement a eu lieu, et [old_pos] sinon.*)
let move (old_position : int * int) (new_position : int * int) : int * int =
  match get new_position with
  | Empty ->
      let character = get old_position in
      set old_position Empty ;
      set new_position character ;
      new_position   
  | _ -> old_position

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
  |Up ->(1,0)
  |Down -> (-1,0)
  |Right -> (0,1)
  |Left -> (0,-1)

(** [move_dir old_pos direc] déplace le contenu de la case en [old_pos] une case vers la direction [direc].
    Si la case atteinte est occupée, laisse le monde inchangé.
    Renvoie la nouvelle position si le mouvement a eu lieu, et [old_pos] sinon.*)
let move_dir (old_pos : int * int) (direc : dir) : int * int = 
  move old_pos (old_pos ++ (dir_to_couple direc))