open World
open Engine

type dir = Up | Down | Right | Left | Stay

exception No_adjacent_space

(** Déplacement d'une entité *)
let () = Random.self_init ()

(** Opérateur somme pour les paires d'entiers*)
let ( ++ ) ((x, y) : int * int) ((dx, dy) : int * int) : int * int =
  (x + dx, y + dy)

(** [kill id] tue le processus de l'objet d'identifiant [id], et remplace le
    contenu par [Empty]. Entre autres, la fonction place [id] dans [dead_set].
*)
let kill (id : int) : unit = dead_ids := IntSet.add id !dead_ids

(** On construit une fonction de comparaison entre les differentes creatures
    pour savoir qui sachant qu'on peux ecraser les creatures plus faibles que
    nous. [Invalid] est traité comme un cactus invincible car c'est le type des
    cases "extérieures". On empêche par là la sortie des entités. [Empty] est
    traité différement comme c'est une case d'air. On ne l'écrit pas ici. *)

(** Si [crea1] est une clé, et de valeur [crea2; crea3], alors [crea1] peut
    écraser [crea2] et [crea3].*)
let toughness_dict : (creature, creature list) Hashtbl.t =
  let dict = Hashtbl.create 10 in
  begin
    Hashtbl.add dict Invalid [];
    (* Invalid est le type des murs extérieurs *)
    Hashtbl.add dict Cactus [];
    Hashtbl.add dict Elephant [ Snake; Spider; Camel; Spider_Egg ];
    Hashtbl.add dict Snake [ Spider; Spider_Egg ];
    Hashtbl.add dict Spider [ Camel ];
    Hashtbl.add dict Camel [ Snake ];
    Hashtbl.add dict Spider_Egg []
  end;
  dict

(** [can_stomp crea1 crea2] renvoie [true] si crea1 peut écraser crea2. Sinon,
    renvoie [false]. *)
let can_stomp (crea1 : creature) (crea2 : creature) =
  assert (Empty <> crea1 && Invalid <> crea1);
  assert (Empty <> crea2);
  assert (Hashtbl.mem toughness_dict crea1);
  let weaker_creatures = Hashtbl.find toughness_dict crea1 in
  List.mem crea2 weaker_creatures

(** [move old_pos new_pos] déplace le contenu de la case en [old_pos] vers la
    case [new_pos]. Si la case [new_pos] est occupé, si la créature contenue
    dans [old_pos] peut écraser celle en [new_pos] d'après [toughness_dict],
    alors il y a écrasement et déplacement. Sinon rien ne se passe. *)
let move (old_position : int * int) (new_position : int * int) : int * int =
  let crea = get_content old_position in
  match get_content new_position with
  | Empty ->
      let character = get old_position in
      set old_position (Empty, invalid_id);
      set new_position character;
      new_position
  | reached_creature when can_stomp crea reached_creature ->
      let id_tue = get_id new_position in
      kill id_tue;
      let character = get old_position in
      set old_position (Empty, invalid_id);
      set new_position character;
      new_position
  | _ -> old_position

(** [random_dir ()] renvoie une direction cardinale au hasard, ou la direction
    "immobile" *)
let random_dir () : dir =
  let int_new_dir = Random.int 5 in
  match int_new_dir with
  | 0 -> Up
  | 1 -> Down
  | 2 -> Right
  | 3 -> Left
  | _ -> Stay

(** [random_dir_no_stay ()] renvoie une direction cardinale au hasard, mais pas la
    direction "immobile" *)
let random_dir_no_stay () : dir =
  let int_new_dir = Random.int 5 in
  match int_new_dir with 0 -> Up | 1 -> Down | 2 -> Right | _ -> Left

(** [dir_to_couple direc] convertit une direction cardinale en son couple
    d'entiers correspondant. *)
let dir_to_couple (direc : dir) : int * int =
  match direc with
  | Right -> (1, 0)
  | Left -> (-1, 0)
  | Down -> (0, 1)
  | Up -> (0, -1)
  | Stay -> (0, 0)

(** [move_dir old_pos direc] déplace le contenu de la case en [old_pos] une case
    vers la direction [direc]. Si la case atteinte est occupée, si la créature
    contenue dans [old_pos] peut écraser celle atteinte d'après
    [toughness_dict], alors il y a écrasement et déplacement. Sinon rien ne se
    passe. *)
let move_dir (old_pos : int * int) (direc : dir) : int * int =
  move old_pos (old_pos ++ dir_to_couple direc)

(* Compteur privé pour l'identifiant *)
let _id = ref 0

(**renvoie le prochian identifiant libre*)
let prochain_id () : int =
  _id := !_id + 1;
  !_id

let id_courant () : int = !_id
let is_empty (position : int * int) : bool = get position = (Empty, invalid_id)

let get_adjacent_cells ((x, y) : int * int) : (int * int) list =
  List.filter
    (fun (x, y) -> 0 <= x && x < height && 0 <= y && y < width)
    [ (x + 1, y); (x, y + 1); (x - 1, y); (x, y - 1) ]

(** [get_random_empty_adjacent_cell position] renvoie une case adjacente
    aléatoire vide à côté si elle existe. Sinon elle lève l'exception
    [No_adjacent_space].*)
let get_random_empty_adjacent_cell (position : int * int) : int * int =
  let adjacent_cells = Array.of_list (get_adjacent_cells position) in
  let len = Array.length adjacent_cells in
  if len = 0 then raise No_adjacent_space
  else
    let idx = Random.int len in
    adjacent_cells.(idx)

(** [spawn position entity] conjure une [entity] à la case [position], même si
    la case est déjà remplie. *)
