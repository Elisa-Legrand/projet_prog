open World
open Engine
open Pqueue

type dir = Up | Down | Right | Left | Stay

exception No_adjacent_space
exception No_path_found

(** Déplacement d'une entité *)
let () = Random.self_init ()

(** Opérateur somme pour les paires d'entiers*)
let ( ++ ) ((x, y) : int * int) ((dx, dy) : int * int) : int * int =
  (x + dx, y + dy)

(** [_kill id] tue le processus de l'objet d'identifiant [id]. Entre autres, la
    fonction place [id] dans [dead_set]. Ne CLEAN pas le sprite. Doit être
    utilisé seulement par [move] et [kill_and_clean] *)
let _kill (id : int) : unit = dead_ids := IntSet.add id !dead_ids

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
    Hashtbl.add dict Angry_Elephant [ Snake; Spider; Camel; Spider_Egg; Robot ];
    Hashtbl.add dict Stunned_Elephant [];
    Hashtbl.add dict Snake [ Spider; Spider_Egg; Camel ];
    Hashtbl.add dict Spider [ Camel ];
    Hashtbl.add dict Camel [ Stunned_Elephant; Spider_Egg; Boost ];
    Hashtbl.add dict Robot
      [ Snake; Stunned_Elephant; Spider_Egg; Elephant; Spider; Camel ];
    Hashtbl.add dict Spider_Egg [];
    Hashtbl.add dict Empty [];
    Hashtbl.add dict SuperCamel
      [
        Spider;
        Spider_Egg;
        Snake;
        Elephant;
        Angry_Elephant;
        Stunned_Elephant;
        Robot;
        Boost;
      ]
  end;
  dict

(*tue l'entité en pos et efface son sprite*)
let kill_and_clean pos =
  let id = get_id pos in
  _kill id;
  set pos (Empty, invalid_id)

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
      _kill id_tue;
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

(** [random_dir_no_stay ()] renvoie une direction cardinale au hasard, mais pas
    la direction "immobile" *)
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
  incr _id;
  !_id

let id_courant () : int = !_id
let is_empty (position : int * int) : bool = get position = (Empty, invalid_id)

(*renvoie la liste des cases adjacentes (si en bord de plateau n'en renvoie pas 4)*)
let get_adjacent_cells ((x, y) : int * int) : (int * int) list =
  List.filter
    (fun (x, y) -> 0 <= x && x < width && 0 <= y && y < height)
    [ (x + 1, y); (x, y + 1); (x - 1, y); (x, y - 1) ]

(** [get_random_empty_adjacent_cell position] renvoie une case adjacente
    aléatoire vide à côté si elle existe. Sinon elle lève l'exception
    [No_adjacent_space].*)
let get_random_empty_adjacent_cell (position : int * int) : int * int =
  let empty_cells =
    List.filter
      (fun cell -> get_content cell = Empty)
      (get_adjacent_cells position)
  in
  let adjacent_empty_cells = Array.of_list empty_cells in
  let len = Array.length adjacent_empty_cells in
  if len = 0 then raise No_adjacent_space
  else
    let idx = Random.int len in
    adjacent_empty_cells.(idx)

(*renvoie toute les cases ou la creature peut aller ie soit tué ce qui y'était soit il n'y avai rine sur cette case*)
let get_walkable_adjacent_cells (crea : creature) ((x, y) : int * int) :
    (int * int) list =
  List.filter
    (fun (x, y) ->
      let target_crea = get_content (x, y) in
      target_crea = Empty || can_stomp crea target_crea)
    (get_adjacent_cells (x, y))

(** [a_star crea src dest] renvoie une liste de cases successives, si elle
    existe, que [crea] doit parcourir pour atteindre [dest] à partir de [src] en
    utilisant l'algorithme A*, munie de la distance de Manhattan comme
    heuristique. La fonction enclenche [No_path_found] si il n'y a pas de chemin
    de [src] à [dist]. La donnée de [crea] permet de savoir sur quelle case on
    peut marcher. *)
let a_star (crea : creature) (src : int * int) (dest : int * int) :
    (int * int) list =
  assert (crea <> Empty && crea <> Invalid && crea <> Cactus);
  let pq = pqueue_create () in
  let dist_from_src = Hashtbl.create (width * height) in
  let parent = Hashtbl.create (width * height) in
  Hashtbl.add dist_from_src src 0;
  Hashtbl.add parent src src;

  let manhattan_distance ((x1, y1) : int * int) ((x2, y2) : int * int) : int =
    abs (x1 - x2) + abs (y1 - y2)
  in

  let get_dist_from_src ((x, y) : int * int) : int =
    match Hashtbl.find_opt dist_from_src (x, y) with
    | Some d -> d
    | None -> max_int
  in

  let is_explored ((x, y) : int * int) : bool =
    get_dist_from_src (x, y) <> max_int
  in

  let priority ((x, y) : int * int) : int =
    get_dist_from_src (x, y) + manhattan_distance (x, y) dest
  in

  let rec treat_neighbors (curr : int * int) (neighbors : (int * int) list) =
    match neighbors with
    | [] -> ()
    | neighbor :: q ->
        if not (is_explored neighbor) then (
          let d_no_deviation = get_dist_from_src neighbor in
          let d_deviation = 1 + get_dist_from_src curr in
          (* la distance entre curr et neighbor est 1, ils sont côtes à côtes... *)
          if d_deviation < d_no_deviation then begin
            Hashtbl.replace dist_from_src neighbor d_deviation;
            Hashtbl.replace parent neighbor curr
          end;
          pqueue_add pq (priority neighbor) neighbor;
          treat_neighbors curr q)
    (* la priorité est mise à jour si neighbor est déjà dedans *)
  in

  (* [_aux_reconstruct_path cell] donne le chemin de [src] à [cell] *)
  let rec _aux_reconstruct_path cell =
    if cell = src then [ src ]
    else cell :: _aux_reconstruct_path (Hashtbl.find parent cell)
  in
  let reconstruct_path () = List.rev (_aux_reconstruct_path dest) in

  pqueue_add pq (priority src) src;

  let found_dest = ref false in
  while (not (pqueue_is_empty pq)) && not !found_dest do
    let current_cell = pqueue_pop pq in
    if current_cell <> dest then
      let neighbors = get_walkable_adjacent_cells crea current_cell in
      treat_neighbors current_cell neighbors
    else found_dest := true
  done;
  if !found_dest then reconstruct_path () else raise No_path_found

let a_star_get_next_cell crea src dest =
  match a_star crea src dest with
  | [] -> failwith "Empty path"
  | [ _ ] -> failwith "Already on destination"
  | _ :: b :: _ -> b
  | exception No_path_found -> raise No_path_found

(*fonction utilise dans l'affichage du score, vu comme des creatures à droite de la ligne de cactus*)
let chiffre_to_creature (i : int) =
  match i with
  | 0 -> Zero
  | 1 -> Un
  | 2 -> Deux
  | 3 -> Trois
  | 4 -> Quatre
  | 5 -> Cinq
  | 6 -> Six
  | 7 -> Sept
  | 8 -> Huit
  | 9 -> Neuf
  | _ -> failwith "pas un chiffre"

(*fonction d'affichage des variables.*)
let update (i : int) (pos : int) : unit =
  let dizaine = i / 10 in
  let unite = i mod 10 in
  set (width + 2, pos) (chiffre_to_creature dizaine, invalid_id);
  set (width + 3, pos) (chiffre_to_creature unite, invalid_id)

let update_temps (i : int) : unit = update i 0
let update_vague (i : int) : unit = update i 2
let update_power_up (i : int) : unit = update i 4

let update_score (i : int) : unit =
  let centaine = i / 100 in
  let dizaine = i / 10 mod 10 in
  let unite = i mod 10 in
  set (width + 2, 6) (chiffre_to_creature centaine, invalid_id);
  set (width + 3, 6) (chiffre_to_creature dizaine, invalid_id);
  set (width + 4, 6) (chiffre_to_creature unite, invalid_id)

let init_nom_var () : unit =
  set (width + 1, 0) (Temps, invalid_id);
  set (width + 1, 2) (Vague, invalid_id);
  set (width + 1, 4) (Power_up, invalid_id);
  set (width + 1, 6) (Score, invalid_id)
