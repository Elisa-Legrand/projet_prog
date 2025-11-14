(** Type du contenu d'une case du monde. *)
type creature =
  | Empty
  | Cactus
  | Spider
  | Spider_Egg
  | Camel
  | Snake
  | Elephant
  | Invalid
  | Angry_Elephant
  | Stunned_Elephant
  | Robot
  | Boost
  | SuperCamel
  | Zero
  | Un
  | Deux
  | Trois
  | Quatre
  | Cinq
  | Six
  | Sept
  | Huit
  | Neuf
  | Score
  | Temps
  | Vague
  | Power_up

type cell = creature * int

let invalid_id = -1
let width, height = (50, 30)

(* Le système de coordonnées est le suivant :

(0,0)-------------------> + x (width)
  |
  |
  |
  |
  |
  |
  v
 + y (height)

*)

(** Le monde [world] est un tableau mutable. *)
let world : cell array array =
  Array.make_matrix (width + 5) height (Empty, invalid_id)

(** [get (x,y)] renvoie le contenu et l'identifiant de la case en position [x,y]
    du monde. Renvoie [(Invalid, -1)] pour toutes les cases hors du monde.*)
let get ((x, y) : int * int) : cell =
  try world.(x).(y) with Invalid_argument _ -> (Invalid, invalid_id)

(** [set (x,y) v] remplit la case en position [x,y] du monde avec l'entité [v].
    Lève [Exception: Invalid_argument] si la position est hors du monde.*)
let set ((x, y) : int * int) (v : cell) : unit = world.(x).(y) <- v

(** [get_content (x,y)] renvoie le contenu de la case en position [x,y] du
    monde. Renvoie [Invalid] pour toutes les cases hors du monde.*)
let get_content ((x, y) : int * int) : creature =
  let content, _ = get (x, y) in
  content

(** [get_id (x,y)] renvoie l'identifiant de la case en position [x,y] du monde.
    Doit renvoyer [-1] pour les cases vides*)
let get_id ((x, y) : int * int) : int =
  let content, id = get (x, y) in
  if content = Empty && id <> invalid_id then
    failwith "Empty cell with wrong id detected"
  else id
