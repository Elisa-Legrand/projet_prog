(** Type du contenu d'une case du monde. *)
type creature = Empty | Cactus | Spider | Spider_Egg | Camel | Snake | Elephant | Invalid
type cell = creature * int

let width, height = 50, 30

(** Le monde [world] est un tableau mutable. *)
let world : cell array array = Array.make_matrix width height (Empty,0)

(** [get (x,y)] renvoie le contenu et l'identifiant de la case en position [x,y] du monde. 
    Renvoie [(Invalid, -1)] pour toutes les cases hors du monde.*)
let get (x, y : int * int) : cell = try world.(x).(y) with Invalid_argument _ -> (Invalid,-1)

(** [set (x,y) v] remplit la case en position [x,y] du monde avec l'entité [v].
    Lève [Exception: Invalid_argument] si la position est hors du monde.*)
let set (x, y : int * int) (v : cell) : unit = world.(x).(y) <- v

(** [get_content (x,y)] renvoie le contenu de la case en position [x,y] du monde. 
    Renvoie [Invalid] pour toutes les cases hors du monde.*)
let get_content (x, y : int * int) : creature =
    let (content, _) = get (x, y) in content

