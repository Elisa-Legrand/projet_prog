open Notty
open World

(** Affichage du contenu d'une cellule. les cas jusqu'à zero sont les entité
    utilisées par le jeu, le reste est simplement à but d'affichage des
    variables.*)
let string_of_cell : cell -> string = function
  | Empty, _ -> "  "
  | Cactus, _ -> "\u{1F335}"
  | Spider, _ -> "\u{1F577}"
  | Spider_Egg, _ -> "\u{1F95A}"
  | Camel, _ -> "\u{1F42A}"
  | Snake, _ -> "\u{1F40D}"
  | Elephant, _ -> "\u{1F418}"
  | Invalid, _ -> failwith "Invalid cell detected"
  | Angry_Elephant, _ -> "\u{1F621}"
  | Stunned_Elephant, _ -> "\u{1F635}"
  | Robot, _ -> "\u{1F916}"
  | Boost, _ -> "\u{1F9EA}"
  | SuperCamel, _ -> "\u{1F9B8}"
  | Zero, _ -> "0"
  | Un, _ -> "1"
  | Deux, _ -> "2"
  | Trois, _ -> "3"
  | Quatre, _ -> "4"
  | Cinq, _ -> "5"
  | Six, _ -> "6"
  | Sept, _ -> "7"
  | Huit, _ -> "8"
  | Neuf, _ -> "9"
  | Score, _ -> "Score:"
  | Temps, _ -> "Temps avant prochaine vague:"
  | Vague, _ -> "Vague:"
  | Power_up, _ -> "Temps restant de power_up:"

(** Fonctions de création de l'image correspondant à l'état actuel du monde.*)
let draw_cell (c : cell) : image = I.string A.empty (string_of_cell c)

let draw_world () : image =
  I.hcat @@ Array.to_list
  @@ Array.map
       (fun column -> I.vcat @@ Array.to_list @@ Array.map draw_cell column)
       world

open Notty_unix

(** [terminal] est une constante qui correspond au terminal où le jeu est joué*)
let terminal : Term.t = Term.create ()

(** [render ()] met à jour l'affichage courant dans le terminal*)
let render () : unit = Term.image terminal (draw_world ())

let () = render ()
