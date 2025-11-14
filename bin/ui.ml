open Notty
open World

(** Affichage du contenu d'une cellule.*)
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
  | Zero,_ -> "\u{0030}\u{FE0F}\u{20E3}" 
  | Un,_ -> "\u{0031}\u{FE0F}\u{20E3}" 
  | Deux,_ -> "\u{0032}\u{FE0F}\u{20E3}" 
  | Trois,_ -> "\u{0033}\u{FE0F}\u{20E3}" 
  | Quatre,_ -> "\u{0034}\u{FE0F}\u{20E3}" 
  | Cinq,_ -> "\u{0035}\u{FE0F}\u{20E3}"  
  | Six,_ -> "\u{0036}\u{FE0F}\u{20E3}" 
  | Sept,_ -> "\u{0037}\u{FE0F}\u{20E3}"  
  | Huit,_ -> "\u{0038}\u{FE0F}\u{20E3}"  
  | Neuf,_ -> "\u{0039}\u{FE0F}\u{20E3}"  

(* Codes des emojis pour les animaux pertinents
   serpent : "\u{1F40D}"
   éléphant : "\u{1F418}"
   araignée : "\u{1F577}"
   oeuf : "\u{1F95A}"
   Des sites comme l'emojipedia peuvent vous donner plus de codes.
*)

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
