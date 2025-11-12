open Ui
open Effect
open Effect.Deep

module IntSet = Set.Make(Int)

(** L'effet [End_of_turn] indique qu'une entité est prête à passer la main car elle a terminé
    son tour.*)
type _ Effect.t += End_of_turn: unit t

(** File de threads
   [queue] contient toutes les entités en attente de leur prochain tour, 
   sous forme de fonctions [ia]. Pour chaque entité, [ia ()] va jouer le code de l'entité
   correspondant à son prochain tour. *)
let queue : (unit -> unit) Queue.t = Queue.create ()

let camel_pos = ref (-1, -1)

(** Set des identifiants des créatures mortes. *)
let dead_ids = ref IntSet.empty

let is_dead (id : int) : bool =
  IntSet.mem id !dead_ids

(** Joue l'effet [End_of_turn], et vérifie juste avant de reprendre le tour si la créature est morte.
    Renvoie [true] si la créature est encore en vie à la reprise de son tour. *)
let safe_perform (id : int) : bool =
  perform End_of_turn;
  let dead = is_dead id in
  if dead then dead_ids := IntSet.remove id !dead_ids;
  not (dead)

(** [player ia] est appelé pour jouer le tour caractérisé par la fonction [ia].
    L'exécution de la fonction est arrêtée si l'effet [End_of_turn] est perform
    et la continuation est enfilée dans [queue].*)
let player (character : unit -> unit): unit =
  try character ()
  with|effect End_of_turn  , k ->
    Queue.add (fun () -> continue k ()) queue

(** [run_queue ()] exécute les fonctions correspondant aux tours successifs des entités du jeu.
    Si la file devient vide lors de la partie, la fonction lève l'exception [Queue.Empty].
    (le type de retour 'a s'explique par le fait que la fonction n'est pas censée terminer)*)
let run_queue () : 'a =
  while true do
    render () ;
    let suspended_character = Queue.pop queue in
    suspended_character () ;
  done
  


(**perform prenant en compte la mort: les creatures appelent
safe perform pour que la premiere étape de leur continuation 
soit de tester si ils sont morts*)
let safe_perform (id : int):bool=
    perform(End_of_turn);
    is_dead id