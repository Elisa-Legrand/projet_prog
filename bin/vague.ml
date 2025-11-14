open World
open Ui
open Utils
open Player
open Engine
open Snake
open Elephant
open Spider
open Robot
open Effect
open Effect.Deep

(** [random_position ()] renvoie une position aléatoire dans le monde*)
let random_position () : int * int = (Random.int width, Random.int height)

(*simplifie les fonctions d'apparition d'entité*)
let spawn_pos (truc:creature) (pos : int*int) :unit=
    match truc with
    |Spider_Egg ->spawn_spider_egg pos
    |Spider -> spawn_spider pos
    |Camel -> spawn_camel pos
    |Elephant -> spawn_elephant pos
    |Snake -> spawn_snake pos
    |Empty|Invalid|Angry_Elephant|Stunned_Elephant->()
    |Robot -> spawn_robot pos
    |chose -> set pos (chose,invalid_id)
    (**)

(*fait apparaitre crea à une position libre aleatoire *)
let spawn (truc:creature):unit =
    let pos = ref (random_position()) in
    while (get_content !pos <> Empty) do 
        pos := random_position()
    done;
    spawn_pos truc !pos

(*variables indicatrice des vagues pour le joueur*)
let nb_vague = ref 0 
let temps_avant_prochaine_vague = ref 0

(*fait apparaitre une vague d'ennemi avec nb_vague elephant et serpent, une araignée et un robot 
et gère l'update des variables liées au système de vague.*)
let rec systeme_vague():unit =
    render();
    if !temps_avant_prochaine_vague =0 then begin
            spawn Boost;
        for _ =0 to (!nb_vague) do
            spawn Elephant;
            spawn Snake;
        done;
        spawn Spider;
        spawn Robot;
        update_vague !nb_vague;
        nb_vague := !nb_vague +1;
    end ;
    temps_avant_prochaine_vague:=(50 + !temps_avant_prochaine_vague -1) mod 50;
    update_temps !temps_avant_prochaine_vague;
    perform(End_of_turn);
    systeme_vague()




