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

let spawn_pos (truc:creature) (pos : int*int) :unit=
    match truc with
    |Spider_Egg ->spawn_spider_egg pos
    |Spider -> spawn_spider pos
    |Camel -> spawn_camel pos
    |Elephant -> spawn_elephant pos
    |Snake -> spawn_snake pos
    |Empty|Invalid|Angry_Elephant|Stunned_Elephant->()
    |Robot -> ()
    |chose -> set pos (chose,invalid_id)
    (*spawn_robot pos*)

let spawn (truc:creature):unit =
    let pos = ref (random_position()) in
    while (get_content !pos <> Empty) do 
        pos := random_position()
    done;
    spawn_pos truc !pos


let nb_vague = ref 0 
let temps_avant_prochaine_vague = ref 0

let rec systeme_vague():unit =
    render();
    if !temps_avant_prochaine_vague =0 then begin
        for _ =0 to (!nb_vague) do
            spawn Spider;
            spawn Elephant;
            spawn Snake;
        done;
        update_vague !nb_vague;
        nb_vague := !nb_vague +1;
    end ;
    temps_avant_prochaine_vague:=(50 + !temps_avant_prochaine_vague -1) mod 50;
    update_temps !temps_avant_prochaine_vague;
    perform(End_of_turn);
    systeme_vague()




