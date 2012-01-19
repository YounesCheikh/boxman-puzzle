(*
BoxMan Game
by Younes CHEIKH
Enseignant: Thierry montaut

File downloaded from ww.cheikh.me
contact me : younes.cheikh@gmail.com
*)


(* déclaration des variables *)

(* longueur de fenetre de jeu *)
let window_high = 550;;

(*largueur de fenetre de jeu*)
let window_large = 400;;

(*points x & y de depart par default *)
let default_point_x = 50;;
let default_point_y = 50;;

(*largueur de bodure*)
let bordure_taille = 3;;

(*couleur de bodure*)   
let bordure_col = blue;;

(*couleur de l'arriere plan*)
let default_col = (rgb 160 160 160);;

(*couleur de box*)
let box_col = (rgb 0 230 0);;	

(*couleur de box*)
let box_col2 = (rgb 0 50 0);;

(*couleur de box*)
let box_col3 = (rgb 0 128 0);;

(*couleur de box*)
let box_col4 = (rgb 187 238 240);;	

(*couleur de box*)
let box_col5 = (rgb 148 228 55);;

(*couleur de box*)
let box_col6 = (rgb 105 209 128);;

(* la taille de box *)
let box_taille = 50;;

(* couleurs de mur *)
let mur_col1 = (rgb 0 0 0);;
let mur_col2 = (rgb 162 56 10);;

(* la taille de mur *)
let mur_taille = 50;;

(* les couleurs de cercle *)
(* on utilise plusieur couleur pour 3D *)
let cercle_col1 =(rgb 148 228 238);;	
let cercle_col2 =(rgb 187 238 240);;

(* la taille de cercle*)
let cercle_taille = 20;;

(*couleur de joueur*)
let man_col = (rgb 23 23 0);;

(* la taille de joueur *)
let man_taille = 50;;

(*longueur de fenetre de jeu*)
let menu_high = 550;;

(*largueur de fenetre de jeu*)
let menu_large = 200;;

(*arriere plan de menu*)
let bg_menu = white;;

(* niveau finale *)
let niveau_max=10;;