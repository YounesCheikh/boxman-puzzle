(*
BoxMan Game
by Younes CHEIKH
Enseignant: Thierry montaut

File downloaded from ww.cheikh.me
contact me : younes.cheikh@gmail.com
*)

(*
Controlez BOXMAN avec les touches de navigation. La mission consiste à pousser toutes les boîtes vers la zone de destination.
Vous engagez la mission suivante une fois la mission en cours terminée.
Appuyer sur la touche 'q' ou sur '4' pour aller vers la gauche.
Appuyer sur la touche 'd' ou sur '6' pour aller vers la droite.
Appuyer sur la touche 'z' ou sur '8' pour aller vers le haut.
Appuyer sur la touche 's' ou sur '2' pour aller vers le bas.
'u' ou '5' pour annuler.
'R' pour recommencer. 
*)

(* la bibliotheque graphique*)
#open "graphics";; 
(* On utilise la bibliotheque sys pour calculer le temp*)
#open "sys";;

(* *********************************** *)

(* Exceptions *)

(* exception ListeCourte pour eviter les erreur de fichier score *)
exception ListeCourte;;

exception QuitJeu;;

(* *********************************** *)
(* le fichier qui contient la fonction qui dessine les lettres en degitale*)
include "fill_string.ml";;

(* le fichier des variables *)
include "variables.ml";;

(* le fichier des fonctions *)
include "fonctions.ml";;

(* le fichier des niveaux *)
include "niveaux.ml";;


(* ******************************************** *)
(* La fonction globale *)
			
let rec boxman = 
fun (n, (* Nombre de niveau *)
	m, (* Nombre des mouvements *)
	score_vect, (* le vecteur des scores *)
	temp_actuel, (* le temp de debut *)
	minutes, (* le nombre des minutes *)
	(x,y), (* les cordonnes de bonhomme *)
	mur_l, (* liste des points pour le mur *)
	cercles_l, (* liste des points des cercles *)
	box_l, (* liste des points des boites (boxs)*)
	(x_u,y_u), (* dernieres cordonnees de bonhomme *)
	box_l_u (* derniere boxliste (on la sauvgarde chaque mouvements pour permet le jouer d'annuler le dernier mouvement)*)
	)->
	if n>niveau_max then () (* cette condition pour eviter les bugs ou cas ou n depasse le nombre des niveaux *)
	else 
(*
play_niveau c'est la fonction qui permet de passer d'un niveau à l'autre
*)
	let play_niveau = fun 
	(1,score_v) -> begin
					bordure_fenetre(); (* pour chaque niveau on dessine la fenetre gauche *)
					placer_mur n1.mur; (* et on place le nouveau mur *)
					placer_cercles(n1.cercle,n1.box); (* on place les cerlces *)
					dessine_man n1.man; (* on dessine le bonhomme dans son position pardefault *)
					placer_box(n1.box,n1.cercle); (* on place les boites *)
					affiche_niv_mov(1,0,score_v.(0)); (* on affiche le niveau et le nombre de deplacements *)
					boxman(1,0,score_v,int_of_float(time()),0,n1.man,n1.mur,n1.cercle,n1.box,n1.man,n1.box) (* commence une nouvelle partie*)
					end
	|(2,score_v) -> begin
					bordure_fenetre();
					placer_mur n2.mur;
					placer_cercles(n2.cercle,n2.box);
					dessine_man n2.man;
					placer_box(n2.box,n2.cercle);
					affiche_niv_mov(2,0,score_v.(1));
					boxman(2,0,score_v,int_of_float(time()),0,n2.man,n2.mur,n2.cercle,n2.box,n2.man,n2.box)
					end
	|(3,score_v) -> begin
					bordure_fenetre();
					placer_mur n3.mur;
					placer_cercles(n3.cercle,n3.box);
					dessine_man n3.man;
					placer_box(n3.box,n3.cercle);
					affiche_niv_mov(3,0,score_v.(2));
					boxman(3,0,score_v,int_of_float(time()),0,n3.man,n3.mur,n3.cercle,n3.box,n3.man,n3.box)
					end
	|(4,score_v) -> begin
					bordure_fenetre();
					placer_mur n4.mur;
					placer_cercles(n4.cercle,n4.box);
					dessine_man n4.man;
					placer_box(n4.box,n4.cercle);
					affiche_niv_mov(4,0,score_v.(3));
					boxman(4,0,score_v,int_of_float(time()),0,n4.man,n4.mur,n4.cercle,n4.box,n4.man,n4.box)
					end
	|(5,score_v) -> begin
					bordure_fenetre();
					placer_mur n5.mur;
					placer_cercles(n5.cercle,n5.box);
					dessine_man n5.man;
					placer_box(n5.box,n5.cercle);
					affiche_niv_mov(5,0,score_v.(4));
					boxman(5,0,score_v,int_of_float(time()),0,n5.man,n5.mur,n5.cercle,n5.box,n5.man,n5.box)
					end
	|(6,score_v) -> begin
					bordure_fenetre();
					placer_mur n6.mur;
					placer_cercles(n6.cercle,n6.box);
					dessine_man n6.man;
					placer_box(n6.box,n6.cercle);
					affiche_niv_mov(6,0,score_v.(5));
					boxman(6,0,score_v,int_of_float(time()),0,n6.man,n6.mur,n6.cercle,n6.box,n6.man,n6.box)
					end
	|(7,score_v) -> begin
					bordure_fenetre();
					placer_mur n7.mur;
					placer_cercles(n7.cercle,n7.box);
					dessine_man n7.man;
					placer_box(n7.box,n7.cercle);
					affiche_niv_mov(7,0,score_v.(6));
					boxman(7,0,score_v,int_of_float(time()),0,n7.man,n7.mur,n7.cercle,n7.box,n7.man,n7.box)
					end
 	|(8,score_v) -> begin
					bordure_fenetre();
					placer_mur n8.mur;
					placer_cercles(n8.cercle,n8.box);
					dessine_man n8.man;
					placer_box(n8.box,n8.cercle);
					affiche_niv_mov(8,0,score_v.(7));
					boxman(8,0,score_v,int_of_float(time()),0,n8.man,n8.mur,n8.cercle,n8.box,n8.man,n8.box)
					end 
	|(9,score_v) -> begin
					bordure_fenetre();
					placer_mur n9.mur;
					placer_cercles(n9.cercle,n9.box);
					dessine_man n9.man;
					placer_box(n9.box,n9.cercle);
					affiche_niv_mov(9,0,score_v.(8));
					boxman(9,0,score_v,int_of_float(time()),0,n9.man,n9.mur,n9.cercle,n9.box,n9.man,n9.box)
					end 
 	|(10,score_v) -> begin
					bordure_fenetre();
					placer_mur n10.mur;
					placer_cercles(n10.cercle,n10.box);
					dessine_man n10.man;
					placer_box(n10.box,n10.cercle);
					affiche_niv_mov(10,0,score_v.(9));
					boxman(10,0,score_v,int_of_float(time()),0,n10.man,n10.mur,n10.cercle,n10.box,n10.man,n10.box)
					end
	|_ -> begin
		bordure_fenetre(); 
		fin_niveaux(); (* si l'utilisateur fini tous les niveaux on affiche une felicitation :) *)
		sleep(5000);
		bordure_fenetre();
		placer_mur n1.mur;
		placer_cercles(n1.cercle,n1.box);
		dessine_man n1.man;
		placer_box(n1.box,n1.cercle);
		affiche_niv_mov(1,0,score_vect.(0));
		boxman(1,0,score_vect,int_of_float(time()),0,n1.man,n1.mur,n1.cercle,n1.box,n1.man,n1.box) (* on recommence le niveau 1 *)
		(* raise Quitjeu *)
		end 
in 

(* Gestion de la souris *)
let e2=wait_next_event[Button_down;Poll]
and (xm,ym)=mouse_pos() in 
if e2.button then 
	(* Gestion des clicks sur le menu *)
	begin
		if 	xm > (default_point_x+window_large+bordure_taille+20) 
			& xm< (default_point_x+window_large+bordure_taille+menu_large-20) 
			& ym> (default_point_y+10)
			& ym< (default_point_y+30) then raise QuitJeu
									(* goodbye() *) (* on quit le jeu *)
		else if 
			xm > (default_point_x+window_large+bordure_taille+20) 
			& xm< (default_point_x+window_large+bordure_taille+menu_large-20) 
			& ym> (default_point_y+30)
			& ym< (default_point_y+50) then 
									(* on recommence la partie en cours *)
									(sleep(800);play_niveau(n,score_vect))
		else if 
			xm > (default_point_x+window_large+bordure_taille+20) 
			& xm< (default_point_x+window_large+bordure_taille+menu_large-20) 
			& ym> (default_point_y+50)
			& ym< (default_point_y+70) 
			& n<> 1 then 
					(* on passe au niveau precedent *)
					(sleep(800);play_niveau(n-1,score_vect))
		else if 
			xm > (default_point_x+window_large+bordure_taille+20) 
			& xm< (default_point_x+window_large+bordure_taille+menu_large-20) 
			& ym> (default_point_y+70)
			& ym< (default_point_y+90) 
			& n<> niveau_max then
							(* on passe au niveau suivant *)
							(sleep(800);play_niveau(n+1,score_vect))
		else if 
			xm > (default_point_x+window_large+bordure_taille+20) 
			& xm< (default_point_x+window_large+bordure_taille+menu_large-20) 
			& ym> (default_point_y+90)
			& ym< (default_point_y+110) 
			& n<> niveau_max then 
								begin
									(* on affiche l'aide *)
									aide(true);
									(* on efface l'aide avec les fonctions suivantes *)
									bordure_fenetre();
									placer_mur mur_l;
									placer_cercles(cercles_l,box_l);
									dessine_man (x,y);
									placer_box(box_l,cercles_l);
									affiche_niv_mov(n,m,score_vect.(n-1));
									boxman(n,m,score_vect,temp_actuel,minutes,(x,y),mur_l,cercles_l,box_l,(x_u,y_u),box_l_u)
									
								end
		else boxman(n,m,score_vect,temp_actuel,minutes,(x,y),mur_l,cercles_l,box_l,(x_u,y_u),box_l_u)
	end
else
begin
(*Gestion du temps*)
	let dif_temps=abs((int_of_float(time()))- temp_actuel) in (* on utilise abs pour eviter un temp negatif *)
		if dif_temps mod 100<60 then (* la fonction time mod 100 ça donne des valeur entre 0 et 99 *)
			let secondes=dif_temps mod 100 in
			afficher_time(minutes,secondes) (* la fonction affiche le temp *)
		(* si non on acctualise le temp *)	
		else boxman(n,m,score_vect,int_of_float(time()),minutes+1,(x,y),mur_l,cercles_l,box_l,(x_u,y_u),box_l_u);
(* si tout les boxs cachent toutes les cercles on passe au niveau suivants *)
if compare_2_listes(cercles_l,box_l) then 
	begin 
		(* dans ce cas la partie en cours termine *)
		efface_man(x,y);
		dessine_man(x,y);
		(* on verif si le joueur a le meilleur score ou pas *)
		if compare_score(n-1,m,score_vect) then
			begin
			(* savgarde le meilleur score *)
			let f_score=open_out "score.txt" in
			sauvgarder_nouveau_score(update_score(n-1,m,score_vect),f_score);
			close_out f_score;
			end
		else ();
		sleep(1000);
		if (m<=score_vect.(n-1)) or (score_vect.(n-1)=0) then
		(* si l'utilisateur a un meilleur score -> true *)
		nouveau_niveau(n+1,true)
		else nouveau_niveau(n+1,false);
		if n<>niveau_max then sleep(2000);
		let score_vect=update_score(n-1,m,score_vect) in play_niveau(n+1,score_vect)
	end
else
begin
	(* ******************************************** *)
	(* avec le passage de la souris on colore le menu avec la couleur rouge *)
	(* Debut *)
	begin
		if 	xm > (default_point_x+window_large+bordure_taille+20) 
			& xm< (default_point_x+window_large+bordure_taille+menu_large-20) 
			& ym> (default_point_y+10)
			& ym< (default_point_y+30) then 
									begin
										set_color red;
										moveto (default_point_x+window_large+bordure_taille+20) (default_point_y+10);
										draw_string "Quit Jeu";
					
									end
									(* goodbye() (* on quit le jeu *) *)
		else if 
			xm > (default_point_x+window_large+bordure_taille+20) 
			& xm< (default_point_x+window_large+bordure_taille+menu_large-20) 
			& ym> (default_point_y+30)
			& ym< (default_point_y+50) then 
										(set_color red;
										moveto (default_point_x+window_large+bordure_taille+20) (default_point_y+30);
										draw_string "Recommencer";)
		else if 
			xm > (default_point_x+window_large+bordure_taille+20) 
			& xm< (default_point_x+window_large+bordure_taille+menu_large-20) 
			& ym> (default_point_y+50)
			& ym< (default_point_y+70)  then 
						(set_color red;
						moveto (default_point_x+window_large+bordure_taille+20) (default_point_y+50);
						draw_string "Niveau Precedent";)
		else if 
			xm > (default_point_x+window_large+bordure_taille+20) 
			& xm< (default_point_x+window_large+bordure_taille+menu_large-20) 
			& ym> (default_point_y+70)
			& ym< (default_point_y+90) then
								(set_color red;
								moveto (default_point_x+window_large+bordure_taille+20) (default_point_y+70);
								draw_string "Niveau Suivant")
		else if 
			xm > (default_point_x+window_large+bordure_taille+20) 
			& xm< (default_point_x+window_large+bordure_taille+menu_large-20) 
			& ym> (default_point_y+90)
			& ym< (default_point_y+110) 
			& n<> niveau_max then 
								begin
									set_color red;
									moveto (default_point_x+window_large+bordure_taille+20) (default_point_y+90);
									draw_string "Aide!";
								end
		else begin
				set_color blue;
				moveto (default_point_x+window_large+bordure_taille+20) (default_point_y+10);
				draw_string "Quit Jeu";
				moveto (default_point_x+window_large+bordure_taille+20) (default_point_y+30);
				draw_string "Recommencer";
				moveto (default_point_x+window_large+bordure_taille+20) (default_point_y+50);
				draw_string "Niveau Precedent";
				moveto (default_point_x+window_large+bordure_taille+20) (default_point_y+70);
				draw_string "Niveau Suivant";
				moveto (default_point_x+window_large+bordure_taille+20) (default_point_y+90);
				draw_string "Aide!";
			end(* boxman(n,m,score_vect,temp_actuel,minutes,(x,y),mur_l,cercles_l,box_l,(x_u,y_u),box_l_u) *)
	end;
	(* Fin *)

	(************************************)
	(* (*Gestion du temps*)
	if abs((int_of_float(time()))- temp_actuel) mod 100<60 then afficher_time(minutes,dif_temps mod 100)
	else boxman(n,m,score_vect,int_of_float(time()),minutes+1,(x,y),mur_l,cercles_l,box_l,(x_u,y_u),box_l_u); *)
	(*   *)
	let e1 = wait_next_event[Key_pressed;Poll] in
		begin
			if e1.keypressed then 
				let c = read_key() in
					let direction = (match c with
						`8` -> if ((y < (window_high-man_taille)) & verif_point_y_positif(x,y,mur_l)) then
								begin
									efface_man(x,y);
									placer_cercles(cercles_l,box_l);
									dessine_man_haut(x,y);
									if verif_point_y_positif(x,y,box_l) then 
										(* 1 er cas on pousse pas une boite *)
										begin
											efface_man(x,y);
											placer_cercles(cercles_l,box_l);
											dessine_man_haut(x,y+man_taille);
											boxman(n,m,score_vect,temp_actuel,minutes,(x,y+man_taille),mur_l,cercles_l,box_l,(x_u,y_u),box_l_u)
										end
									else
										begin
											(* on pouse la boite *)
											if (verif_point_y_positif(x,y+man_taille,mur_l)) & (verif_point_y_positif(x,y+man_taille,box_l)) then
												begin
												sleep(100);
												efface_man(x,y);
												efface_man(x,y+man_taille);
												placer_cercles(cercles_l,update_liste(box_l,(x,y+man_taille),(x,y+2*man_taille)));
												dessine_man_haut(x,y+man_taille);
												placer_box (update_liste(box_l,(x,y+man_taille),(x,y+2*man_taille)),cercles_l);
												affiche_niv_mov(n,m+1,score_vect.(n-1));
												let ((x_u,y_u),box_l_u)= ((x,y),box_l) 
												in boxman(n,m+1,score_vect,temp_actuel,minutes,(x,y+man_taille),mur_l,cercles_l,update_liste(box_l,(x,y+man_taille),(x,y+2*man_taille)),(x_u,y_u),box_l_u)
												end
											(* else boxman(n,m,score_vect,temp_actuel,minutes,(x,y),mur_l,cercles_l,box_l,(x_u,y_u),box_l_u) *)
										end
								end;
						|`z` -> if ((y < (window_high-man_taille)) & verif_point_y_positif(x,y,mur_l)) then
								begin
									efface_man(x,y);
									placer_cercles(cercles_l,box_l);
									dessine_man_haut(x,y);
									if verif_point_y_positif(x,y,box_l) then 
										begin
											efface_man(x,y);
											placer_cercles(cercles_l,box_l);
											dessine_man_haut(x,y+man_taille);
											boxman(n,m,score_vect,temp_actuel,minutes,(x,y+man_taille),mur_l,cercles_l,box_l,(x_u,y_u),box_l_u)
										end
									else
										begin
											if (verif_point_y_positif(x,y+man_taille,mur_l)) & (verif_point_y_positif(x,y+man_taille,box_l)) then
												begin
												sleep(100);
												efface_man(x,y);
												efface_man(x,y+man_taille);
												placer_cercles(cercles_l,update_liste(box_l,(x,y+man_taille),(x,y+2*man_taille)));
												dessine_man_haut(x,y+man_taille);
												placer_box (update_liste(box_l,(x,y+man_taille),(x,y+2*man_taille)),cercles_l);
												affiche_niv_mov(n,m+1,score_vect.(n-1));
												let ((x_u,y_u),box_l_u)= ((x,y),box_l) 
												in boxman(n,m+1,score_vect,temp_actuel,minutes,(x,y+man_taille),mur_l,cercles_l,update_liste(box_l,(x,y+man_taille),(x,y+2*man_taille)),(x_u,y_u),box_l_u)
												end
											(* else boxman(n,m,score_vect,temp_actuel,minutes,(x,y),mur_l,cercles_l,box_l,(x_u,y_u),box_l_u) *)
										end
									end;		
						|`2` -> if (y+default_point_y > (default_point_y)) & (verif_point_y_negatif(x,y,mur_l)) then
								begin
									efface_man(x,y);
									placer_cercles(cercles_l,box_l);
									dessine_man_bas(x,y);
									if verif_point_y_negatif(x,y,box_l) then 
										begin
											efface_man(x,y);
											placer_cercles(cercles_l,box_l);
											(* efface_man(x,y-man_taille); *)
											dessine_man_bas(x,y-man_taille);
											boxman(n,m,score_vect,temp_actuel,minutes,(x,y-man_taille),mur_l,cercles_l,box_l,(x_u,y_u),box_l_u)
										end
									else
										begin
											if (verif_point_y_negatif(x,y-man_taille,mur_l)) & (verif_point_y_negatif(x,y-man_taille,box_l)) then
												begin
												sleep(100);
												efface_man(x,y);
												efface_man(x,y-man_taille);
												placer_cercles(cercles_l,update_liste(box_l,(x,y-man_taille),(x,y-2*man_taille)));
												dessine_man_bas(x,y-man_taille);
												placer_box (update_liste(box_l,(x,y-man_taille),(x,y-2*man_taille)),cercles_l);
												affiche_niv_mov(n,m+1,score_vect.(n-1));
												let ((x_u,y_u),box_l_u)= ((x,y),box_l) 
												in boxman(n,m+1,score_vect,temp_actuel,minutes,(x,y-man_taille),mur_l,cercles_l,update_liste(box_l,(x,y-man_taille),(x,y-2*man_taille)),(x_u,y_u),box_l_u)
												end
											(* else boxman(n,m,score_vect,temp_actuel,minutes,(x,y),mur_l,cercles_l,box_l,(x_u,y_u),box_l_u) *)
										end
									end;	
						|`s` -> if (y+default_point_y > (default_point_y)) & (verif_point_y_negatif(x,y,mur_l)) then
								begin
									efface_man(x,y);
									placer_cercles(cercles_l,box_l);
									dessine_man_bas(x,y);
									if verif_point_y_negatif(x,y,box_l) then 
										begin
											efface_man(x,y);
											placer_cercles(cercles_l,box_l);
											(* efface_man(x,y-man_taille); *)
											dessine_man_bas(x,y-man_taille);
											boxman(n,m,score_vect,temp_actuel,minutes,(x,y-man_taille),mur_l,cercles_l,box_l,(x_u,y_u),box_l_u)
										end
									else
										begin
											if (verif_point_y_negatif(x,y-man_taille,mur_l)) & (verif_point_y_negatif(x,y-man_taille,box_l)) then
												begin
												sleep(100);
												efface_man(x,y);
												efface_man(x,y-man_taille);
												placer_cercles(cercles_l,update_liste(box_l,(x,y-man_taille),(x,y-2*man_taille)));
												dessine_man_bas(x,y-man_taille);
												placer_box (update_liste(box_l,(x,y-man_taille),(x,y-2*man_taille)),cercles_l);
												affiche_niv_mov(n,m+1,score_vect.(n-1));
												let ((x_u,y_u),box_l_u)= ((x,y),box_l) 
												in boxman(n,m+1,score_vect,temp_actuel,minutes,(x,y-man_taille),mur_l,cercles_l,update_liste(box_l,(x,y-man_taille),(x,y-2*man_taille)),(x_u,y_u),box_l_u)
												end
											(* else boxman(n,m,score_vect,temp_actuel,minutes,(x,y),mur_l,cercles_l,box_l,(x_u,y_u),box_l_u) *)
										end
									end;		
						|`6` -> if (x < (window_large-man_taille)) & (verif_point_x_positif(x,y,mur_l)) then
									begin
										efface_man(x,y);
										placer_cercles(cercles_l,box_l);
										dessine_man_droite(x,y);
										if verif_point_x_positif(x,y,box_l) then
											begin
												efface_man(x,y);
												placer_cercles(cercles_l,box_l);
												dessine_man_droite(x+man_taille,y);
												boxman(n,m,score_vect,temp_actuel,minutes,(x+man_taille,y),mur_l,cercles_l,box_l,(x_u,y_u),box_l_u)
											end
										else
											begin
												if (verif_point_x_positif(x+man_taille,y,mur_l)) & (verif_point_x_positif(x+man_taille,y,box_l)) then
													begin
														sleep(100);
														efface_man(x,y);
														efface_man(x+man_taille,y);
														placer_cercles(cercles_l,update_liste(box_l,(x+man_taille,y),(x+2*man_taille,y)));
														dessine_man_droite(x+man_taille,y);
														placer_box (update_liste(box_l,(x+man_taille,y),(x+2*man_taille,y)),cercles_l);
														affiche_niv_mov(n,m+1,score_vect.(n-1));
														let ((x_u,y_u),box_l_u)= ((x,y),box_l) 
														in boxman(n,m+1,score_vect,temp_actuel,minutes,(x+man_taille,y),mur_l,cercles_l,update_liste(box_l,(x+man_taille,y),(x+2*man_taille,y)),(x_u,y_u),box_l_u)
													end
												(* else 
													boxman(n,m,score_vect,temp_actuel,minutes,(x,y),mur_l,cercles_l,box_l,(x_u,y_u),box_l_u) *)
											end
									end;
						|`d` -> if (x < (window_large-man_taille)) & (verif_point_x_positif(x,y,mur_l)) then
									begin
										efface_man(x,y);
										placer_cercles(cercles_l,box_l);
										dessine_man_droite(x,y);
										if verif_point_x_positif(x,y,box_l) then
											begin
												efface_man(x,y);
												placer_cercles(cercles_l,box_l);
												dessine_man_droite(x+man_taille,y);
												boxman(n,m,score_vect,temp_actuel,minutes,(x+man_taille,y),mur_l,cercles_l,box_l,(x_u,y_u),box_l_u)
											end
										else
											begin
												if (verif_point_x_positif(x+man_taille,y,mur_l)) & (verif_point_x_positif(x+man_taille,y,box_l)) then
													begin
														sleep(100);
														efface_man(x,y);
														efface_man(x+man_taille,y);
														placer_cercles(cercles_l,update_liste(box_l,(x+man_taille,y),(x+2*man_taille,y)));
														dessine_man_droite(x+man_taille,y);
														placer_box (update_liste(box_l,(x+man_taille,y),(x+2*man_taille,y)),cercles_l);
														affiche_niv_mov(n,m+1,score_vect.(n-1));
														let ((x_u,y_u),box_l_u)= ((x,y),box_l) 
														in boxman(n,m+1,score_vect,temp_actuel,minutes,(x+man_taille,y),mur_l,cercles_l,update_liste(box_l,(x+man_taille,y),(x+2*man_taille,y)),(x_u,y_u),box_l_u)
													end
												(* else 
													boxman(n,m,score_vect,temp_actuel,minutes,(x,y),mur_l,cercles_l,box_l,(x_u,y_u),box_l_u) *)
											end
									end;
						|`4` -> if (x+default_point_x > (default_point_x)) & (verif_point_x_negatif(x,y,mur_l))  then
									begin
										efface_man(x,y);
										placer_cercles(cercles_l,box_l);
										dessine_man_gauche(x,y);
										if verif_point_x_negatif(x,y,box_l) then
											begin
												efface_man(x,y);
												placer_cercles(cercles_l,box_l);
												dessine_man_gauche(x-man_taille,y);
												boxman(n,m,score_vect,temp_actuel,minutes,(x-man_taille,y),mur_l,cercles_l,box_l,(x_u,y_u),box_l_u)
											end
										else
											begin
												if (verif_point_x_negatif(x-man_taille,y,mur_l)) & (verif_point_x_negatif(x-man_taille,y,box_l)) then
													begin
														sleep(100);
														efface_man(x,y);
														efface_man(x-man_taille,y);
														placer_cercles(cercles_l,update_liste(box_l,(x-man_taille,y),(x-2*man_taille,y)));
														dessine_man_gauche(x-man_taille,y);
														placer_box (update_liste(box_l,(x-man_taille,y),(x-2*man_taille,y)),cercles_l);
														affiche_niv_mov(n,m+1,score_vect.(n-1));
														let ((x_u,y_u),box_l_u)= ((x,y),box_l) 
														in boxman(n,m+1,score_vect,temp_actuel,minutes,(x-man_taille,y),mur_l,cercles_l,update_liste(box_l,(x-man_taille,y),(x-2*man_taille,y)),(x_u,y_u),box_l_u)
													end
												(* else 
													boxman(n,m,score_vect,temp_actuel,minutes,(x,y),mur_l,cercles_l,box_l,(x_u,y_u),box_l_u) *)
											end										
									end;			
						|`q` -> if (x+default_point_x > (default_point_x)) & (verif_point_x_negatif(x,y,mur_l))  then
									begin
										efface_man(x,y);
										placer_cercles(cercles_l,box_l);
										dessine_man_gauche(x,y);
										if verif_point_x_negatif(x,y,box_l) then
											begin
												efface_man(x,y);
												placer_cercles(cercles_l,box_l);
												dessine_man_gauche(x-man_taille,y);
												boxman(n,m,score_vect,temp_actuel,minutes,(x-man_taille,y),mur_l,cercles_l,box_l,(x_u,y_u),box_l_u)
											end
										else
											begin
												if (verif_point_x_negatif(x-man_taille,y,mur_l)) & (verif_point_x_negatif(x-man_taille,y,box_l)) then
													begin
														sleep(100);
														efface_man(x,y);
														efface_man(x-man_taille,y);
														placer_cercles(cercles_l,update_liste(box_l,(x-man_taille,y),(x-2*man_taille,y)));
														dessine_man_gauche(x-man_taille,y);
														placer_box (update_liste(box_l,(x-man_taille,y),(x-2*man_taille,y)),cercles_l);
														affiche_niv_mov(n,m+1,score_vect.(n-1));
														let ((x_u,y_u),box_l_u)= ((x,y),box_l) 
														in boxman(n,m+1,score_vect,temp_actuel,minutes,(x-man_taille,y),mur_l,cercles_l,update_liste(box_l,(x-man_taille,y),(x-2*man_taille,y)),(x_u,y_u),box_l_u)
													end
												(* else 
													boxman(n,m,score_vect,temp_actuel,minutes,(x,y),mur_l,cercles_l,box_l,(x_u,y_u),box_l_u) *)
											end										
									end;	
						|`u` -> begin
									bordure_fenetre();
									placer_mur mur_l;
									placer_cercles(cercles_l,box_l_u);
									dessine_man(x_u,y_u);
									placer_box(box_l_u,cercles_l);
									if compare_2_listes(box_l_u,box_l) then 
									begin
										affiche_niv_mov(n,m,score_vect.(n-1));
										boxman(n,m,score_vect,temp_actuel,minutes,(x_u,y_u),mur_l,cercles_l,box_l_u,(x_u,y_u),box_l_u)
									end
									else
									begin
										affiche_niv_mov(n,m-1,score_vect.(n-1));
										boxman(n,m-1,score_vect,temp_actuel,minutes,(x_u,y_u),mur_l,cercles_l,box_l_u,(x_u,y_u),box_l_u)
									end
								end;
						|`+`-> if n<> niveau_max then play_niveau(n+1,score_vect); (* niveau suivant*)
						|`-`-> if n<> 1 then play_niveau(n-1,score_vect); (* niveau precedent *)
						|`r` -> play_niveau(n,score_vect); (* recommence le niveau *)
						|_ -> (efface_man(x,y);
								dessine_man(x,y)) ) in boxman(n,m,score_vect,temp_actuel,minutes,(x,y),mur_l,cercles_l,box_l,(x_u,y_u),box_l_u);
			else boxman(n,m,score_vect,temp_actuel,minutes,(x,y),mur_l,cercles_l,box_l,(x_u,y_u),box_l_u)
		end
	end
end;;



let jeu = fun () ->	
	begin
		open_graph"730x680+250+10";
		clear_graph();
		bandes [(0,192,192)];
		bordure_fenetre();
		welcom(true);
		bordure_fenetre();
		menu_fenetre();
		placer_menu();
		placer_mur n1.mur;
		placer_cercles(n1.cercle,n1.box);
		dessine_man n1.man;
		placer_box(n1.box,n1.cercle);
		affiche_niv_mov(1,0,v_score.(0));
		boxman(1,0,v_score,int_of_float(time()),0,n1.man,n1.mur,n1.cercle,n1.box,n1.man,n1.box)
	end;;
try jeu() with QuitJeu -> goodbye();;