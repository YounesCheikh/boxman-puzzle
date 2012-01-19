(*
BoxMan Game
by Younes CHEIKH
Enseignant: Thierry montaut

File downloaded from ww.cheikh.me
contact me : younes.cheikh@gmail.com
*)

(* *************** Fonctions ****************** *)

(* a l'aide de cette fonction fait une couleur arriere plan *)
let bandes lc = let n = list_length lc
                and dx = ref 0 and rx = ref 0
                and c = ref (255,255,255)
                and x = ref 0 and x2 = ref 0
                and h = ref 0
                and l = ref lc
          in if n>0 then
          begin
          (* open_graph ""; *)
          dx := size_x()/n; rx := size_x() mod n;
          h := size_y();
          x2 := 0;
          for i=1 to n
          do let (r,g,b) = hd(!l) 
             in set_color (rgb r g b) ;
             l := tl(!l);
             x := !x2; x2 := !x + !dx;
             if !rx>0 then begin
                           rx := !rx-1; x2 := !x2+1
                           end;
             fill_rect !x 0 (!x2- !x+1) !h
             done;
       end ;;

(* fonction dessine Bourdure du fenetre *)
let bordure_fenetre = fun () ->
begin
	set_color bordure_col;
	fill_rect (default_point_x-bordure_taille) (default_point_y-bordure_taille) (window_large+(2*bordure_taille)) (window_high+(2*bordure_taille));
	set_color default_col;
	fill_rect default_point_x default_point_y window_large window_high
end;;

(* on dessine la fenetre tout au debut *)
(* bordure_fenetre();; *)

(* ---- sleep ----*)
let rec aux_sleep (temp_debut,x) =
begin
	let t = temp_debut in 
		if time() -. temp_debut < x/.1000. then aux_sleep(t,x)
		else ()
end;;

let sleep = fun (x) -> aux_sleep (time(),float_of_int(x));;

(* fonction affiche le numero du nouveau niveau *) 
let nouveau_niveau = fun (n,b) -> (*b est booleen vrai si le joeur à un meilleur score*)
if n>niveau_max then ()
else
begin
	set_color default_col;
	fill_rect default_point_x default_point_y window_large window_high;
	set_color cercle_col1;
	fill_rect (default_point_x+50) (default_point_y+180) (window_large-100) (window_high-(380));
	set_color mur_col1;
	fill_rect (default_point_x+80) (default_point_y+210) (window_large-160) (window_high-(440));
	set_color white;
	fill_rect (default_point_x+100) (default_point_y+230) (window_large-200) (window_high-(480));
	set_color black;
	moveto (default_point_x+110) (default_point_y+275);
	if b=true then 
		begin
			set_color (rgb 0 128 0);
			draw_string "Bravo! Vous avez";
			moveto (default_point_x+110) (default_point_y+255);
			draw_string "Le Meilleur Score :)";
		end
	else
		begin
			set_color (rgb 160 0 0);
			draw_string "Bravo! Mais vous";
			moveto (default_point_x+110) (default_point_y+255);
			draw_string "pouvez faire mieux :(";
		end;
	set_color black;
	moveto (default_point_x+110) (default_point_y+235);
	draw_string "Mission suivante:";
	moveto (default_point_x+255) (default_point_y+235);
	draw_string (string_of_int(n))
end;;

(* nouveau_niveau(5,true);; *)

(* Fonction affiche apres le dernier niveau que le jeu est terminé *)
let fin_niveaux = fun () ->
begin
	set_color default_col;
	fill_rect default_point_x default_point_y window_large window_high;
	set_color cercle_col1;
	fill_rect (default_point_x+50) (default_point_y+200) (window_large-100) (window_high-(400));
	set_color mur_col1;
	fill_rect (default_point_x+80) (default_point_y+230) (window_large-160) (window_high-(460));
	set_color white;
	fill_rect (default_point_x+100) (default_point_y+250) (window_large-200) (window_high-(500));
	set_color black;
	moveto (default_point_x+125) (default_point_y+275);
	draw_string "Bravo! Vous avez";
	moveto (default_point_x+105) (default_point_y+255);
	draw_string "fini toutes les missions";
end;;

(* Fonction affiche quand on quit le jeu *)
let goodbye = fun () ->
begin
	set_color default_col;
	fill_rect default_point_x default_point_y window_large window_high;
	set_color cercle_col1;
	fill_rect (default_point_x+50) (default_point_y+200) (window_large-100) (window_high-(400));
	set_color mur_col1;
	fill_rect (default_point_x+80) (default_point_y+230) (window_large-160) (window_high-(460));
	set_color white;
	fill_rect (default_point_x+100) (default_point_y+250) (window_large-200) (window_high-(500));
	set_color black;
	moveto (default_point_x+105) (default_point_y+275);
	draw_string "Merci d'avoir jouer";
	moveto (default_point_x+105) (default_point_y+255);
	draw_string "ce jeu! good bye!";
	sleep(5000);
	(* quit() *)
end;;

(* fonction dessine la bordure du menu adroite *)
let menu_fenetre = fun () ->
begin
	set_color bordure_col;
	fill_rect (default_point_x+window_large+bordure_taille) (default_point_y-bordure_taille) (menu_large+bordure_taille) (menu_high+(2*bordure_taille));
	set_color bg_menu;
	fill_rect (default_point_x+window_large+bordure_taille) default_point_y menu_large menu_high
end;;

(* on dessine le menu *)
(* menu_fenetre();; *)

(*
la fonction draw rect n'existe pas dans la bibliothèque 'graphics'
cette fonction dessine un rectangle sans le coloré
*)
let draw_rect = fun x y l h ->
begin
	moveto x y;
	lineto (x+l) y;
	lineto (x+l) (y+h);
	lineto x (y+h);
	lineto x y
end;;

(*fonction affiche le temps *)

let afficher_time (mm,ss) = 
let (x,y) = ((default_point_x+window_large+bordure_taille+20),(menu_high+default_point_y-115)) in
begin
	if ss mod 2 = 0 then
	set_color red
	else set_color black;

		moveto (x+10) y;
		if ss<10 then
			begin
				if mm < 10 then
					draw_string ("Temps: "^"0"^string_of_int(mm)^":"^"0"^string_of_int(ss))
				else 
					draw_string ("Temps: "^string_of_int(mm)^":"^"0"^string_of_int(ss))
			end
		else 
			begin
				if mm < 10 then
					draw_string ("Temps: "^"0"^string_of_int(mm)^":"^string_of_int(ss))
				else 
					draw_string ("Temps: "^string_of_int(mm)^":"^string_of_int(ss))
			end
end;;
(* Fonction dessine le menu adroite *)

let affiche_niv_mov = fun (n,m,ms) ->
let (x,y) = ((default_point_x+window_large+bordure_taille+20),(menu_high+default_point_y-100)) in
begin
	set_color (rgb 88 88 88);
	fill_rect (x) (y-80) 160 90;
	set_color white;
	fill_rect (x+5) (y-75) 150 80;
	moveto (x+10) (y-35);
	set_color (rgb 88 88 88);
	draw_string ("Niveau: "^string_of_int(n)^" / "^string_of_int(niveau_max));
	moveto (x+10) (y-55);
	if ms=0 then
	draw_string ("Pas de score!")
	else
	draw_string ("Meilleur Score: "^string_of_int(ms));
	moveto (x+10) (y-75);
	if (m<=ms or ms=0) then set_color (rgb 0 128 0)
	else set_color (rgb 160 0 0);
	draw_string ("deplacements: "^string_of_int(m));
end;;
	

let placer_menu = fun () ->
begin
	set_color black;
	(* LOGO *)
	let (x,y) = ((default_point_x+window_large+bordure_taille+20),(menu_high+default_point_y-105)) in
		begin
			set_color (rgb 88 88 88);
			fill_rect x (y+20) 160 80;
			(* set_color (rgb 168 38 9); *)
			set_color white;
			fill_rect (x+20) (y+40) 120 40;
			set_color black;
			fill_string "BOXMAN" (x+30) (y+50) 3;
			fill_string "BOXMAN" (x+31) (y+51) 3;
			set_color red;
			fill_string "BOXMAN" (x+32) (y+52) 3;
		end;
	
	(* Controls *)
	set_color (rgb 88 88 88);
	let x=(default_point_x+window_large+bordure_taille+20) and y = menu_high+default_point_y-220 in
		begin
			draw_rect x y 30 30;
			moveto (x+15) (y+5);
			lineto (x+15) (y+25);
			lineto (x+20) (y+15);
			moveto (x+15) (y+25);
			lineto (x+10) (y+15);
			moveto (x+35) (y+5);
			draw_string "'8' ou 'z'"
		end;

	(* Les flechs *)
	let x=(default_point_x+window_large+bordure_taille+20) and y = menu_high+default_point_y-260 in
		begin
			draw_rect x y 30 30;
			moveto (x+15) (y+5);
			lineto (x+15) (y+25);
			moveto (x+15) (y+5);
			lineto (x+20) (y+15);
			moveto (x+15) (y+5);
			lineto (x+10) (y+15);
			moveto (x+35) (y+5);
			draw_string "'2' ou 's'"
	end;

	let x=(default_point_x+window_large+bordure_taille+20) and y = menu_high+default_point_y-300 in
		begin
			draw_rect x y 30 30;
			moveto (x+5) (y+15);
			lineto (x+25) (y+15);
			lineto (x+15) (y+10);
			moveto (x+25) (y+15);
			lineto (x+15) (y+20);
			moveto (x+35) (y+5);
			draw_string "'6' ou 'd'"
		end;

	let x=(default_point_x+window_large+bordure_taille+20) and y = menu_high+default_point_y-340 in
		begin
			draw_rect x y 30 30;
			moveto (x+25) (y+15);
			lineto (x+5) (y+15);
			lineto (x+15) (y+10);
			moveto (x+5) (y+15);
			lineto (x+15) (y+20);
			moveto (x+35) (y+5);
			draw_string "'4' ou 'q'"
		end;

	let x=(default_point_x+window_large+bordure_taille+20) and y = menu_high+default_point_y-380 in
		begin
			draw_rect x y 30 30;
			moveto (x+12) (y+5);
			draw_string "U";
			moveto (x+35) (y+5);
			draw_string "Pour annuler"
		end;

	let x=(default_point_x+window_large+bordure_taille+20) and y = menu_high+default_point_y-420 in
		begin
			draw_rect x y 30 30;
			moveto (x+12) (y+5);
			draw_string "R";
			moveto (x+35) (y+5);
			draw_string "Pour Recommencer"
		end;
	(* liens *)
	begin
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
	end
end;;

(* on place le menu *)
(* placer_menu();; *)

(* Fonction affiche l'aide *)
let rec aide = fun (x) ->
let e=wait_next_event[Button_down;Poll]
and (xm,ym) = mouse_pos()
in if e.button then 
	begin
		if xm>(default_point_x+190) & xm<(default_point_x+340) 
		& ym>(default_point_y+220) & ym<(default_point_y+240) then ()
		else aide(x)
	end
else
begin
	if x=true then
	begin
	let (x,y) = ((default_point_x+window_large+bordure_taille+20),(menu_high+default_point_y-120)) in
	begin
		set_color (rgb 88 88 88);
		fill_rect (x) (y-60) 160 90;
		set_color white;
		fill_rect (x+5) (y-55) 150 80;
		moveto (x+40) (y-25);
		set_color (rgb 88 88 88);
		draw_string ("Help...");
	end;
	set_color white;
	fill_rect default_point_x default_point_y window_large window_high;
	set_color red;
	moveto (default_point_x+10) (default_point_y+505);
	draw_string "Help:";
	
	moveto (default_point_x+10) (default_point_y+505);
	lineto (default_point_x+50) (default_point_y+505);
	
	set_color black;
	moveto (default_point_x+10) (default_point_y+480);
	draw_string "Controlez BOXMAN avec les touches de navigation.";
	moveto (default_point_x+10) (default_point_y+460);
	draw_string "La mission consiste à pousser toutes les boîtes";
	moveto (default_point_x+10) (default_point_y+440);
	draw_string "vers la zone de destination. Vous engagez la";
	moveto (default_point_x+10) (default_point_y+420);
	draw_string "mission suivante une fois la mission en cours";
	moveto (default_point_x+10) (default_point_y+400);
	draw_string "terminée.";
	moveto (default_point_x+10) (default_point_y+380);
	draw_string "Appuyer sur la touche 'q' ou sur '4' pour aller ";
	moveto (default_point_x+10) (default_point_y+360);
	draw_string "vers la gauche. Appuyer sur la touche 'd' ou sur";
	moveto (default_point_x+10) (default_point_y+340);
	draw_string "'6' pour aller vers la droite.";
	moveto (default_point_x+10) (default_point_y+320);
	draw_string "Appuyer sur la touche 'z' ou sur '8' pour aller";
	moveto (default_point_x+10) (default_point_y+300);
	draw_string "vers le haut. Appuyer sur la touche 's' ou sur";
	moveto (default_point_x+10) (default_point_y+280);
	draw_string "'2' pour aller vers le bas.";
	moveto (default_point_x+10) (default_point_y+260);
	draw_string "'u' ou '5' pour annuler. 'R' pour recommencer. ";
	set_color red;
	draw_rect (default_point_x+190) (default_point_y+220) 150 20;
	moveto (default_point_x+200) (default_point_y+220);
	draw_string "Retourner au jeu ";
	end;
	aide(false)
end;;

(* aide();; *)

(*fonction dessine le box*)
let dessine_box = fun (x,y) ->
begin
	set_color box_col;
	fill_poly [| x+1,y+1;x+box_taille-2,y+1;x+box_taille-6,y+5;x+6,y+5 |];
	fill_poly [|x+box_taille-2,y+1;x+box_taille-2,y+box_taille-2;x+box_taille-5,y+box_taille-5;x+box_taille-5,y+5|];
	set_color box_col2;
	fill_poly [| x+box_taille-2,y+box_taille-2;x+1,y+box_taille-2;x+5,y+box_taille-5;x+box_taille-5,y+box_taille-5 |];
	fill_poly [| x+1,y+box_taille-2;x+1,y+2;x+5,y+5;x+5,y+box_taille-5 |];
	set_color box_col3;
	fill_rect (x+6) (y+6) 39 39
end;;

(*fonction dessine le box*)
let dessine_box2 = fun (x,y) ->
begin
	set_color box_col4;
	fill_poly [| x+1,y+1;x+box_taille-2,y+1;x+box_taille-6,y+5;x+6,y+5 |];
	fill_poly [|x+box_taille-2,y+1;x+box_taille-2,y+box_taille-2;x+box_taille-5,y+box_taille-5;x+box_taille-5,y+5|];
	set_color box_col5;
	fill_poly [| x+box_taille-2,y+box_taille-2;x+1,y+box_taille-2;x+5,y+box_taille-5;x+box_taille-5,y+box_taille-5 |];
	fill_poly [| x+1,y+box_taille-2;x+1,y+2;x+5,y+5;x+5,y+box_taille-5 |];
	set_color box_col6;
	fill_rect (x+6) (y+6) 39 39
end;;

(*fonction efface le box*)
let efface_box = fun (x,y) ->
begin
	set_color default_col;
	fill_rect x y box_taille box_taille
end;;

(*
dessine_box (default_point_x,default_point_y);; 
*)

(*fonction dessine le cercle*)
let dessine_cercle = fun (x,y) ->
begin
	set_color cercle_col1;
	fill_circle (x+cercle_taille+5) (y+5+cercle_taille-1) cercle_taille;
	set_color cercle_col2;
	fill_circle (x+cercle_taille+10) (y+3+cercle_taille-1) ((cercle_taille/2)-2)
end;;

(*
dessine_cercle(default_point_x,default_point_y);;
*)

(*fonction dessine un carré de mur*)
let dessine_mur = fun (x,y) ->
begin
	set_color mur_col1;
	fill_rect x y mur_taille mur_taille;
	set_color mur_col2;
	fill_rect (x+1) (y+1) 30 15 ;
	fill_rect (x+32) (y+1) 17 15 ;
	fill_rect (x+1) (y+18) 17 14 ;
	fill_rect (x+19) (y+18) 30 14 ;
	fill_rect (x+1) (y+34) 30 15 ;
	fill_rect (x+32) (y+34) 17 15 ;
end;;

(*
dessine_mur (default_point_x,default_point_y);;
*)

(* fonction dessine le joueur *) 

let dessine_man_bas = fun (x,y) ->
begin
	set_color (rgb 255 192 114);
	fill_rect (x+default_point_x+10) (y+default_point_y) 5 15;
	fill_rect (x+default_point_x+35) (y+default_point_y) 5 15;
	set_color (rgb 128 0 0);
	fill_ellipse (x+default_point_x+25) (y+default_point_y+12) 15 10;
	set_color black;
	draw_ellipse (x+default_point_x+25) (y+default_point_y+12) 15 10;
	set_color (rgb 255 192 114);
	fill_circle (x+default_point_x+25) (y+default_point_y+18) 10;
	set_color black;
	draw_circle (x+default_point_x+25) (y+default_point_y+18) 10;
	set_color black;
	fill_ellipse (x+default_point_x+25) (y+default_point_y+20) 10 8
end;;

let dessine_man_haut = fun (x,y) ->
begin
	set_color (rgb 255 192 114);
	fill_rect (x+default_point_x+10) (y+default_point_y+man_taille-15) 5 15;
	fill_rect (x+default_point_x+35) (y+default_point_y+man_taille-15) 5 15;
	set_color (rgb 128 0 0);
	fill_ellipse (x+default_point_x+25) (y+default_point_y+man_taille-12) 15 10;
	set_color black;
	draw_ellipse (x+default_point_x+25) (y+default_point_y+man_taille-12) 15 10;
	set_color (rgb 255 192 114);
	fill_circle (x+default_point_x+25) (y+default_point_y+man_taille-18) 10;
	set_color black;
	draw_circle (x+default_point_x+25) (y+default_point_y+man_taille-18) 10;
	set_color black;
	fill_ellipse (x+default_point_x+25) (y+default_point_y+man_taille-20) 10 8
end;;

let dessine_man_gauche = fun (x,y) ->
begin
	set_color (rgb 255 192 114);
	fill_rect (x+default_point_x) (y+default_point_y+10) 15 5;
	fill_rect (x+default_point_x) (y+default_point_y+35) 15 5;
	set_color (rgb 128 0 0);
	fill_ellipse (x+default_point_x+12) (y+default_point_y+25) 10 15;
	set_color black;
	draw_ellipse (x+default_point_x+12) (y+default_point_y+25) 10 15;
	set_color (rgb 255 192 114);
	fill_circle (x+default_point_x+18) (y+default_point_y+25) 10;
	set_color black;
	draw_circle (x+default_point_x+18) (y+default_point_y+25) 10;
	set_color black;
	fill_ellipse (x+default_point_x+20) (y+default_point_y+25) 8 10
end;;


let dessine_man_droite = fun (x,y) ->
begin
	set_color (rgb 255 192 114);
	fill_rect (x+default_point_x+man_taille-15) (y+default_point_y+10) 15 5;
	fill_rect (x+default_point_x+man_taille-15) (y+default_point_y+35) 15 5;
	set_color (rgb 128 0 0);
	fill_ellipse (x+default_point_x+man_taille-12) (y+default_point_y+25) 10 15;
	set_color black;
	draw_ellipse (x+default_point_x+man_taille-12) (y+default_point_y+25) 10 15;
	set_color (rgb 255 192 114);
	fill_circle (x+default_point_x+man_taille-18) (y+default_point_y+25) 10;
	set_color black;
	draw_circle (x+default_point_x+man_taille-18) (y+default_point_y+25) 10;
	set_color black;
	fill_ellipse (x+default_point_x+man_taille-20) (y+default_point_y+25) 8 10
end;;

let dessine_man = fun (x,y) ->
begin
	set_color black;
	fill_rect (x+default_point_x+15) (y+default_point_y) 5 15;
	fill_rect (x+default_point_x+30) (y+default_point_y) 5 15;
	set_color (rgb 128 0 0);
	fill_ellipse (x+default_point_x+25) (y+default_point_y+17) 12 12;
	set_color black;
	draw_ellipse (x+default_point_x+25) (y+default_point_y+17) 12 12;
	set_color (rgb 255 192 114);
	fill_circle (x+default_point_x+25) (y+default_point_y+30) 10;
	set_color black;
	draw_circle (x+default_point_x+25) (y+default_point_y+30) 10;
	set_color black;
	fill_ellipse (x+default_point_x+25) (y+default_point_y+37) 8 4;
 	set_color (rgb 255 192 114);
	fill_circle (x+default_point_x+15) (y+default_point_y+17) 4;
	fill_circle (x+default_point_x+35) (y+default_point_y+17) 4;
	set_color black;
	fill_circle (x+default_point_x+20) (y+default_point_y+30) 2;
	fill_circle (x+default_point_x+30) (y+default_point_y+30) 2;
	draw_arc (x+default_point_x+25) (y+default_point_y+26) 5 2 180 0;
end;;

(* fonction efface le joueur*)
let efface_man = fun (x,y) ->
begin
	set_color default_col;
	fill_rect (x+default_point_x) (y+default_point_y) man_taille man_taille
end;;

(*
dessine_man (default_point_x,default_point_y);;
*)

(*
Fonction verif si un point inclus dans un liste ou pas
On utilise cette fonction dans la fonction suivante pour compare deux liste
*)
let rec point_in_liste = fun
(_,[]) -> false
|((x,y),(a,b)::l) -> if (x,y) = (a,b) then true
					else point_in_liste((x,y),l);;

(*
point_in_liste((9,11),l2);;
*)

(*fonction construit le mur*)
let rec placer_mur = fun 
[] -> ()
|((x,y)::l) -> begin 
					dessine_mur(x+default_point_x,y+default_point_y);
					placer_mur(l)
				end;;
(*
placer_mur mur_liste3;;				
*)

(*fonction place les cercles dans leur points*)

let rec placer_cercles = fun 
([],_) -> ()
|((x,y)::l,l2) -> 
				begin 
					if point_in_liste((x,y),l2) then placer_cercles(l,l2)
					else
					(dessine_cercle(x+default_point_x,y+default_point_y);
					placer_cercles(l,l2))
				end;;
(* let rec placer_cercles = fun 
([]) -> ()
|((x,y)::l) -> 
				begin 
					dessine_cercle(x+default_point_x,y+default_point_y);
					placer_cercles(l)
				end;; *)

(*			
placer_cercles cercles_liste3;;
*)

(*fonction place les boxs dans leur points*)

let rec placer_box = fun 
([],_) -> ()
|((x,y)::l,l2) -> 
				begin 
					if point_in_liste((x,y),l2) then 
					(dessine_box2(x+default_point_x,y+default_point_y);placer_box(l,l2))
					else
					(dessine_box(x+default_point_x,y+default_point_y);
					placer_box(l,l2))
				end;;
				
(* let rec placer_box = fun 
([]) -> ()
|((x,y)::l) -> 
				begin 
					dessine_box(x+default_point_x,y+default_point_y);
					placer_box(l)
				end;; *)

(*				
placer_box box_liste3;;
*)

(*fonction met à jour les cordonnees des boxs *)
let rec update_liste = fun 
([],b,n) -> []
|(a::l,b,n) -> 	if a=b then n::update_liste(l,b,n)
				else a::update_liste(l,b,n);; 

(* les fonctions suivantes verif la limite d'un rectangle avec un liste *)

(* verife points y (haut) *)		
let rec verif_point_y_positif = fun 
(_,_,[]) -> true
|(x,y,(a,b)::l) -> if (b <> y+man_taille) then verif_point_y_positif(x,y,l)
					else
						begin
							if  (x = a) then false
							else verif_point_y_positif(x,y,l)
						end;;

(* verife points y (bas) *)	
let rec verif_point_y_negatif = fun 
(_,_,[]) -> true
|(x,y,(a,b)::l) -> if (b+man_taille <> y) then verif_point_y_negatif(x,y,l)
					else
						begin
							if  (x = a) then false
							else verif_point_y_negatif(x,y,l)
						end;;

(* verife points x (droite) *)							
let rec verif_point_x_positif = fun 
(_,_,[]) -> true
|(x,y,(a,b)::l) -> if (a-man_taille <> x) then verif_point_x_positif(x,y,l)
					else
						begin
							if  (y = b) then false
							else verif_point_x_positif(x,y,l)
						end;;

(* verife points x (gauche) *)	
let rec verif_point_x_negatif = fun 
(_,_,[]) -> true
|(x,y,(a,b)::l) -> if (a+man_taille <> x) then verif_point_x_negatif(x,y,l)
					else
						begin
							if  (y = b) then false
							else verif_point_x_negatif(x,y,l)
						end;;


(*
Fonction compare deux liste 
on utilise cette fonction pour verifer si touts les boxs cachent toutes les cercles
*)
let rec compare_2_listes = fun
([],_) -> true
|((a::l1),l2) -> if point_in_liste(a,l2) then true & compare_2_listes(l1,l2)
				else false;;
(*				
compare_2_listes(l1,l2);;
*)

(* ********************** Gestion du score ************** *)
let charger_score = fun(v,f) ->
	try (for i = 0 to (niveau_max-1)
	do
		v.(i) <- (int_of_string(input_line f))
	done)
with End_of_file -> ();;

let v_score=make_vect niveau_max 0;;

let f file_name =
      try 
          let inchan = open_in file_name in
          let line = input_line inchan in ()
      with
          _ -> let tmp = open_out file_name in 
               close_out tmp;;

f "score.txt";;
let f_score= open_in "score.txt";;

charger_score(v_score,f_score);;
close_in(f_score);;



let compare_score = fun (n,m,v) ->
	if v.(n)=0 or m<v.(n) then true (* on laisse m<v.(n) et pas m<=v.(n)*)
	else false ;;
	
(* compare_score (0,5,v_score);; *)

let update_score = fun (n,m,v) -> 
begin 
	v.(n) <- m ;
	v;
end;;

let sauvgarder_nouveau_score(v,f) =
	for i=0 to (niveau_max-1) 
	do
		if i<>(niveau_max-1) then output_string f (string_of_int(v.(i))^"\n")
		else output_string f (string_of_int(v.(i)))
	done;;

	
let dessine_man_welcom = fun (x,y,t) ->
begin
	set_color black;
	fill_rect (x+default_point_x+15*t) (y+default_point_y) (5*t) (15*t);
	fill_rect (x+default_point_x+30*t) (y+default_point_y) (5*t) (15*t);
	set_color (rgb 128 0 0);
	fill_ellipse (x+default_point_x+25*t) (y+default_point_y+17*t) (12*t) (12*t);
	set_color black;
	draw_ellipse (x+default_point_x+25*t) (y+default_point_y+17*t) (12*t) (12*t);
	set_color (rgb 255 192 114);
	fill_circle (x+default_point_x+25*t) (y+default_point_y+30*t) (10*t);
	set_color black;
	draw_circle (x+default_point_x+25*t) (y+default_point_y+30*t) (10*t);
	set_color black;
	fill_ellipse (x+default_point_x+25*t) (y+default_point_y+37*t) (8*t) (4*t);
 	set_color (rgb 255 192 114);
	fill_circle (x+default_point_x+15*t) (y+default_point_y+17*t) (4*t);
	fill_circle (x+default_point_x+35*t) (y+default_point_y+17*t) (4*t);
	(*le yeux*)
	set_color white;
	fill_circle (x+default_point_x+20*t) (y+default_point_y+30*t) (2*t);
	fill_circle (x+default_point_x+30*t) (y+default_point_y+30*t) (2*t);
	set_color black;
	fill_circle (x+default_point_x+(21*t)+1) (y+default_point_y+30*t) (1*t);
	fill_circle (x+default_point_x+(29*t)-1) (y+default_point_y+30*t) (1*t);
	(*les lunettes*)
	set_color black;
	draw_circle (x+default_point_x+20*t) (y+default_point_y+30*t) (3*t);
	draw_circle (x+default_point_x+30*t) (y+default_point_y+30*t) (3*t);
	moveto (x+default_point_x+12+20*t) (y+default_point_y+30*t);
	lineto (x+default_point_x+28+20*t) (y+default_point_y+30*t);
	(*la bouche *)
	set_color black;
	draw_arc (x+default_point_x+25*t) (y+default_point_y+26*t) (5*t) (2*t) (180) (0*t);

end;;

let rec welcom = fun (b) -> (* message de bienvenue *)
let e = wait_next_event[Button_down;Poll] 
and (xm,ym)=mouse_pos() in
if e.button then 
	begin
		if xm >= (default_point_x+280) & xm <=(default_point_x+390)
		& ym>=(default_point_y+100) & ym<=(default_point_y+120) then ()
		else welcom(false)
	end
else
begin
	if b=true then (* si b true on dessine si non on dessine pas *)
	begin
		set_color white;
		fill_rect default_point_x default_point_y window_large window_high;
		set_color (rgb 0 0 0);
		fill_rect (default_point_x+10) (default_point_y+410) (window_large-20) (window_high-420);
		menu_fenetre();
		placer_menu();
		affiche_niv_mov(1,0,v_score.(0));
		set_color (rgb 160 160 160);
		fill_string "BOXMAN" (default_point_x+28) (default_point_y+448) 10;
		set_color green;
		fill_string "BOXMAN" (default_point_x+30) (default_point_y+450) 10;
		dessine_man_welcom(100,0,4);
			(* le text *)
		set_color (rgb 160 160 160);
		fill_rect (default_point_x+80) (default_point_y+175) (250) (220);
		set_color white;
		fill_rect (default_point_x+85) (default_point_y+180) (240) (210);
		set_color black;
		moveto (default_point_x+120) (default_point_y+370);
		draw_string "Centre universitaire";
		moveto (default_point_x+120) (default_point_y+370);
		lineto (default_point_x+280) (default_point_y+370);
		moveto (default_point_x+105) (default_point_y+345);
		draw_string "Jean Francois Champollion";
		moveto (default_point_x+105) (default_point_y+345);
		lineto (default_point_x+300) (default_point_y+345);
		moveto (default_point_x+115) (default_point_y+320);
		draw_string "Licence Informatique";
		moveto (default_point_x+115) (default_point_y+320);
		lineto (default_point_x+310) (default_point_y+320);
		moveto (default_point_x+150) (default_point_y+295);
		draw_string "Boxman Game";
		moveto (default_point_x+150) (default_point_y+295);
		lineto (default_point_x+240) (default_point_y+295);
		set_color (rgb 160 160 160);
		fill_rect (default_point_x+100) (default_point_y+270) 200 5;
		set_color black;
		moveto (default_point_x+100) (default_point_y+245);
		draw_string "By Younes CHEIKH";
		moveto (default_point_x+100) (default_point_y+220);
		draw_string "younes.cheikh@gmail.com";
		moveto (default_point_x+100) (default_point_y+195);
		draw_string "Website: www.cheikh.me";
		set_color red;
		draw_rect (default_point_x+280) (default_point_y+100) 110 20;
		moveto (default_point_x+300) (default_point_y+100);
		draw_string "Commencer";
		welcom(false)
	end
	else welcom(false)
end;;