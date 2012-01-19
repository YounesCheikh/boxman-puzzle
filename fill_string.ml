(*
BoxMan Game
by Younes CHEIKH
Enseignant: Thierry montaut

File downloaded from ww.cheikh.me
contact me : younes.cheikh@gmail.com
*)
let write_char = fun
("A",t,x,y) -> begin 
					for i=0 to 5 do
					fill_circle x (y+t*i) (t/2);
					done;
					for i=1 to 3 do
					fill_circle (x+t*i) (y+t*6) (t/2);
					done;
					for i=0 to 5 do
					fill_circle (x+4*t) (y+t*i) (t/2);
					done;
					for i=1 to 3 do
					fill_circle (x+t*i) (y+t*3) (t/2);
					done;
				end;
|("B",t,x,y) -> begin
					for i=0 to 6 do
					fill_circle x (y+t*i) (t/2);
					done;
					for i=1 to 3 do
					fill_circle (x+t*i) (y+t*6) (t/2);
					done;
					for i=1 to 3 do
					fill_circle (x+t*i) (y+t*3) (t/2);
					done;
					for i=1 to 3 do
					fill_circle (x+t*i) (y) (t/2);
					done;
					for i=1 to 2 do
					fill_circle (x+t*4) (y+t*i) (t/2);
					done;
					for i=4 to 5 do
					fill_circle (x+t*4) (y+t*i) (t/2);
					done;
				end;
|("O",t,x,y) -> begin
					for i=1 to 5 do
					fill_circle x (y+t*i) (t/2);
					done;
					for i=1 to 5 do
					fill_circle (x+t*4) (y+t*i) (t/2);
					done;
					for i=1 to 3 do
					fill_circle (x+t*i) (y+t*6) (t/2);
					done;
					for i=1 to 3 do
					fill_circle (x+t*i) (y) (t/2);
					done;
				end;
|("X",t,x,y) -> begin
					for i=0 to 1 do
					fill_circle x (y+t*i) (t/2);
					done;
					for i=5 to 6 do
					fill_circle x (y+t*i) (t/2);
					done;
					for i=0 to 1 do
					fill_circle (x+t*4) (y+t*i) (t/2);
					done;
					for i=5 to 6 do
					fill_circle (x+t*4) (y+t*i) (t/2);
					done;
					for i=2 to 4 do
					fill_circle (x+t*(i-1)) (y+t*i) (t/2);
					done;
					for i=2 to 4 do
					fill_circle (x+t*(i-1)) (y-t*(i-6)) (t/2);
					done;
				end;
|("M",t,x,y) -> begin
					for i=0 to 6 do
					fill_circle x (y+t*i) (t/2);
					done;
					for i=0 to 6 do
					fill_circle (x+t*4) (y+t*i) (t/2);
					done;
					for i=3 to 4 do
					fill_circle (x+t*2) (y+t*i) (t/2);
					done;
					fill_circle (x+t) (y+t*5) (t/2);
					fill_circle (x+t*3) (y+t*5) (t/2);
				end;
|("N",t,x,y) -> begin
					for i=0 to 6 do
					fill_circle x (y+t*i) (t/2);
					done;
					for i=0 to 6 do
					fill_circle (x+t*4) (y+t*i) (t/2);
					done;
					fill_circle (x+t) (y+t*4) (t/2);
					fill_circle (x+t*2) (y+t*3) (t/2);
					fill_circle (x+t*3) (y+t*2) (t/2);
				end;
				
|(_,_,_,_) -> ();;
(* write_char("N",10,100,100);; *)
let tetec = fun s->
if s= "" then failwith "Erreur : chaine vide!"
else nth_char s 0;;


let tetes = fun s->string_of_char (tetec(s));;

let reste = fun s->
if s="" then "Erreur : chaine vide!"
else sub_string s 1 (string_length s -1);;

let rec fill_string s x y t=
if s="" then () 
else
	begin
		write_char(tetes(s),t,x,y);
		fill_string (reste(s)) (x+6*t) y t
	end;;

(* fill_string "BOXMAN" 50 100 10;; *)