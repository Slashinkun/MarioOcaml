open Engine

(*Comment demarrer le jeu ?
   -Requis : WSL sur le PC et VSCode
   1-Demarrer un terminal(cmd) et aller dans le dossier où contient le fichier project.ml (avec le terminal)
   2-a) Si le fichier project sans extention existe alors faire ./project pour demarrer le jeu
   -b) sinon faire make pour generer le fichier project et puis faire instruction 2a)
   
   Pour quitter le jeu : appuyer sur echap
*)

(*Controles :
   Q/D  Deplacer a gauche/droite
   Z    Sauter
   *)


let personnage =
   " o \n/O\\\n ^"
(* Calcule la taille en nombre de caractères horizontaux et verticaux du message*)

let h_width, h_height = get_bounds personnage


(*Cree un bloc de w largeur et h hauteur de caractere c*)
let rec build_block c w h = 
   let rec build_block_w c w = if w <= 0 then "" else Char.escaped c  ^ build_block_w c (w-1)  in
   if h <= 0 then "" else "\n" ^ (build_block_w c w)  ^  (build_block c w (h-1));;




(*------Image(Sprite) et Acteurs(Les trucs)-------*)

let sol = (build_block '#'  120 2)
let sol_20 = (build_block '#' 60 2)

let mur1_sprite = (build_block '#' 5 5)
let mur2_sprite = (build_block '#' 5 9)
 

let plateforme_sprite = (build_block '#' 25 2)

let ennemi = "&&\n||"
let piece = "€"
let bloc_c ="----\n|? |\n----"


let obstacle1 = ((0.0,0.0),sol) (*Objet sol*)
let obstacle2= ((127.0,0.0),sol_20)
let obstacle3 = ((186.0,0.0),sol_20)
let obstacle4 = ((245.0,0.0),sol)
let plateforme = ((40.0,10.0),plateforme_sprite)
let plateforme1 = ((246.0,10.0),plateforme_sprite)
let plateforme2 = ((266.0,20.0),plateforme_sprite)


let mur1 = ((70.0,2.0),mur1_sprite)
let mur2 = ((100.0,2.0),mur2_sprite)
let mur3 = ((142.0,2.0),mur2_sprite)
let mur4 = ((172.0,2.0),mur1_sprite)


let mob1= ((50.0, 4.0),ennemi)
let mob2= ((60.0,4.0),ennemi)
let mob3 =((203.0,2.0),ennemi)
let mob4 = ((254.0,2.0),ennemi)
let mob5 = ((257.0,12.0),ennemi)

let piece1 = ((8.0,21.0),piece)
let piece2 = ((16.0,2.0),piece)
let piece3 = ((50.0,12.0),piece)
let piece4 = ((100.0,20.0),piece)
let piece5 = ((157.0,7.0),piece)

let bloc1 =((25.0,10.0),bloc_c)
let bloc2 = ((50.0,18.0),bloc_c)

let l_ennemis = [mob1;mob2;mob3;mob4]
let l_obstacles = [obstacle1;obstacle2;obstacle3;obstacle4;plateforme;plateforme1;plateforme2;mur1;mur2;mur3;mur4]
let l_pieces = [piece1;piece2;piece3;piece4;piece5]
let l_blocs = [bloc1;bloc2]

(*----Fonctions et etc-----*)

let vies = 5 (*C'est les vies*)

(* Concatène deux tableaux ensemble *)
let rec concatene_tab l1 l2 = match l1 with
   | [] -> l2
   | t::q -> t::(concatene_tab q l2)


(*Translation des objets en parametres avec le vecteur*)
let rec translate v liste_obj = match liste_obj with 
  |[] -> []
  |tete :: queue -> (match tete with 
                    |(coord,sprite) ->  (coord +.. v,sprite) :: translate v queue)



(* État initial du système, c'est un couple de vecteurs, le premier contient
   la position du message, le second contient sa direction de déplacement *)

let init_state = ((2.0,4.0),(0.0,0.0),(l_obstacles,l_ennemis,l_pieces,l_blocs,vies)) (*Etat au debut du jeu*)


(*Met a jour l'etat des ennemis dans le jeu
   le premier c'est la liste des ennemis et le second c'est le personnage
   Renvoie une liste de solid contenant les ennemis mis a jour*)
let rec  update_mob l p = match l with 
|[] -> []
|t::q -> (let npos,nvitesse,contact = (update_physics t (-.2.0,0.0) [gravity]  (concatene_tab l_obstacles [p])) in (*On met a jour la physique*)
(match contact with (*On regarde ses collisions*)
 |(None,None) -> (npos,ennemi) :: (update_mob q p)
 |(None,Some e) -> if collision t p then  (update_mob q p) else (npos,ennemi) :: (update_mob q p)
 |(Some c, Some e) -> (npos,ennemi)  :: (update_mob q p)
)
)



(*Met a jour l'etat des pieces dans le jeu
le premier c'est la liste des pieces et le second c'est le personnage*)
let rec update_coins l p = match l with
|[] -> []
|t::q ->  (let npos,nvitesse,contact = (update_physics t (0.0,0.0) []  [p]) in (*On met a jour la physique*)
(match contact with (*On regarde ses collisions*)
|(None,None) -> (npos,piece) :: (update_coins q p)
| _ -> (update_coins q p)
)
)

(*Met a jour l'etat des blocs cassables dans le jeu
   le premier c'est la liste des blocs cassables et le second c'est le personnage*)
let rec update_bloc l p = match l with
|[] -> []
|t::q -> (let npos,nvitesse,contact = (update_physics t (0.0,0.0) []  [p]) in (*On met a jour la physique*)
(match contact with (*On regarde ses collisions*)
|(None,None) -> t :: (update_bloc q p) 
|(None,Some e) -> (update_bloc q p)
|(Some c,None) ->  t:: (update_bloc q p)  )
)

(* Cette fonction est appelée de manière répétée plusieurs fois par seconde pour mettre à jours
   l'état du système. Ici l'état est un couple composé de la position du message et de sa direction.
   Elle doit renvoyer le nouvel état.*)
let update ((x, y), (xincr, yincr),(l_obstacle,l_mobs,l_coins,l_bloc,vies)) key = 

   let (perso:solid) =((x,y),personnage) in  (*Objet perso*)

   (*Mise a jour des ennemis,pieces,blocs*)
   let l_enns = (update_mob l_mobs perso) in
   let l_coincoin = (update_coins l_coins perso ) in
   let l_blocbloc = (update_bloc l_bloc perso ) in

   let jeu = (concatene_tab l_enns l_obstacle) in
   
   
   
   (*Deplacement du perso*)
   let npos,nvitesse,contact =  
   match key with
   |Some (Char 'q',false,c)-> update_physics perso (-10.0,0.0) [gravity] jeu
   |Some (Char 'd',false,c)-> update_physics perso (10.0,0.0) [gravity]  jeu
   |Some (Char 'q',true,c)-> update_physics perso (-20.0,0.0) [gravity]  jeu
   |Some (Char 'd',true,c)-> update_physics perso (20.0,0.0) [gravity]   jeu
   |Some (Char 'z',false,c)-> update_physics perso (0.0,30.0) [gravity] l_obstacle (*Saut*)
   | _ -> update_physics perso (0.0,0.0) [gravity] l_obstacle in
    
   
   

   (*Collision avec les acteurs et saut dans le vide*)

   (*Si le perso est hors de l'ecran (dans le vide)*) 
   if y < -3. then ((0.0,5.0), nvitesse,(l_obstacle,l_enns,l_coincoin,l_blocbloc,(vies-1))) else
   match contact with (*Si il rentre en collision avec quelque chose*)
   |(Some c, Some e ) -> ((0.0,5.0), nvitesse,(l_obstacle,l_enns,l_coincoin,l_blocbloc,(vies-1)))
   |(None,None)-> (npos, nvitesse,(l_obstacle,l_enns,l_coincoin,l_blocbloc,vies))
   |_ ->  (npos, nvitesse,(l_obstacle,l_enns,l_coincoin,l_blocbloc,vies))
  
   

   
   
  
 
 
(* Affiche l'état du système, ici le message à la position pos, et les nombres 1,2,3,4 aux quatre coins
de l'écran. *)
let affiche (pos, _,(l_obstacle,l_ennemi,l_coins,l_bloc,vies)) =
   
   let posx,posy = pos in
   
   let gameover = [(width/.2.,height/.2.),"GAME OVER"] in
   let jeu = (concatene_tab (l_bloc) (concatene_tab l_coins ((concatene_tab (concatene_tab [(pos,personnage)]  l_obstacle) (l_ennemi))))) in
   
   let compteurvies = [((0.0,35.0),"Vies: " ^ string_of_int(vies))] in (*Pour debugger*)

   
   if vies <=0 then gameover else if pos > ((width/.2.),0.0) 
      then  concatene_tab compteurvies (translate ((width/.2.-.posx),0.0) jeu)
    else   
     concatene_tab compteurvies jeu
            
    


(* Appelle de la boucle principale le premier argument est l'état initial du système.
   Les autres arguments sont les fonctions update et affiche*)
let _ = loop init_state update affiche 
