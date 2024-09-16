(** Moteur de Jeux pour le terminal *)

(** {1 Géométrie de l'écran} *)

val width : float
(** Largeur de l'écran de jeu en nombre de caractères *)

val height : float
(** Hauteur de l'écran de jeu en nombre de caractères *)

(** {1 Gestion des entrées clavier} *)

(** Type pour les touches du clavier,
le constructeur [Special] correspond à des touches
particulières qui ne devraient pas être utilisées *)
type key =
  | LeftArrow
  | RightArrow
  | UpArrow
  | DownArrow
  | Char of char
  | Special of string

type key_mod = (key * bool * bool) option
(** Le type pour les touches avec en plus les "modificateurs",
Le premier booléen correspond à la touche Shift et le second à la touche Ctrl.
La valeur [None] représente qu'aucune touche n'est pressée. *)

val string_of_key : key_mod -> string
(** Renvoie une chaîne de caractère qui décris la touche pressée*)

(** {1 Vecteur du plan} *)

type vec = float * float
(** Un type pour les vecteurs du plan *)

val ( +.. ) : vec -> vec -> vec
(** L'addition des vecteurs dans le plan *)

val ( -.. ) : vec -> vec -> vec
(** La soustraction des vecteurs dans le plan *)

val ( *.. ) : float -> vec -> vec
(* La multiplication par un scalaire du plan *)

(** {1 Entité du moteur et moteur physique} *)

type solid = vec * string
(** Les solides sont donnés par un vecteur représentant son coin en bas à gauche
et une chaîne de caractères qui dessine le solide *)

val string_unicode_fold_left : ('a -> string -> 'a) -> 'a -> string -> 'a
(** Itere sur une chaine de caractère en prenant un caractère unicode à chaque fois*)

val get_bounds : string -> vec
(** Renvoie les dimensions d'une chaîne de caractères. Comparée à [String.length], cette
fonction prend en compte les sauts de ligne ['\n'] comme une dimension verticale.
Elle ignore les espaces et calcule correctement la taille des caractères Unicode.
C'est-à-dire qu'elle renvoie les dimensions d'un rectangle tel que les caractères visibles
de la chaîne de caractères sont tous inclus dans le rectangle. *)

val is_above : solid -> solid -> bool
(** Renvoie vrais si le premier solide est au dessus du second *)

val is_below : solid -> solid -> bool
(** Renvoie vrais si le premier solide est en dessous du second *)

val is_right : solid -> solid -> bool
(** Renvoie vrais si le premier solide est à droite du second *)

val is_left : solid -> solid -> bool
(** Renvoie vrais si le premier solide est à gauche du second *)

val collision : solid -> solid -> bool
(** Indique si deux objets sont en collision *)

val update_physics :
  solid ->
  vec ->
  vec list ->
  solid list ->
  vec * vec * (solid option * solid option)
(** [update_physics obj vitesse forces obstacles] renvoie un triplet
[(npos, nvitesse, contacts)] qui correspond à la nouvelle position ([npos]) et vitesse ([nvitesse])
de [obj] à partir de son ancienne position et vitesse, des [forces] qui lui sont appliquées
et des éventuelles collisions avec des [obstacles]. La valeur [contacts] est une paire de solide
optionnelle qui indique si [obj] est en contact avec un obstacle horizontalement ou verticalement. *)

val gravity : vec
(** Un vecteur représentant la force de la gravité *)

(** {1 Boucle principale} *)

val loop :
  'state -> ('state -> key_mod -> 'state) -> ('state -> solid list) -> 'a
(** Boucle principale du programme.
Le premier argument est l'état initial du système.
Le second argument est une fonction qui, à partir de l'état d'un système
et des touches pressées, calcule le nouvel état du système.
Le troisième argument est une fonction qui renvoie la liste des éléments à afficher.
Cette fonction ne retourne jamais. *)
