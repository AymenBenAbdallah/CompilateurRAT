open Type 

(* Définition du type des informations associées aux identifiants *)
type info =
  (* Information associée à une constante : son nom (non indispensable mais aide au test et debbugage) et sa valeur *)
  | InfoConst of string * int
  (* Information associée à une variable : son nom (non indispensable mais aide au test et debbugage),
  son type, et son adresse ie son déplacement (int) par rapport à un registre (string) *)
  | InfoVar of string * typ * int * string
  (* Information associée à une fonction : son nom (utile pour l'appel), son type de retour et la liste des types des paramètres *)
  | InfoFun of string * typ * typ list

(* Table des symboles *)
type tds 

(* Données stockées dans la tds et dans les AST : pointeur sur une information *)
type info_ast

(* Création d'une table pour stocker les fonctions déclarés *)
(* Retourne : une hashtable vide de (string représentant le nom de la fonction, info_ast de la fonction)  qui servira au stockage des fonctions déclaréss *)
val creerTableFonc : unit -> (string, info_ast) Hashtbl.t

(* Ajoute une fonction dans la table des fonctions *)
(* Paramètre H : la hashtable à laquelle va être ajoutée la fonction *)
(* Paramètre n : le nom de la fonction à ajouter *)
(* Paramètre info : l'info à ajouter *)
(* Retour : unit *)
val ajouter_fonc : (string,info_ast) Hashtbl.t -> string -> info_ast -> unit

(* Ajoute une information dans la table des symboles locale *)
(* tds : la tds courante *)
(* string : le nom de l'identificateur *)
(* info : l'information à associer à l'identificateur *)
(* retour : unit *)
val ajouter : tds -> string -> info_ast -> unit 

(* Création d'une table des symboles à la racine *)
val creerTDSMere : unit -> tds 

(* Création d'une table des symboles fille *)
(* Le paramètre est la table mère *)
val creerTDSFille : tds -> tds 

(* Ajoute une information dans la table des symboles locale *)
(* tds : la tds courante *)
(* string : le nom de l'identificateur *)
(* info : l'information à associer à l'identificateur *)
(* Si l'identificateur est déjà présent dans TDS, l'information est écrasée *)
(* retour : unit *)
val ajouter : tds -> string -> info_ast -> unit 

(* Recherche les informations d'un identificateur dans la tds locale *)
(* Ne cherche que dans la tds de plus bas niveau *)
val chercherLocalement : tds -> string -> info_ast option 

(* Recherche les informations d'un identificateur dans la tds globale *)
(* Si l'identificateur n'est pas présent dans la tds de plus bas niveau *)
(* la recherche est effectuée dans sa table mère et ainsi de suite *)
(* jusqu'à trouver (ou pas) l'identificateur *)
val chercherGlobalement : tds -> string -> info_ast option 

(* Affiche la tds locale *)
val afficher_locale : tds -> unit 

(* Affiche la tds locale et récursivement *)
val afficher_globale : tds -> unit 

(* get_type : Tds.info_ast -> typ *)
(* Paramètre i : le pointeur en question *)
(* Retourne le type de l'identifiant pointé par le pointeur *)
val get_type : info_ast -> typ

(* get_adresse : Tds.info_ast -> int *)
(* Paramètre i : le pointeur en question *)
(* retourne la valeur du pointeur (l'adresse associé à l'identifiant pointé) *)
val get_adresse : info_ast -> int

(* Créer une information à associer à l'AST à partir d'une info *)
val info_to_info_ast : info -> info_ast

(* Récupère l'information associée à un noeud *)
val info_ast_to_info : info_ast -> info

(* Modifie le type si c'est une InfoVar, ne fait rien sinon *)
val modifier_type_info : typ -> info_ast -> unit

(* Modifie l'emplacement (dépl, registre) si c'est une InfoVar, ne fait rien sinon *)
val modifier_adresse_info : int -> string -> info_ast -> unit
