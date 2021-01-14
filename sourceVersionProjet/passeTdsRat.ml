(* Module de la passe de gestion des identifiants *)
module PasseTdsRat : Passe.Passe with type t1 = Ast.AstSyntax.programme and type t2 = Ast.AstTds.programme =
struct

  open Tds
  open Exceptions
  open Ast
  open AstTds

  type t1 = Ast.AstSyntax.programme
  type t2 = Ast.AstTds.programme

(* analyse_tds_affectable : AstSyntax.affectable -> AstTds.affectable *)
(* Paramètres : - tds : la table des symboles courante *)
(*              - a : l'affectable à analyser *)
(*              - typeAccess : chaine de caractère ("ecriture" ou "lecture")*)
(* Vérifie la bonne utilisation des identifiants et transforme l'affectable
   en un affectable de type AstTds.affectable *)
(* Erreuir si mauvaise utilisation d'un identifiant ou utilisation d'un
   identifiant non déclaré*)
let rec analyse_tds_affectable tds a typeAcces=
  match a with
    | AstSyntax.Valeur v -> let nv = (analyse_tds_affectable tds v typeAcces) 
      in Valeur(nv)
    | AstSyntax.Ident id -> 
      begin
        match chercherGlobalement tds id with
            | None -> (raise (IdentifiantNonDeclare id))
            | Some info -> 
              begin
                match (info_ast_to_info info) with
                | InfoVar _ -> Ident(info)
                | InfoConst(_, _) -> if (typeAcces == "lecture") then
                    Ident(info) else raise (MauvaiseUtilisationIdentifiant id)
                | _ -> raise (MauvaiseUtilisationIdentifiant id)
              end
      end
    

(* analyse_tds_expression : AstSyntax.expression -> AstTds.expression *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre e : l'expression à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'expression
en une expression de type AstTds.expression *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_expression tabFun tds e = 
  match e with
    | AstSyntax.True -> True
    | AstSyntax.False -> False
    | AstSyntax.Entier n -> Entier n
    | AstSyntax.Binaire (b, e1, e2) -> 
      let ne1 = analyse_tds_expression tabFun tds e1 in
      let ne2 = analyse_tds_expression tabFun tds e2 in
      Binaire(b, ne1, ne2)
    | AstSyntax.Rationnel(e1, e2) ->
      let ne1 = analyse_tds_expression tabFun tds e1 in
      let ne2 = analyse_tds_expression tabFun tds e2 in
      Rationnel(ne1, ne2)
    | AstSyntax.Numerateur e ->
      let ne = analyse_tds_expression tabFun tds e in
      Numerateur ne
    | AstSyntax.Denominateur e ->
      let ne = analyse_tds_expression tabFun tds e in
      Denominateur ne
    (*| AstSyntax.Ident id ->
      begin
        match chercherGlobalement tds id with
          | None -> raise (IdentifiantNonDeclare id)
          | Some infoAstId ->
            match (info_ast_to_info infoAstId) with
              | InfoVar _ -> Ident(infoAstId)
              | InfoConst(_,e) -> Entier(e)
              | _ -> raise (MauvaiseUtilisationIdentifiant id)
      end*)
    | AstSyntax.AppelFonction (id, le) ->
    begin
      let l = Hashtbl.find_all tabFun id in
      match l with
        | [] -> 
          begin 
            match chercherGlobalement tds id with 
            | None -> (raise (IdentifiantNonDeclare id))
            | Some _ -> raise (MauvaiseUtilisationIdentifiant id)
          end 
        | _ -> AstTds.AppelFonction(l, List.map (analyse_tds_expression tabFun tds) le) 
    end
    | AstSyntax.Acces v -> AstTds.Acces(analyse_tds_affectable tds v "lecture")
    | AstSyntax.Null -> Null
    | AstSyntax.New t -> New t
    | AstSyntax.Adresse adr -> begin
      match (chercherGlobalement tds adr) with
        | None -> (raise (AdresseNonAssociee adr))
        | Some info_ast_adr -> 
          begin
            match ( info_ast_to_info info_ast_adr) with
              | InfoVar(_,_,_,_) -> Adresse info_ast_adr
              | InfoConst(_,_) -> Adresse info_ast_adr
              | InfoFun(id,_,_)  -> raise (MauvaiseUtilisationIdentifiant id)
          end
    end
      



(* analyse_tds_instruction : AstSyntax.instruction -> tds -> AstTds.instruction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre i : l'instruction à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'instruction
en une instruction de type AstTds.instruction *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_instruction tabFun tds i =
  match i with
  | AstSyntax.Declaration (t, n, e) ->
      begin
        match chercherLocalement tds n with
        | None ->
            (* L'identifiant n'est pas trouvé dans la tds locale, 
            il n'a donc pas été déclaré dans le bloc courant *)
            (* Vérification de la bonne utilisation des identifiants dans l'expression *)
            (* et obtention de l'expression transformée *) 
            let ne = analyse_tds_expression tabFun tds e in
            (* Création de l'information associée à l'identfiant *)
            let info = InfoVar (n,Undefined, 0, "") in
            (* Création du pointeur sur l'information *)
            let ia = info_to_info_ast info in
            (* Ajout de l'information (pointeur) dans la tds *)
            ajouter tds n ia;
            (* Renvoie de la nouvelle déclaration où le nom a été remplacé par l'information 
            et l'expression remplacée par l'expression issue de l'analyse *)
            Declaration (t, ne, ia) 
        | Some _ ->
            (* L'identifiant est trouvé dans la tds locale, 
            il a donc déjà été déclaré dans le bloc courant *) 
            raise (DoubleDeclaration n)
      end
  | AstSyntax.Affectation (a,e) ->
    begin 
      let na = analyse_tds_affectable tds a "ecriture" in 
      let ne = analyse_tds_expression tabFun tds e in
      Affectation(na, ne)
    end
  | AstSyntax.Constante (n,v) -> 
      begin
        match chercherLocalement tds n with
        | None -> 
        (* L'identifiant n'est pas trouvé dans la tds locale, 
        il n'a donc pas été déclaré dans le bloc courant *)
        (* Ajout dans la tds de la constante *)
        ajouter tds n (info_to_info_ast (InfoConst (n,v))); 
        (* Suppression du noeud de déclaration des constantes devenu inutile *)
        Empty
        | Some _ ->
          (* L'identifiant est trouvé dans la tds locale, 
          il a donc déjà été déclaré dans le bloc courant *) 
          raise (DoubleDeclaration n)
      end
  | AstSyntax.Affichage e -> 
      (* Vérification de la bonne utilisation des identifiants dans l'expression *)
      (* et obtention de l'expression transformée *)
      let ne = analyse_tds_expression tabFun tds e in
      (* Renvoie du nouvel affichage où l'expression remplacée par l'expression issue de l'analyse *)
      Affichage (ne)
  | AstSyntax.Conditionnelle (c,t,e) -> 
      (* Analyse de la condition *)
      let nc = analyse_tds_expression tabFun tds c in
      (* Analyse du bloc then *)
      let tast = analyse_tds_bloc tabFun tds t in
      (* Analyse du bloc else *)
      let east = analyse_tds_bloc tabFun tds e in
      (* Renvoie la nouvelle structure de la conditionnelle *)
      Conditionnelle (nc, tast, east)
  | AstSyntax.TantQue (c,b) -> 
      (* Analyse de la condition *)
      let nc = analyse_tds_expression tabFun tds c in
      (* Analyse du bloc *)
      let bast = analyse_tds_bloc tabFun tds b in
      (* Renvoie la nouvelle structure de la boucle *)
      TantQue (nc, bast)

      
(* analyse_tds_bloc : AstSyntax.bloc -> AstTds.bloc *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre li : liste d'instructions à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le bloc
en un bloc de type AstTds.bloc *)
(* Erreur si mauvaise utilisation des identifiants *)
and analyse_tds_bloc tabFun tds li =
  (* Entrée dans un nouveau bloc, donc création d'une nouvelle tds locale 
  pointant sur la table du bloc parent *)
  let tdsbloc = creerTDSFille tds in
  (* Analyse des instructions du bloc avec la tds du nouveau bloc 
  Cette tds est modifiée par effet de bord *)
   let nli = List.map (analyse_tds_instruction tabFun tdsbloc) li in
   (* afficher_locale tdsbloc ; *) (* décommenter pour afficher la table locale *)
   nli



(* analyser_tds_param : tds * expression list -> (typ * info_ast)list *)
(* Paramètre tds : la table des symboles locale *)
(* Paramètre : la liste des paramètres à analyser *)
(* Analyse les paramètres en entrée d'une fonction *)
(* Erreur si double declaration d'un même paramètre *)
let analyser_tds_param (tds,lp) = 
  List.fold_right (fun (t,np) tq -> match (chercherLocalement tds np) with
    | None -> (let info_p = Tds.InfoVar (np,t,0,"") in
      let info =  Tds.info_to_info_ast info_p in
      Tds.ajouter tds np info;
      (t,info)::tq)
    | Some _ -> (raise (DoubleDeclaration np))) lp []



(* analyse_tds_fonction : AstSyntax.fonction -> AstTds.fonction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre : la fonction à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme la fonction
en une fonction de type AstTds.fonction *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyse_tds_fonction maintds tabFun (AstSyntax.Fonction(t,n,lp,li,e))  = 
  match chercherGlobalement maintds n with
    | Some info -> 
      match info_ast_to_info info with
        | InfoFun(_, _, tp) ->
          begin
            if ((List.length tp) = (List.length lp)) then 
              let tlp = (List.map (fst) lp) in 
              if (not ( Type.est_compatible_list tp tlp)) then
                begin
                  let tp = List.map (fst) lp in
                  let info_f = Tds.InfoFun (n,t,tp) in
                  let info_af = (Tds.info_to_info_ast info_f) in
                  let tds = creerTDSFille maintds in
                  ajouter tds n info_af;
                  ajouter_fonc tabFun n (info_to_info_ast (Tds.InfoFun (n,t,tp)));
                  let infop = analyser_tds_param (tds,lp) in
                  let li_anal = List.map (analyse_tds_instruction tabFun tds) li in
                  let r = analyse_tds_expression tabFun tds e in
                  ajouter maintds n (info_to_info_ast (Tds.InfoFun (n,t,tp)));
                  AstTds.Fonction(t,info_af,infop,li_anal,r)
                end
              else (raise (DoubleDeclaration n))
            else (raise (DoubleDeclaration n))
          end
        | _ -> failwith "variable erronée"
    | _ -> 
      begin
        let tp = List.map (fst) lp in
        let info_f = Tds.InfoFun (n,t,tp) in
        let info_af = (Tds.info_to_info_ast info_f) in
        let tds = creerTDSFille maintds in
        ajouter tds n (info_to_info_ast (Tds.InfoFun (n,t,tp)));
        let ip = analyser_tds_param (tds,lp) in
        let li_anal = List.map (analyse_tds_instruction tabFun tds) li in
        let r = analyse_tds_expression tabFun tds e in
        ajouter maintds n (info_to_info_ast (Tds.InfoFun (n,t,tp)));
        AstTds.Fonction(t,info_af,ip,li_anal,r)          
      end
  

(* analyser : AstSyntax.ast -> AstTds.ast *)
(* Paramètre : le programme à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le programme
en un programme de type AstTds.ast *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyser (AstSyntax.Programme (fonctions,prog)) =
  let tds = creerTDSMere () in
  let tabFun = creerTableFonc () in
  let nf = List.map (analyse_tds_fonction tds tabFun) fonctions in
  let nb = analyse_tds_bloc tabFun tds prog in
  Programme (nf,nb)

end
