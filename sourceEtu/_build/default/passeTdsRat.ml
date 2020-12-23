(* Module de la passe de gestion des identifiants *)
module PasseTdsRat : Passe.Passe with type t1 = Ast.AstSyntax.programme and type t2 = Ast.AstTds.programme =
struct

  open Tds
  open Exceptions
  open Ast
  open AstTds

  type t1 = Ast.AstSyntax.programme
  type t2 = Ast.AstTds.programme


(* analyse_tds_expression : AstSyntax.expression -> AstTds.expression *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre e : l'expression à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'expression
en une expression de type AstTds.expression *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_expression tds e = 
  match e with
    | AstSyntax.True -> True
    | AstSyntax.False -> False
    | AstSyntax.Entier n -> Entier n
    | AstSyntax.Binaire (b, e1, e2) -> 
      let ne1 = analyse_tds_expression tds e1 in
      let ne2 = analyse_tds_expression tds e2 in
      Binaire(b, ne1, ne2)
    | AstSyntax.Rationnel(e1, e2) ->
      let ne1 = analyse_tds_expression tds e1 in
      let ne2 = analyse_tds_expression tds e2 in
      Rationnel(ne1, ne2)
    | AstSyntax.Numerateur e ->
      let ne = analyse_tds_expression tds e in
      Numerateur ne
    | AstSyntax.Denominateur e ->
      let ne = analyse_tds_expression tds e in
      Denominateur ne
    | AstSyntax.Ident id ->
      begin
        match chercherGlobalement tds id with
          | None -> raise (IdentifiantNonDeclare id)
          | Some infoAstId ->
            match (info_ast_to_info infoAstId) with
              | InfoFun (id, _, _) -> raise (MauvaiseUtilisationIdentifiant id)
              | _ -> Ident infoAstId
      end
    | AstSyntax.AppelFonction (id, le) -> 
      match (chercherGlobalement tds id) with
        | None -> raise (IdentifiantNonDeclare id)
        | Some infoAstId -> 
          match (info_ast_to_info infoAstId) with
            | InfoFun (id, _, _) -> AppelFonction(infoAstId, List.map (analyse_tds_expression tds) le)
            | _ -> raise (MauvaiseUtilisationIdentifiant id)



(* analyse_tds_instruction : AstSyntax.instruction -> tds -> AstTds.instruction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre i : l'instruction à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'instruction
en une instruction de type AstTds.instruction *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_instruction tds i =
  match i with
  | AstSyntax.Declaration (t, n, e) ->
      begin
        match chercherLocalement tds n with
        | None ->
            (* L'identifiant n'est pas trouvé dans la tds locale, 
            il n'a donc pas été déclaré dans le bloc courant *)
            (* Vérification de la bonne utilisation des identifiants dans l'expression *)
            (* et obtention de l'expression transformée *) 
            let ne = analyse_tds_expression tds e in
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
  | AstSyntax.Affectation (n,e) ->
      begin
        match chercherGlobalement tds n with
        | None -> 
          (* L'identifiant n'est pas trouvé dans la tds globale. *) 
          raise (IdentifiantNonDeclare n)
        | Some info -> 
          (* L'identifiant est trouvé dans la tds globale, 
          il a donc déjà été déclaré. L'information associée est récupérée. *) 
          begin
            match info_ast_to_info info with
            | InfoVar _ -> 
              (* Vérification de la bonne utilisation des identifiants dans l'expression *)
              (* et obtention de l'expression transformée *) 
              let ne = analyse_tds_expression tds e in
              (* Renvoie de la nouvelle affectation où le nom a été remplacé par l'information 
              et l'expression remplacée par l'expression issue de l'analyse *)
               Affectation (ne, info)
            |  _ ->
              (* Modification d'une constante ou d'une fonction *)  
              raise (MauvaiseUtilisationIdentifiant n) 
          end
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
      let ne = analyse_tds_expression tds e in
      (* Renvoie du nouvel affichage où l'expression remplacée par l'expression issue de l'analyse *)
      Affichage (ne)
  | AstSyntax.Conditionnelle (c,t,e) -> 
      (* Analyse de la condition *)
      let nc = analyse_tds_expression tds c in
      (* Analyse du bloc then *)
      let tast = analyse_tds_bloc tds t in
      (* Analyse du bloc else *)
      let east = analyse_tds_bloc tds e in
      (* Renvoie la nouvelle structure de la conditionnelle *)
      Conditionnelle (nc, tast, east)
  | AstSyntax.TantQue (c,b) -> 
      (* Analyse de la condition *)
      let nc = analyse_tds_expression tds c in
      (* Analyse du bloc *)
      let bast = analyse_tds_bloc tds b in
      (* Renvoie la nouvelle structure de la boucle *)
      TantQue (nc, bast)

      
(* analyse_tds_bloc : AstSyntax.bloc -> AstTds.bloc *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre li : liste d'instructions à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le bloc
en un bloc de type AstTds.bloc *)
(* Erreur si mauvaise utilisation des identifiants *)
and analyse_tds_bloc tds li =
  (* Entrée dans un nouveau bloc, donc création d'une nouvelle tds locale 
  pointant sur la table du bloc parent *)
  let tdsbloc = creerTDSFille tds in
  (* Analyse des instructions du bloc avec la tds du nouveau bloc 
  Cette tds est modifiée par effet de bord *)
   let nli = List.map (analyse_tds_instruction tdsbloc) li in
   (* afficher_locale tdsbloc ; *) (* décommenter pour afficher la table locale *)
   nli


(* analyser_tds_param : tds * expression list -> (typ * info_ast)list *)
(* Paramètre tds : la table des symboles locale *)
(* Paramètre : la liste des paramètres à analyser *)
(* Analyse les paramètres en entrée d'une fonction *)
(* Erreur si double declaration d'un même paramètre *)
let analyser_tds_param (tdsLocale,liste_param) = 
  List.fold_right (fun (typp,nom_param) tq -> match (chercherLocalement tdsLocale nom_param) with
                                              | None -> (let info_p = Tds.InfoVar (nom_param,typp,0,"") in
                                                        let info_ast_p =  Tds.info_to_info_ast info_p in
                                                        Tds.ajouter tdsLocale nom_param info_ast_p;
                                                        (typp,info_ast_p)::tq)
                                              | Some _ -> (raise (DoubleDeclaration nom_param))) liste_param []



(* analyse_tds_fonction : AstSyntax.fonction -> AstTds.fonction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre : la fonction à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme la fonction
en une fonction de type AstTds.fonction *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyse_tds_fonction maintds (AstSyntax.Fonction(t,n,lp,li,e))  = 
  match chercherGlobalement maintds n with
    | Some _ -> (raise (DoubleDeclaration n))
    | None -> begin
                let types_param = List.map (fst) lp in
                let infof = Tds.InfoFun (n,t,types_param) in
                let info_ast_f = (Tds.info_to_info_ast infof) in
                let tdsLocale = creerTDSFille maintds in
                ajouter tdsLocale n (info_to_info_ast (Tds.InfoFun (n,t,types_param)));
                let info_param = analyser_tds_param (tdsLocale,lp) in
                let linstru_analysees = List.map (analyse_tds_instruction tdsLocale) li in
                let retour_analyse = analyse_tds_expression tdsLocale e in
                ajouter maintds n (info_to_info_ast (Tds.InfoFun (n,t,types_param)));
                AstTds.Fonction(t,info_ast_f,info_param,linstru_analysees,retour_analyse)
                
    end

  

(* analyser : AstSyntax.ast -> AstTds.ast *)
(* Paramètre : le programme à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le programme
en un programme de type AstTds.ast *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyser (AstSyntax.Programme (fonctions,prog)) =
  let tds = creerTDSMere () in
  (* let nf = List.map (analyse_tds_fonction tds) fonctions in *)
  let nf = [] in
  let nb = analyse_tds_bloc tds prog in
  Programme (nf,nb)

end
