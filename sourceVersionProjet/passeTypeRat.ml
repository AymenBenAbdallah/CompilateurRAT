
(* Module de la passe de Typage *)
module PasseTypeRat : Passe.Passe with type t1 = Ast.AstTds.programme and type t2 = Ast.AstType.programme =
struct

  open Type
  open Tds
  open Exceptions
  open Ast
  open AstType

  type t1 = Ast.AstTds.programme
  type t2 = Ast.AstType.programme

let rec get_type_affectable a = 
  match a with
    | AstTds.Ident(info) -> get_type info
    | AstTds.Valeur(aff) -> Pointeur(get_type_affectable aff)

(*let rec chercher_fonction_compatible liste_fonctions types e = 
  match liste_fonctions with 
  | [] -> raise (FonctionIncompatible)
  | t::q ->  let i= (info_ast_to_info t) in
    match i with
      | (InfoFun(_,tr,tp)) -> 
          begin
            if (est_compatible_list types tp) then
              t
            else 
              (chercher_fonction_compatible q types e)
          end 
      | _ -> failwith "Impossible"*)

(* analyse_type_expression : AstTds.expression -> AstType.expression *)
(* Paramètre e : l'expression à analyser *)
(* Vérifie la bonne cohérence des types utilisés et tranforme l'expression
en une expression de type AstType.expression *)
(* Erreur si cohérence entre les types utilisés *)
let rec analyse_type_expression e = 
  match e with
    | AstTds.True -> (Type.Bool, AstType.True)
    | AstTds.False -> (Type.Bool, False)
    | AstTds.Entier n -> (Type.Int, Entier n)
    | AstTds.Binaire (b, e1, e2) -> 
    begin
      let (te1, ne1) = analyse_type_expression e1 in
      let (te2, ne2) = analyse_type_expression e2 in
      match b with
        | Plus -> 
          begin
            if ((Type.est_compatible te1 Type.Int) && (Type.est_compatible te2 Type.Int)) then 
              (Type.Int, Binaire (PlusInt, ne1, ne2))
            else if ((Type.est_compatible te1 Type.Rat) && (Type.est_compatible te2 Type.Rat)) then
              (Type.Rat, Binaire (PlusRat, ne1, ne2))
            else
              raise (TypeBinaireInattendu(b, te1, te2))
          end
        | Mult -> 
          begin
            if ((Type.est_compatible te1 Type.Int) && ((Type.est_compatible te2 Type.Int))) then
              (Type.Int, Binaire (MultInt, ne1, ne2))
            else if ((Type.est_compatible te1 Type.Rat) && ((Type.est_compatible te2 Type.Rat))) then
              (Type.Rat, Binaire (MultRat, ne1, ne2))
            else 
              raise (TypeBinaireInattendu(b,te1,te2))
          end
        | Equ -> 
          begin
            if ((Type.est_compatible te1 Type.Int) && ((Type.est_compatible te2 Type.Int))) then
              (Type.Bool, Binaire (EquInt, ne1,ne2))
            else if ((Type.est_compatible te1 Type.Bool) && ((Type.est_compatible te2 Type.Bool))) then
              (Type.Bool, Binaire (EquBool,ne1,ne2))
            else 
              raise (TypeBinaireInattendu (b,te1,te2))
          end
        | Inf -> 
          begin
              if ((Type.est_compatible te1 Type.Int) && ((Type.est_compatible te2 Type.Int))) then
                (Type.Bool, Binaire (Inf,ne1,ne2))
              else 
                raise (TypeBinaireInattendu (b,te1,te2))
          end
      end
    | AstTds.Rationnel(e1, e2) ->
      begin
        let (te1,ne1) = analyse_type_expression e1 in
        let (te2,ne2) = analyse_type_expression e2 in
        match ((est_compatible te1 Type.Int),(est_compatible te2 Type.Int)) with
          | (true,true) -> (Type.Rat, Rationnel (ne1,ne2))
          | (true,false) -> raise (TypeInattendu (te2, Type.Int))
          | (false,true) -> raise (TypeInattendu (te1, Type.Int))
          | (false,false) -> raise (TypesParametresInattendus([te1; te2], [Type.Int; Type.Int]))
      end
    | AstTds.Numerateur e ->
      begin
        let (te,ne)=(analyse_type_expression e) in
        if (est_compatible te Type.Rat) then 
          (Type.Int, Numerateur ne)
        else 
          raise (TypeInattendu (te, Type.Rat))
      end
    | AstTds.Denominateur e ->
      begin
        let (te,ne)=(analyse_type_expression e) in
        if (est_compatible te Type.Rat) then 
          (Type.Int, Denominateur ne)
        else 
          raise (TypeInattendu (te, Type.Rat))
      end
    (*| AstTds.Ident info_ast ->
      begin 
        let info = info_ast_to_info info_ast in
        match info with 
          |InfoVar(_,t,_,_) -> (t, Ident(info_ast))
          |InfoFun _ -> raise (MauvaiseUtilisationIdentifiant "")
          |InfoConst (_,n) -> (Int, Entier n)
      end*)
    | AstTds.AppelFonction (info_ast, le) ->
      (*begin
        let i = (info_ast_to_info info_ast) in
        match i with
          | (InfoFun(_,tRetour,tParam)) -> 
            begin
              let nle = List.map (analyse_type_expression) le in
              let (types, nlexp)=((List.map (fst) nle),(List.map (snd) nle)) in
              if (est_compatible_list types tParam) then 
                (tRetour, AppelFonction(info_ast, nlexp))
              else raise (TypesParametresInattendus (tParam,types))
            end
          | _ -> failwith "Cas Impossible"
          
      end*)
        (Int, Null)
    | AstTds.Acces(a) -> (get_type_affectable a, Acces(a))
    | AstTds.Null -> (Pointeur(Undefined), Null)
    | AstTds.New(t) -> (Pointeur(t), New(t))
    | AstTds.Adresse(i) -> (Int, Adresse(i))

(* analyse_type_instruction : AstTds.instruction -> tds -> AstType.instruction *)
(* Paramètre i : l'instruction à analyser *)
(* Vérifie la cohérence des types et tranforme l'instruction
en une instruction de type AstTds.instruction *)
(* Erreur si non cohérence des types *)
let rec analyse_type_instruction i =
  match i with
  | AstTds.Declaration (t, e, info_ast) ->
    begin
      let (te, ne) = analyse_type_expression e in
      if (est_compatible te t) then
        begin
          let _ = modifier_type_info te info_ast in
          AstType.Declaration(ne,info_ast)
        end
      else
        raise (TypeInattendu(te,t))
    end
  | AstTds.Affectation (aff, e) ->
    begin
      let (te,ne) = analyse_type_expression e in
      let t = get_type_affectable aff in 
      if (est_compatible te t) then
        Affectation (ne, aff)
      else
        raise (TypeInattendu (te, t))
    end        
  | AstTds.Affichage e -> 
    begin
      let (te,ne)=(analyse_type_expression e) in
        begin
          if (est_compatible te Type.Int) then 
            (AffichageInt ne)
          else if (est_compatible te Type.Rat) then 
            (AffichageRat ne)
          else 
            (AffichageBool ne)
        end
    end
  | AstTds.Conditionnelle (c,t,e) ->
    begin
      let (tc,nc) = analyse_type_expression c in
      if (est_compatible tc Type.Bool) then 
        let analyse_then = analyse_type_bloc t in
        let analyse_else = analyse_type_bloc e in
        Conditionnelle(nc,analyse_then,analyse_else)
      else 
        raise (TypeInattendu(tc,Type.Bool))
    end
  | AstTds.TantQue (c,b) -> 
    begin
      let (tc,nc) = analyse_type_expression c in
      if (est_compatible tc Type.Bool) then 
        let analyse_bloc = analyse_type_bloc b in
        AstType.TantQue(nc, analyse_bloc)
      else raise (TypeInattendu(tc, Type.Bool))
    end
  | AstTds.Empty -> Empty


(* analyse_type_bloc : AstTds.bloc -> AstType.bloc *)
(* Paramètre li : liste d'instructions à analyser *)
(* Vérifie la cohérence des types et tranforme le bloc
en un bloc de type AstType.bloc *)
(* Erreur si non cohérence des types *)
and analyse_type_bloc li =
  List.map analyse_type_instruction li
   

(* analyse_type_fonction : AstTds.fonction -> AstType.fonction *)
(* Paramètre : la fonction à analyser *)
(* Vérifie la cohérence des types et tranforme la fonction
en une fonction de type AstType.fonction *)
(* Erreur si non cohérence des types *)
let analyse_type_fonction (AstTds.Fonction(t,info_ast,info_param,li,retour))  = 
  let ali = analyse_type_bloc li in
  let (tr, nr) = analyse_type_expression retour in
  let infoP = List.map (snd) info_param in
  if (est_compatible tr t) then
    Fonction(info_ast, infoP, ali, nr)
  else
    raise (TypeInattendu(tr,t))



(* analyser : AstTds.ast -> AstType.ast *)
(* Paramètre : le programme à analyser *)
(* Vérifie la cohérence des types et tranforme le programme
en un programme de type AstType.ast *)
(* Erreur si non cohérence des types *)
let analyser (AstTds.Programme (fonctions, prog)) =
  let analyseF = List.map (analyse_type_fonction) fonctions in
  let analyseB = analyse_type_bloc prog in
  Programme (analyseF, analyseB)

end
