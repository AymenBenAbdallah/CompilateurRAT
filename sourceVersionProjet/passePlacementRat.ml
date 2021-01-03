(* Module de la passe de Typage *)
module PassePlacementRat : Passe.Passe with type t1 = Ast.AstType.programme and type t2 = Ast.AstPlacement.programme =
struct

  open Type
  open Tds
  open Ast
  open AstPlacement
  open Exceptions

  type t1 = Ast.AstType.programme
  type t2 = Ast.AstPlacement.programme


(* analyse_placement_instruction : AstType.instruction -> String -> Int -> Int *)
(* Paramètre i : l'instruction à analyser *)
(* Paramètre reg : le registre mémoire en question *)
(* Paramètre base : position courante dans le registre mémoire *)
(* Alloque la mémoire nécessaire pour le bon fonctionnement des instructions*)
(* Erreur si variable introuvable *)
let rec analyse_placement_instruction i reg base =
  match i with
  | AstType.Declaration (_, info) ->
    begin
      match info_ast_to_info info with
        | InfoVar(_, t, _, _) -> 
          let _ = modifier_adresse_info base reg info in
          getTaille t
        | _ -> raise VariableIntrouvable
    end
  | AstType.Conditionnelle (_,t,e) -> 
  begin
    let _ = (analyse_placement_bloc t reg base) in
    let _ = (analyse_placement_bloc e reg base) in
    0
  end

  | AstType.TantQue (_,b) -> 
  begin
    let _ = (analyse_placement_bloc b reg base) in
    0
  end
  | _ -> 0


(* analyse_placement_bloc : AstType.bloc -> String -> Int -> Int List *)
(* Paramètre li : liste d'instructions à analyser *)
(* Paramètre reg : le registre mémoire en question *)
(* Paramètre base : position courante dans le registre mémoire *)
(* Alloque la mémoire nécessaire pour le bon fonctionnement des instructions*)
and analyse_placement_bloc li reg base =
  List.fold_left (fun tq q ->  
    let ti=(analyse_placement_instruction q reg (base+tq)) in
    tq+ti) 0 li


(* analyse_placement_parametre : info_ast -> String -> Int -> Int *)
(* Paramètre infoP : le parametre à traiter de la fonction *)
(* Paramètre reg : le registre mémoire en question *)
(* Paramètre base : position courante dans le registre mémoire *)
(* Alloque la mémoire nécessaire pour le parametre donné *)
(* Erreur si la nature du paramètre n'est pas reconnue (pas une variable) *)
let analyse_placement_parametre infoP reg base =
  match (info_ast_to_info infoP) with
    | InfoVar(_, t, _, _) -> 
      begin
        let _ = modifier_adresse_info (base-(getTaille t)) reg infoP in
        getTaille t
      end
    | _ -> raise NatureParamInattendue

(* analyse_placement_parametre : info_ast List -> String -> Int -> Int List *)
(* Paramètre infoP : les paramètres à traiter de la fonction *)
(* Paramètre reg : le registre mémoire en question *)
(* Paramètre base : position courante dans le registre mémoire *)
(* Alloque la mémoire nécessaire pour les parametre donnés *)
let analyse_placement_parametres infoP reg base = 
  List.fold_right (fun t tq -> let plt=(analyse_placement_parametre t reg (base-tq)) in plt+tq) infoP 0

(* analyse_Placement_fonction : AstType.fonction -> AstPlacement.fonction *)
(* Paramètre : la fonction à analyser *)
(* Alloque la mémoire nécessaire pour la fonction donnée *)
let analyse_placement_fonction (AstType.Fonction(info_ast, infoP, li, r))  = 
  begin
    let _ = (analyse_placement_bloc li "LB" 3) in
    let _ = (analyse_placement_parametres infoP "LB" 0) in
    AstPlacement.Fonction(info_ast,infoP,li,r)
  end



(* analyser : AstType.ast -> AstPlacement.ast *)
(* Paramètre : le programme à analyser *)
(* Alloque la mémoire nécessaire pour le programme *)
let analyser (AstType.Programme (fonctions, prog)) =
  let analyseF = List.map (analyse_placement_fonction) fonctions in
  let _ = analyse_placement_bloc prog "SB" 0 in
  Programme (analyseF, prog)

end
