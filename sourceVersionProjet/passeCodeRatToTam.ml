(*(* Module de la passe de Typage *)
module PasseCodeRatToTam : Passe.Passe with type t1 = Ast.AstPlacement.programme and type t2 = string =
struct

  open Type
  open Tds
  open Ast
  open Exceptions
  open AstType
  open Code

  type t1 = Ast.AstPlacement.programme
  type t2 = string


(* analyse_code_expression : AstType.expression -> String *)
(* Paramètre e : l'expression à analyser *)
(* Génère le code Tam de l'expression donnée *)
(* Erreur si non cohérence des variables *)
let rec analyse_code_expression e =
  match e with
    | True -> "LOADL 1\n"
    | False -> "LOADL 0\n"
    | AstType.Entier n ->
      "LOADL " ^ (string_of_int n) ^ "\n"
    | AstType.Binaire(b, e1, e2) ->
      begin
        let ne1 = analyse_code_expression e1 in
        let ne2 = analyse_code_expression e2 in 
        match b with
        | PlusInt -> ne1 ^ ne2 ^ "SUBR IAdd \n"
        | PlusRat -> ne1 ^ ne2 ^ "CALL (SB) RAdd \n"
        | MultInt -> ne1 ^ ne2 ^ "SUBR IMul \n"
        | MultRat -> ne1 ^ ne2 ^ "CALL (SB) RMul \n"
        | EquInt ->  ne1 ^ ne2 ^ "SUBR IEq \n"
        | EquBool -> ne1 ^ ne2 ^ "SUBR BEqu \n"
        | Inf ->  ne1 ^ ne2 ^ "SUBR ILss \n"
      end
    | AstType.Rationnel(e1, e2) ->
      begin
        let ne1 = analyse_code_expression e1 in
        let ne2 = analyse_code_expression e2 in 
        ne1 ^ ne2
      end
    | AstType.Numerateur e ->
      (analyse_code_expression e) ^ "POP (0) 1\n"
    | AstType.Denominateur e ->
      (analyse_code_expression e) ^ "POP (1) 1\n"
    | AstType.Ident info_ast ->
      begin
        match (info_ast_to_info info_ast) with
          | InfoConst (_, n) ->
            "LOADL "^(string_of_int n)
          | InfoVar (_, t, base, reg) ->
            "LOAD (" ^ (string_of_int (getTaille t)) ^ ") " ^ (string_of_int base) ^ "[" ^ reg ^ "]\n"
          | _ -> raise VariableIntrouvable
      end
    | AstType.AppelFonction(info, e) ->
      begin
        match (info_ast_to_info info) with
          | InfoFun(n,_,_) ->
                (List.fold_left (fun tt q -> tt ^ (analyse_code_expression q)) "" e ) ^ "CALL (SB) " ^ n ^ "\n" 
          | _ -> raise FonctionIntrouvable
          end

(* analyse_code_instruction : AstType.instruction -> String *)
(* Paramètre e : l'instruction à analyser *)
(* Génère le code Tam de l'instruction donnée *)
(* Erreur si non cohérence des variables *)
let rec analyse_code_instruction i =
  match i with
  | AstType.Declaration (e, info) ->
    begin
      match (info_ast_to_info info) with
        | InfoVar (_,t,base,reg) -> 
          let ne = (analyse_code_expression e) in
          "PUSH " ^ (string_of_int (getTaille t)) ^ "\n" ^
          ne ^ 
          "STORE (" ^ (string_of_int (getTaille t)) ^ ") " ^ (string_of_int base) ^ "[" ^ reg ^ "]\n"
        | _ -> raise VariableIntrouvable
    end
  | AstType.Affectation (e,info) -> begin
    match (info_ast_to_info info) with
      | InfoVar (_,t,base,reg) -> 
        let ne = analyse_code_expression e in
        ne ^
        "STORE (" ^ (string_of_int (getTaille t)) ^
        ") " ^ (string_of_int base) ^ "[" ^ reg ^ "]\n" 
      | _ -> raise VariableIntrouvable
  end
  | AstType.AffichageInt e ->
    let ne = analyse_code_expression e in
    ne ^ "SUBR IOut\n"
  | AstType.AffichageRat e -> 
    let ne = analyse_code_expression e in
    ne ^ "CALL (SB) ROut\n"
  | AstType.AffichageBool e -> 
    let ne = analyse_code_expression e in
    ne ^ "SUBR BOut\n"
  | AstType.Conditionnelle (c,t,e) ->
    begin
      let nc = analyse_code_expression c in 
      let nt = analyse_code_bloc t in
      let ne = analyse_code_bloc e in
      let labelElse = Code.getEtiquette() in
      let labelFinElse = Code.getEtiquette() in
      nc ^ "JUMPIF (0) " ^ labelElse ^ "\n" ^
      nt ^ "\nJUMP " ^ labelFinElse ^ "\n" ^ 
      labelElse ^ "\n" ^
      ne ^ labelFinElse ^ "\n"
    end
  | AstType.TantQue (c,b) -> 
    begin
      let nc = (analyse_code_expression c) in 
      let labelWhile = Code.getEtiquette() in
      let labelEndWhile = Code.getEtiquette() in
      labelWhile ^ "\n" ^ 
      nc ^ "\nJUMPIF (0) " ^ labelEndWhile ^ "\n" ^
      (analyse_code_bloc b) ^ "\nJUMP " ^ labelWhile ^ "\n" ^ 
      labelEndWhile ^ "\n"
    end
  | AstType.Empty -> ""


(* analyse_code_bloc : AstType.bloc -> String *)
(* Paramètre li : le bloc (ou liste d'instructions) à analyser *)
(* Génère le code Tam de le bloc donné *)
(* Erreur si non cohérence des variables *)
and analyse_code_bloc li =
(List.fold_left (fun tt q -> tt ^ (analyse_code_instruction q)) "" li)

(* analyser : AstType.ast -> string *)
(* Paramètre : le programme à analyser *)
(* Génère le code Tam de le bloc donné *)
(* Erreur si non cohérence des variables *)
let analyse_code_fonction (AstPlacement.Fonction(info_ast, _, li, r)) =
  match (info_ast_to_info info_ast) with
    | InfoFun (n, tr, tp) -> 
      let taille_parametres = (List.fold_right (fun t tq -> (getTaille t)+tq) tp 0) in
      let tailleVarLoc = (List.fold_right (fun t tq -> (AstType.taille_variables_declarees t)+tq) li 0) in
      n ^ "\n" ^
      (analyse_code_bloc li) ^
      (analyse_code_expression r) ^
      "POP (" ^ (string_of_int (getTaille tr)) ^ ") " ^ (string_of_int tailleVarLoc) ^ "\n" ^
      "RETURN (" ^ (string_of_int (getTaille tr)) ^ ") " ^ (string_of_int taille_parametres) ^ "\n"
    | _ -> raise FonctionIntrouvable


(* analyser : AstPlacement.ast -> string *)
(* Paramètre : le programme à analyser *)
(* Génère le code Tam de le bloc donné *)
(* Erreur si non cohérence des variables *)
let analyser (AstPlacement.Programme (fonctions, prog)) =
  let _ =  (List.fold_right (fun t tq -> (AstType.taille_variables_declarees t)+tq) prog 0) in
    (getEntete ()) ^ (List.fold_left (fun tt q -> tt ^ (analyse_code_fonction q)) ""  fonctions) ^ "main\n" ^ (analyse_code_bloc prog) ^ "HALT\n"
    (*"POP (0) " ^ (string_of_int taille_variables) ^ "\n" ^ "HALT\n"*)

end
*)