open Lexer
open Ast

exception ParseError of string

open Lexer

let unexpected_end (_ : unit) : 'a =
  raise (ParseError "Unexpected end of input")

let expect (t : token) : token list -> token list = function
  | [] -> unexpected_end ()
  | t1 :: ts ->
    if t = t1
    then ts
    else raise (ParseError
                  ("Expected " ^ tok_to_str t ^ ", got: " ^ tok_to_str t1))

let lassoc (seps : (token * (expr -> expr -> expr)) list)
(sub_expr : token list -> expr * token list)
(src : token list) : expr * token list =
  let rec helper ex ts = match ts with
  | t :: rest -> (match List.assoc_opt t seps with
  | None -> (ex, ts) 
  | Some f ->
  let (b, r) = sub_expr rest in
  helper (f ex b) r)
  | _ -> (ex, ts) (* This is the end of the input. *) in
  let (a, r) = sub_expr src in
  helper a r

let rec parse_pattern_vars : token list -> pattern_vars * token list = function
  | Id x :: rest -> (PatternVar (x), rest)
  | LParen :: rest -> 
      let rec parse_pattern_vars_list src (acc : string list) = 
        match src with
        | RParen :: rest -> (List.rev acc, rest)
        | Id x :: Comma :: rest -> parse_pattern_vars_list rest (x :: acc) in
      let (xs, rest) = parse_pattern_vars_list rest [] in
      (PatternMultiVars (xs), rest)

and parse_match_branch : token list -> match_branch * token list = function
  | Id x :: rest -> 
    (match rest with 
      (* optional pattern_vars *)
      | DoubleArrow :: rest2 -> 
        let (e, rest3) = parse_expr rest2 in
        (MatchBr (x, None, e), rest3)
      | rest2 -> 
        let (pat_vars, rest3) = parse_pattern_vars rest2 in
        let (e, rest4) = parse_expr rest3 in
        ((MatchBr (x, Some(pat_vars), e), rest4)))  

and parse_type (src : token list) : typ * token list =  
  let rec parse_tuple_ty_list acc src =
    match src with
    | RParen :: r -> (List.rev acc, r)
    | Times :: r ->
      let (e, r2) = parse_type r in
      parse_tuple_ty_list (e :: acc) r2 
    | _ -> raise (ParseError "Expected ), or * in tuple type") in
  
  let rec parse_single_type (src : token list) = match src with
    | TInt :: rest -> (IntTy, rest)
    | TBool :: rest -> (BoolTy, rest)
    | TString :: rest -> (StrTy, rest)
    | TUnit :: rest -> (UnitTy, rest)
    | Id x :: rest -> (UserTy x, rest)
    | LParen :: rest -> 
      let (ty, rest2) = parse_single_type rest in
      (ty, rest2) in

  let parse_extended_type (src : token list) (ty1 : typ) = match src with
    | Times :: r -> 
      let (ls, r2) = parse_tuple_ty_list [ty1] r in
      (TupleTy (ls), expect RParen r2)
    | Arrow :: r ->
      let (ty2, r2) = parse_single_type r in
      (FuncTy (ty1, ty2), expect RParen r2) 
    (* <type> was just a single type, so it's done *)
    | _ -> (ty1, src)
    in

  let (ty1, r) = parse_single_type src in
  (match r with
    | RParen :: r2 -> (ty1, r2)
    | _ -> parse_extended_type r ty1)


and parse_unop : token list -> unop * token list = function
  | Not :: rest -> (NotComp, rest)
  | Negate :: rest -> (Neg, rest)

and parse_binop : token list -> binop * token list = function
  | Plus :: rest -> (Add, rest)
  | Minus :: rest -> (Subtract, rest)
  | Times :: rest -> (Mult, rest)
  | Divide :: rest -> (Div, rest)
  | Mod :: rest -> (Modulo, rest)
  | Lt :: rest -> (LtComp, rest)
  | Eq :: rest -> (EqComp, rest)
  | Concat :: rest -> (Concatenate, rest)
  | And :: rest -> (AndComp, rest)
  | Or :: rest -> (OrComp, rest)

(* EXPR CODE START *)

(* Helper function to parse [<param>]* in Let bindings and expressions
   Output list of parsed <param>s (could be []) *)
and parse_params_list (src : token list) =
  let rec helper src acc = 
    match src with
    (* (Finished)
       these tokens are used in parse_opt_type_and_expr, so they are kept *)
    | Colon :: rest | Eq :: rest -> (List.rev acc, src)
    (* add parsed <param> to "acc" list *)
    | _ ->
      let (p, rest) = parse_param src in
      helper rest (p :: acc)
  in 
  helper src []

(* Helper function to parse optional type for FunExpr in bexpr*)
and parse_opt_type (src : token list) =
  match src with
  (* optional type *)
  | Colon :: rest -> 
    let (t, rest2) = parse_type rest in
    (Some(t), rest2)
  (* no type specified *)
  | DoubleArrow :: rest -> 
    (None, rest)

(* Helper function to parse [: <type>] in Let bindings and expressions 
    Output type (option) and expression *)
and parse_opt_type_and_expr (src : token list) =
  match src with
  (* optional type *)
  | Colon :: rest -> 
    let (t, rest2) = parse_type rest in
    let rest3 = expect Eq rest2 in
    let (e, rest4) = parse_expr rest3 in
    (Some(t), e, rest4)
  (* no type specified *)
  | Eq :: rest -> 
    let (e, rest2) = parse_expr rest in
    (None, e, rest2)

(* Helper function to parse TupleExpr *)
and parse_expr_list (src : token list) =
  let rec helper acc src =
    match src with
    | RParen :: r -> (List.rev acc, r)
    | Comma :: r ->
      let (e, r2) = parse_expr r in
      helper (e :: acc) r2
    | _ ->
      let (e, r) = parse_expr src in
      helper (e :: acc) r
  in
  helper [] src

(* Helper function to parse Let expressions for bexpr *)
and parse_let_expr (src : token list) =
  match src with
    | Id x :: _ -> 
      let (p, rest) = parse_params_list src in 
      let (t, e1, rest2) = parse_opt_type_and_expr rest in
      let rest3 = expect In rest2 in
      let (e2, rest4) = parse_expr rest3 in
      (LetExpr (x, p, t, e1, e2), rest4)
    | Rec :: Id x :: _ -> 
      let (p, rest) = parse_params_list src in 
      let (t, e1, rest2) = parse_opt_type_and_expr rest in
      let rest3 = expect In rest2 in
      let (e2, rest4) = parse_expr rest3 in
      (LetRecExpr (x, p, t, e1, e2), rest4)
    | _ -> raise (ParseError "Ill-formed let expression")

(** Parse a base expression -- that is, expressions which are outside of the
    operator precedence hierarchy. *)
and bexpr : token list -> expr * token list = function
  | Let :: r -> 
    let (e, r2) = parse_let_expr r 
    in (e, r2)
  | If :: r ->
    let (ce, r2) = parse_expr r in
    let r3 = expect Then r2 in
    let (te, r4) = parse_expr r3 in
    let r5 = expect Else r4 in
    let (fe, r6) = parse_expr r5 in
    (IfExpr (ce, te, fe), r6)
  | Fun :: r ->
    let (p, r2) = parse_params_list r in
    let (ty, r3) = parse_opt_type r2 in
    let (e, r4) = parse_expr r3 in
    (FunExpr (p, ty, e), r4)
  | Not :: r ->
    let (e, r2) = bexpr r in
    (UnopExpr (NotComp, e), r2)
  | LParen :: r ->
    let (e, r2) = parse_expr r in
    (e, expect RParen r2)
  | Int i :: r -> (IntExpr i, r)
  | True :: r -> (TrueExpr, r)
  | False :: r -> (FalseExpr, r)
  | String s :: r -> (StrExpr s, r)
  | Id x :: r -> (IdExpr x, r)
  | LParen :: RParen :: r -> (UnitExpr, r)
  (* ( <expr> [, <expr>]+ )  *)
  | LParen :: r -> 
    let (e_ls, r2) = parse_expr_list r in
    (TupleExpr (e_ls), r2)
  | [] -> raise (ParseError "Expected base expression, got end of input")
  (* <expr> <expr> 
     this shadows this error handling branch
     | t :: _ -> raise (ParseError ("Expected base expression, got " ^ tok_to_str t))
     but i can't think of a better way right now *)
  | src ->
    let (e1, r1) = parse_expr src in
    let (e2, r2) = parse_expr r1 in
    (AppExpr (e1, e2), r2)


(** Parse a negate expression. *)
and nexpr : token list -> expr * token list = function
  | Negate :: r ->
    let (e, r2) = bexpr r in
    (UnopExpr (Neg, e), r2)
  | t :: _ -> raise (ParseError ("Expected ~, got: " ^ tok_to_str t))

(** Parse a multiplicative expression. *)
and mexpr (s : token list) : expr * token list =
  lassoc [(Times, fun a b -> BinopExpr (a, Mult, b));
          (Divide, fun a b -> BinopExpr (a, Div, b));
          (Mod, fun a b -> BinopExpr (a, Modulo, b))]
          nexpr
          s

(** Parse an additive expression. *)
and aexpr (s : token list) : expr * token list =
  lassoc [(Plus, fun a b -> BinopExpr (a, Add, b));
          (Minus, fun a b -> BinopExpr (a, Subtract, b));
          (Concat, fun a b -> BinopExpr (a, Concatenate, b))]
          mexpr
          s

(** Parse a comparison expression. *)
and cexpr (s : token list) : expr * token list =
  lassoc [(Lt, fun a b -> BinopExpr (a, LtComp, b));
          (Eq, fun a b -> BinopExpr (a, EqComp, b))]
          aexpr
          s

(** Parse a conjunction. *)
and conj_expr (s : token list) : expr * token list =
  lassoc [(And, fun a b -> BinopExpr (a, AndComp, b))]
          cexpr
          s

(* Parse a disjunction *)
and parse_expr (s : token list) : expr * token list =
  lassoc [(Or, fun a b -> BinopExpr (a, OrComp, b))]
         conj_expr
         s

(* EXPR CODE END *)


and parse_param : token list -> param * token list = function
  | Id x :: rest -> (VarParam (x), rest)
  | LParen :: Id x :: Colon :: rest -> 
    let (ty, rest2) = parse_type rest in
    let rest3 = expect RParen rest2 in
    (VarTyParam (x, ty), rest3)
    

and parse_binding (src : token list) : binding * token list =

  (* helper function for third branch ['|' $id [of <type>]]+ *)
  let parse_type_constructor (src : token list)  = 
    let rec parse_type_constructor_args acc src =
      match src with
      (* ['|' $id [of <type>]]+ *)
      | Id x :: Of :: rest ->
        let (ty, rest2) = parse_type rest in 
        parse_type_constructor_args ((x, Some(ty)) :: acc) rest2
      (* ['|' $id ]+ *)
      | Id x :: rest -> 
        parse_type_constructor_args ((x, None) :: acc) rest
      (* finished *)
      | _ -> (List.rev acc, src) in
    parse_type_constructor_args [] src in

  (match src with
    | Let :: Id x :: params ->
      let (p, rest) = parse_params_list params in 
      let (t, e, rest2) = parse_opt_type_and_expr rest in
      (LetBind (x, p, t, e), rest2)
    | Let :: Rec :: Id x :: params ->
      let (p, rest) = parse_params_list params in 
      let (t, e, rest2) = parse_opt_type_and_expr rest in
      (LetRecBind (x, p, t, e), rest2)
    | Type :: Id x :: Eq :: ls ->
      let (ty_ls, rest) = parse_type_constructor ls in
      (TypeBind (x, ty_ls), rest))


and parse_program (src : token list) : program * token list = 
  (* helper function to parse each binding*)
  let rec parse_mult_bindings src = match src with 
    (* src should be [] *)
    | [] -> ([], src)
    | rest ->
      let (b, rest2) = parse_binding rest in
      let (b2, rest3) = parse_mult_bindings rest2 in
      (b :: b2, rest3) in
  let binds, rest = parse_mult_bindings src in
  (binds, rest)

let parse (src: string) : program  =
  let (ex, rest) = parse_program (tokenize src) in
  match rest with
  | [] -> ex
  | t :: _ -> raise (ParseError ("Expected end of file, got: " ^ tok_to_str t))