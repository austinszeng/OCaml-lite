open Lexer
open Ast

exception ParseError of string

open Lexer

exception ParseError of string

let unexpected_end (_ : unit) : 'a =
  raise (ParseError "Unexpected end of input")

let expect (t : token) : token list -> token list = function
  | [] -> unexpected_end ()
  | t1 :: ts ->
    if t = t1
    then ts
    else raise (ParseError
                  ("Expected " ^ tok_to_str t ^ ", got: " ^ tok_to_str t1))

let rec parse_pattern_vars : token list -> pattern_vars * token list = function
  | Id x :: rest -> (PatternVar (x), rest)
  | LParen :: rest -> 
      let rec parse_pattern_vars_list src (acc : string list) = 
        match src with
        | RParen :: rest -> (List.rev acc, rest)
        | Id x :: Comma :: rest -> parse_pattern_vars_list (x :: acc) rest in
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

(* Need to look into this function a lot more specifically with LParen mechanics *)
and parse_type (src : token list) : typ * token list =  
  let rec single_type : token list = function 
    | TInt :: rest -> (IntTy, rest)
    | TBool :: rest -> (BoolTy, rest)
    | TString :: rest -> (StrTy, rest)
    | TUnit :: rest -> (UnitTy, rest)
    | Id x :: rest -> (UserTy x, rest)
    | LParen :: rest -> 
      let (ty1, rest2) = parse_type rest in
      let rest3 = expect RParen rest2 in
      (ty1, rest3) in 
  (ty1, rest) = single_type src in
  let rec pair_type (src : token list) (ty1 : typ) = 
    match src with
      | Arrow :: rest ->
        let (ty2, rest2) = single_type rest in
        (FuncTy (ty1, ty2), rest2)
      | Times :: rest -> 
        let (ty1, rest2) = single_type rest in
        (PairTy (ty1, ty2), rest2)
      (* not of form  <type> * <type> or <type> -> <type>
          pair_type function does nothing *)
      (* RParen :: rest is implied *)
      | _ -> (ty1, src) in
  pair_type rest ty1


and parse_unop : token list -> unop * token list = function
  | Not :: rest -> (NotComp, rest)
  | Negate :: rest -> (Neg, rest)

 and parse_binop : token list -> binop * token list = function
  | Plus :: rest -> (Add, rest)
  | Minus :: rest -> (Minus, rest)
  | Times :: rest -> (Mult, rest)
  | Divide :: rest -> (Div, rest)
  | Mod :: rest -> (Modulo, rest)
  | Lt :: rest -> (LtComp, rest)
  | Eq :: rest -> (EqComp, rest)
  | Concat :: rest -> (Concatenate, rest)
  | And :: rest -> (AndComp, rest)
  | Or :: rest -> (OrComp, rest)


(*<expr> ::= let $id [<param>]* [: <type>] = <expr> in <expr>
         | let rec $id [<param>]* [: <type>] = <expr> in <expr>
         | if <expr> then <expr> else <expr>
         | fun [<param>]+ [: <type>] => <expr>
         | <expr> <expr>
         | ( <expr> [, <expr>]+ )
         | <expr> <binop> <expr>
         | <unop> <expr>
         | ( <expr> )
         | $int
         | true
         | false
         | $string
         | $id
         | ( )
         | match <expr> with ['|' <match_branch>]+
         
(Highest precedence)
not
~
*, /, mod
+, -, ^
<, =
&&
||
(Lowest precedence)

Perhaps mimic structure of expression hw to enforce precedence
*)
and parse_expr : token list -> expr * token list = function
  |




and parse_param : token list -> param * token list = function
  | Id x :: rest -> (VarParam (x), rest)
  | LParen :: Id x :: Colon :: rest -> 
    let (ty, rest2) = parse_type rest in
    let rest3 = expect RParen rest2 in
    (VarTyParam (x, t), rest3)
    

and parse_binding (src : token list) : binding * token list =

  let rec parse_params_list (src : token list) (acc : string list) =
    let (params_list, rest) =
      (match src with
      (* finished *)
      | RParen :: rest -> (List.rev acc, rest)
      (* these tokens are used in parse_opt_type, so they are kept *)
      | Colon | Eq -> (List.rev acc, src)
      (* add parsed <param> to "acc" list *)
      | Id x :: Comma :: rest | Id x :: rest -> parse_params_list (parse_param (x) :: acc) rest)
    in (params_list, rest) in

  let parse_opt_type (src : token list) (recu : bool) (params_list : string list)-> binding * token list =
    match src with
    (* optional type *)
    | Colon :: typ -> 
      let (t, rest2) = parse_type rest in
      let (e, rest3) = parse_expr rest2 in
      (if not recu then (LetBind (x, params_list, Some(t), e), rest3)
      else (LetRecBind (x, params_list, Some(t), e), rest3))
    (* no type specified *)
    | Eq :: e -> 
      let (e2, rest2) = parse_expr rest in
      (if not recu then (LetBind (x, params_list, None, e2), rest2)
      else (LetRecBind (x, params_list, None, e2), rest2)) in

  (* helper function for third branch ['|' $id [of <type>]]+ *)
  let rec parse_type_constructor (src : token list) = 
    let parse_type_constructor_arg (src: token list) (acc : (string * typ) list)  =
      match src with
      (* ['|' $id [of <type>]]+ *)
      | Id x :: Of :: rest ->
        let (ty, rest2) = parse_type rest in 
        (List.rev ((x, Some(ty)) :: acc), rest2)
      (* ['|' $id ]+ *)
      | Id x -> (List.rev ((x, None) :: acc), src) in
    (match src with
    | Pipe :: rest -> 
      let (ls, rest2) = parse_type_constructor_arg rest [] in
      (match rest2 with
      (* another constructor arg *)
      | Pipe :: rest -> 
        (* recursion? *)
        let (ls2, rest3) = parse_type_constructor rest [] in
        (ls2, rest3)
      (* type binding is finished *)
      | _ -> (ls, rest2))) in

  (match src with
    | Let :: Id x :: params ->
      (* p can be an empty list [] *)
      let (p, rest) = parse_params_list params [] in 
      parse_opt_type rest False p
    | Let :: Rec :: Id x :: params ->
      let (p, rest) = parse_params_list params [] in 
      parse_opt_type rest True p
    | Type :: Id x :: Eq :: ls ->
      let (ty_ls, rest) = parse_type_constructor ls in
      (TypeBind (x, ty_ls), rest))


and parse_program (src : token list) : program * token list = 
  (* helper function to parse each binding*)
  let rec parse_mult_bindings src = match src with 
    | [] -> ([], src)
    | rest ->
      let (b, rest2) = parse_binding rest in
      let (b2, rest3) = parse_mult_bindings rest2 in
      (b :: b2, rest3) in
  let binds, rest = parse_mult_bindings src in
  (binds, rest)

let parse (src: string) : program  =
  let (ex, rest) = program (tokenize src) in
  match rest with
  | [] -> ex
  | t :; _ -> raise (ParseError ("Expected end of file, got: " ^ tok_to_str t))