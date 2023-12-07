open Ast
open Parser

exception RuntimeError of string

type value =
  | VInt of int
  | VBool of bool
  | VStr of string
  | VUnit
  | VTuple of value list
  (* defined by param/arg of fun, env, expr, name of recursion function (optional)*)
  | VClosure of string * (string * value) list * expr * string option 
  (* type defs and constructors *)
  | VUser of string * value list

type env = (string * value) list

let rec interp_expr (e : expr) (env : env) : value = match e with
  (* 
  E |- e1 ==> v1    E[x -> v1] |- e2 ==> v2
  -----------------------------------------
        E |- let x = e1 in e2 ==> v2

  Handle creation of closures and manage environment
  *)
  | LetExpr (id, params, typ_opt, e1, e2) -> 
    let v1 = interp_expr e1 env in 
    let env' = (id, v1) :: env in 
    interp_expr e2 env'
  
  (* revisit *)

  | LetRecExpr (id, params, typ_opt, e1, e2) -> 
    (* 
    TO-DO: "id" needs to be replaced with the parameter of the function
       but how do I only put one param if it can take multiple? 
    *)
    let closure = VClosure (id, env, e1, Some id) in
    let env' = (id, closure) :: env in
    interp_expr e2 env'

  | IfExpr (c, t, e) -> (match interp_expr c env with
    | VBool true -> interp_expr t env
    | VBool false -> interp_expr e env
    | _ -> raise (RuntimeError "Non-boolean argument to condition in if-statement"))
  (* 
  ----------------------------------
  E |- fun x => e1 ==> (E, "fun x => e1")

  I think i need to do something with params to get the arg string?

  Maybe create a function that curries:::

  Note: throughout this page I'll be referring only to functions of a single argument. 
  Remember that functions in OCaml-lite are curried, so a function like fun x y => x + y 
  can be rewritten as fun x => fun y => x + y. That means we only ever need to deal with 
  functions of one argument.
  *)
  | FunExpr (params, typ_opt, e) -> VClosure("", env, e, None)

  (*

  E |- e1 ==> (E', "fun x => e3")   E |- e2 ==> v1   E'[x -> v1] |- e3 ==> v2
  ---------------------------------------------------------------------------
                              E |- e1 e2 ==> v2   
  *)
  | AppExpr (e1, e2) -> 
    let v1 = interp_expr e1 env in (* should be a closure of form (E', "fun x => e3")*)
    let v2 = interp_expr e2 env in (* E |- e2 ==> v1 *)
    (match v1 with
      | VClosure (param, env, ex, rec_f_id) -> 
        (* update environment to show what arg evals to *)
        let env' = (param, v2) :: env in 
        (match rec_f_id with
        | Some id -> interp_expr ex ((id, v1) :: env') (* E'[x -> v1] |- e3 ==> v2 *)
        | None -> raise (RuntimeError "Function application expected"))
      | _ -> raise (RuntimeError "Function application expected: closure not found"))
  | TupleExpr ls -> VTuple (List.map (fun ex -> interp_expr ex env) ls)
  | BinopExpr (e1, bop, e2) -> 
    let v1 = interp_expr e1 env in 
    let v2 = interp_expr e2 env in
    interp_binop_expr bop v1 v2
  | UnopExpr (uop, e) -> 
    let v = interp_expr e env in 
    interp_unop_expr uop v
  | IntExpr i -> VInt i
  | TrueExpr -> VBool true
  | FalseExpr -> VBool false
  | StrExpr s -> VStr s
  (* Look up the id in env and return the associated value
     If no id in env, undefined variable error *)
  | IdExpr x -> 
    (match (List.assoc_opt x env) with
    | Some v -> v
    | None -> raise (RuntimeError "Undefined variable"))
  | UnitExpr -> VUnit
  | MatchExpr (e, branch_ls) -> 
    let v = interp_expr e env in 
    interp_match_branches v branch_ls env

and interp_binop_expr (bop : binop) (v1 : value) (v2 : value) : value = match bop with
  | Add -> (match (v1, v2) with
    | (VInt i1, VInt i2) -> VInt (i1 + i2)
    | _ -> raise (RuntimeError "Non-integer argument to +")) 
  | Subtract -> (match (v1, v2) with
    | (VInt i1, VInt i2) -> VInt (i1 - i2)
    | _ -> raise (RuntimeError "Non-integer argument to -")) 
  | Mult -> (match (v1, v2) with
    | (VInt i1, VInt i2) -> VInt (i1 * i2)
    | _ -> raise (RuntimeError "Non-integer argument to *")) 
  | Div -> (match (v1, v2) with
    | (VInt i1, VInt i2) -> VInt (i1 / i2)
    | _ -> raise (RuntimeError "Non-integer argument to /")) 
  | Modulo -> (match (v1, v2) with
    | (VInt i1, VInt i2) -> VInt (i1 mod i2)
    | _ -> raise (RuntimeError "Non-integer argument to mod")) 
  | LtComp -> (match (v1, v2) with
    | (VInt i1, VInt i2) -> VBool (i1 < i2)
    | _ -> raise (RuntimeError "Non-integer argument to <")) 
  | EqComp -> (match (v1, v2) with
    | (VInt i1, VInt i2) -> VBool (i1 = i2)
    | (VBool b1, VBool b2) -> VBool (b1 = b2)
    | _ -> raise (RuntimeError "Non-integer/Non-boolean argument to =")) 
  | Concatenate -> (match (v1, v2) with
    | (VStr s1, VStr s2) -> VStr (s1 ^ s2)
    | _ -> raise (RuntimeError "Non-string argument to ^")) 
  | AndComp -> (match (v1, v2) with
    | (VBool b1, VBool b2) -> VBool (b1 && b2)
    | _ -> raise (RuntimeError "Non-boolean argument to &&"))
  | OrComp -> (match (v1, v2) with
    | (VBool b1, VBool b2) -> VBool (b1 || b2)
    | _ -> raise (RuntimeError "Non-boolean argument to ||"))

and interp_unop_expr (uop : unop) (v : value) : value = match uop with
  | NotComp -> (match v with
    | (VBool b) -> VBool (not b)
    | _ -> raise (RuntimeError "Non-boolean argument to not"))
  | Neg -> (match v with
    | (VInt i) -> VInt (-i)
    | _ -> raise (RuntimeError "Non-integer argument to ~"))

(* Helper functions for interp_expr MatchExpr
   
   In order to evaluate the expression match e with | p1 -> e1 | p2 -> e2 | ..., 
   we need to first evaluate e to get a value v. Then we need to check p1, p2, ... 
   one at a time to see if they match v. If they do, we add any appropriate values 
   to the context and evaluate the associated expression.*)
and interp_match_branches (v : value) (branch_ls : match_branch list) (env : env) : value =
  (* Loop through match branches to check if $id [<pattern_vars>] matches v... *)
  match branch_ls with
  | MatchBr (pat, pat_vars_opt, ex) :: rest -> 
    (match (interp_pattern v pat pat_vars_opt env) with 
      | Some env' -> 
        (* Expression in match branch gets evaled with new env *)
        interp_expr ex env'
        (* Try next branch *)
      | None -> interp_match_branches v rest env)
  | [] -> raise (RuntimeError "Match expression has no matching pattern")

(* If branch matches v, add pattern to context? *)
and interp_pattern (v : value) (pat : string) (pat_vars_opt : pattern_vars option) (env : env) : env option = 
  match pat_vars_opt with 
  | Some pat_vars -> 
    (match v, pat_vars with 
    (* single var pattern *)
    | VInt _, PatternVar id
    | VBool _, PatternVar id
    | VStr _, PatternVar id -> Some ((id, v) :: env)
    (* tuple *)
    | VTuple vals, PatternMultiVars ids -> 
      if List.length vals = List.length ids then 
        Some (List.fold_left2 (fun en id v -> (id, v) :: en) env ids vals)
      (* no match *)
      else None
    | _ -> raise (RuntimeError "Catch-all: Unimplemented VClosure and VUser"))
  (* no pattern variables, only VUnit has no vars *)
  | None -> 
    (match v with 
    (* Matches branch, but no need to update env *)
    | VUnit -> if pat = "()" then Some env else None
    | _ -> raise (RuntimeError "Invalid pattern, unmatched pattern variables"))

(* process each binding in order while building up an evaluation context. A binding can be 
   either a value binding (let or let rec) or a type binding (type). 
   
   To process a type binding, add each constructor from that type declaration to the 
   evaluation context. In order to process a value binding, evaluate the 
   expression on the right hand side of the binding and associate the resulting
   value with the name on the left hand side of the binding in the evaluation 
   context. In this case you'll need to consider what context to use when 
   evaluating the right hand side based on the presence of absence of the rec 
   keyword. *)
let rec interp_binding (b : binding) (env : env) : env =
  match b with
  (* TO-DO: Deal with params *)
  | LetBind (id, params, typ_opt, e) | LetRecBind (id, params, typ_opt, e) ->
    let v = interp_expr e env in
    (id, v) :: env
  
  | LetRecBind (id, params, typ_opt, e) ->
    (* VClosure (params, env, e, id) *)
    (* TO-DO: Placeholder... *)
    let v = interp_expr e env in
    (id, v) :: env

  | TypeBind (id, constr_ls) ->
    let env' = List.fold_left (bind_constructor id) env constr_ls in
    env'

and bind_constructor (type_id : string) (env : env) (constructor : string * typ option) : env =
  match constructor with
  | (constr_id, Some ty) ->
    (* Add a function to the environment representing the constructor *)
    let closure = VClosure (constr_id, [], IdExpr type_id, None) in
    (constr_id, closure) :: env
  | _ -> env
  
let rec interp_program (p : program) (env) : env = 
  match p with
  | [] -> env
  | binding :: rest ->
    let env' = interp_binding binding env in 
    interp_program rest env'

(* loop through the program to evaluate each binding, building up the eval context *)
let rec interp (p : program) : env  = 
  interp_program p []