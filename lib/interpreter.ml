open Ast

exception RuntimeError of string

type value =
  | VInt of int
  | VBool of bool
  | VStr of string
  | VUnit
  | VTuple of value list
  (* defined by param/arg(s) of fun, env, expr, name of recursion function (optional) *)
  | VClosure of string list * (string * value) list * expr * string option 
  (* type defs and constructors *)
  | VUser of string * value list

let rec print_value : value -> string = function
  | VInt i -> string_of_int i
  | VBool b -> string_of_bool b
  | VStr s -> s
  | VUnit -> "()"
  | VTuple ls ->
    "(" ^ String.concat (", ") (List.map print_value ls) ^ ")"
  (* placeholder -- not sure how to print *)
  | VClosure _ -> "vclosure"
  | VUser _ -> "vuser"

type env = (string * value) list

let string_of_param = function 
  | VarParam id | VarTyParam (id, _) -> id
  
let rec interp_binop_expr (bop : binop) (v1 : value) (v2 : value) : value = match bop with
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

and interp_expr (e : expr) (env : env) : value = match e with
  (* Handle creation of closures and manage environment *)
  | LetExpr (id, params, _, e1, e2) -> 
    let v1 = 
      (match params with 
        | [] -> interp_expr e1 env
        | _ -> 
          let param_ids = List.map string_of_param params in
          VClosure (param_ids, env, e1, None)) in 
    let env' = (id, v1) :: env in 
    interp_expr e2 env'
  
  | LetRecExpr (id, params, _, e1, e2) -> 
    (* same as LetExpr, but eval e1 with the closure in the context *)
    let rec_closure = 
      let param_ids = List.map string_of_param params in 
      VClosure (param_ids, env, e1, Some id) in
    let rec_env = (id, rec_closure) :: env in
    let v1 = interp_expr e1 rec_env in  

    let env' = (id, v1) :: env in
    interp_expr e2 env'

  | IfExpr (c, t, e) -> (match interp_expr c env with
    | VBool true -> interp_expr t env
    | VBool false -> interp_expr e env
    | _ -> raise (RuntimeError "Non-boolean argument to condition in if-statement"))

  | FunExpr (params, _, e) -> 
    let param_ids = List.map string_of_param params in 
    (* TO-DO : handle rec case? *)
    let rec_f_id = None in
    VClosure (param_ids, env, e, rec_f_id)

  | AppExpr (e1, e2) -> 
    let v1 = interp_expr e1 env in
    let v2 = interp_expr e2 env in 
    (match v1 with
      | VClosure (param_ids, env, e3, rec_f_id) -> 
        (match param_ids with 
          | id :: rest -> 
            let env' = (id, v2) :: env in 
            (match rest with 
              (* all params added to env *) 
              | [] -> 
                (match rec_f_id with 
                  (* Add a new binding to the environment where function name points to 
                  the (current) closure created in LetRecBind *)
                  | Some id -> interp_expr e3 ((id, v1) :: env') 
                  | None -> interp_expr e3 env')
              (* currying function params...?
                 TO-DO: check later *)
              | _ -> VClosure (rest, env', e3, rec_f_id))
          | [] -> raise (RuntimeError "No params found in function"))
      | _ -> raise (RuntimeError "Function application expected"))


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
    | None -> raise (RuntimeError ("Undefined variable " ^ x)))
  | UnitExpr -> VUnit
  | MatchExpr (e, branch_ls) -> 
    (* v is a VUser value *)
    let v = interp_expr e env in 
    (* find branch with same id as constructor in VUser *)
    let constr_id, val_ls = (match v with 
      | VUser (id, val_ls) -> id, val_ls
      | _ -> raise (RuntimeError "MatchExpr: expected VUser value")) in 
    interp_match_branch branch_ls constr_id val_ls env

and interp_match_branch ls constr_id val_ls env = match ls with 
  | MatchBr (id, p_vars, ex) :: rest -> 
    if constr_id = id then 
      (* if any pattern vars, add to environment
      take names from list of pattern variables, values from VUser object and match together 
      to become new name-value pair bindings in env *)
      let env' = (match p_vars with 
        | Some vars -> (match vars with 
          | PatternVar id -> List.combine [id] val_ls @ env
          | PatternMultiVars ids -> List.combine ids val_ls @ env)
        | None -> env) in
      (* evaluate expr body of match branch, recursive call with new env *)
      interp_expr ex env'
    else interp_match_branch rest constr_id val_ls env
  | [] -> raise (RuntimeError "No branches matched")

let interp_binding (b : binding) (env : env) : env =
  match b with
  | LetBind (id, params, _, e) ->
    (match params with 
      (* not a function, so just eval expr and update env *)
      | [] -> 
        let v = interp_expr e env in
        (id, v) :: env
      (* Closure is created if params exist (this is a function) *)
      | _ -> 
        let param_ids = List.map string_of_param params in 
        let closure = VClosure (param_ids, env, e, Some (id)) in 
        (id, closure) :: env)
  
  | LetRecBind (id, params, _, e) ->
    let param_ids = List.map string_of_param params in 
    let closure = VClosure (param_ids, env, e, Some (id)) in
    (* defer construction of recursive closure until func app
       closure is just used to keep track of function name *)
    (id, closure) :: env 

  (* Add each constructor to the evaluation context *)
  | TypeBind (_, constr_ls) -> (
    match constr_ls with 
      | [] -> raise (RuntimeError "Type binding has no constructors")
      | _ ->
        List.fold_left (fun acc c -> 
        let v = (match c with 
          (* no type args *)
          | cid, None -> (cid, VClosure ([], env, IdExpr cid, None))
          | cid, Some _ -> (cid, VUser (cid, []))) in 
        v :: acc) env constr_ls)
        
(* process each binding in order while building up an evaluation context. *)
let rec interp_program (p : program) (env) : env = 
  match p with
  | [] -> env
  | binding :: rest ->
    let env' = interp_binding binding env in 
    interp_program rest env'

(* loop through the program to evaluate each binding, building up the eval context *)
let interp (p : program) : env  = 
  interp_program p []