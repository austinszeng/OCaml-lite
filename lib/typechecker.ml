open Ast

exception TypeError of string

(* store set of expr to typtyp bindings *)
type env = (string * typ) list

type c = typ * typ
type constraints = (c) list

type substitutions = (int * typ) list

(* Generate fresh variable for polymorphism, indexed by integers *)
let next_var = ref 0
let fresh_var () : int =
  next_var := !next_var + 1;
  !next_var
let fresh_var_ty () : typ = 
  VarTy (fresh_var())

(* Return free type variables in a type *)
let rec free_var (ty : typ) : int list =
  match ty with
  | IntTy | BoolTy | StrTy | UnitTy | UserTy _ -> []
  | FuncTy (ty1, ty2) -> free_var ty1 @ free_var ty2
  | TupleTy tys -> List.concat (List.map free_var tys)
  | VarTy i -> [i]
  | ForallTy (i, ty) -> List.filter (fun j -> i <> j) (free_var ty)

(* Apply substitutions to update constraints/ environment *)
let rec subst (ty : typ) (s : substitutions) : typ = 
  match ty with
  | IntTy | BoolTy | StrTy | UnitTy | UserTy _ -> ty
  | FuncTy (ty1, ty2) -> FuncTy (subst ty1 s, subst ty2 s)
  | TupleTy tys -> TupleTy (List.map (fun ty -> subst ty s) tys)
  | VarTy i -> 
    (* Gets the type associated with poly type in substitutions list, if any *)
    (match List.assoc_opt i s with
    | None -> ty
    | Some t -> subst t s)
  (* If quantifier not in s, done
     Else, need to apply to ty *)
  | ForallTy (i, t) -> 
    (match List.assoc_opt i s with
    | None -> ty
    | Some _ -> ForallTy (i, subst t s))

(* Unification (solving constraints) *)
(* Pick and remove a constraint t1 = t2 from set
   Reduce based on t1 and t2
    - update solution with new substitution (add to list of substs)
    - OR add new constraints to set 
    - OR fail (so requires error handling...) *)

(* Helper reduction/ unification function:
   If one side of constraint is VarTy, then first perform occurs check to 
   determine whether said VarTy occurs in other side of constraint 
   If it does, unification fails *)
let rec occurs_check (id : int) (ty : typ) : bool =
  match ty with
  | FuncTy (t1, t2) -> occurs_check id t1 || occurs_check id t2
  | TupleTy tys -> List.exists (fun t -> occurs_check id t) tys
  | VarTy i -> i = id
  | IntTy | BoolTy | StrTy | UnitTy | UserTy _ | ForallTy _ -> false

(* Helper unification function that performs unification algorithm on types t1 and t2 *)
and reduce (t1 : typ) (t2 : typ) (subs : substitutions) : substitutions =
  if t1 = t2 then subs else
  match (t1, t2) with
  | (VarTy i, _) -> 
      if occurs_check i t2 then raise (TypeError "Infinite type detected")
      (* Map one variable to the other *)
      else ([i, t2] @ subs)
  | (_, VarTy i) -> 
      if occurs_check i t1 then raise (TypeError "Infinite type detected")
      else ([i, t1] @ subs)
  (* Replace constraint with two new constraints, unifying their argument and return types *)
  | (FuncTy (t1, t2), FuncTy (t3, t4)) -> unify ([(t1, t3); (t2, t4)]) (subs)
  (* Replace constraint with set of new constraints unifying each of their constituent types*)
  | (TupleTy tys1, TupleTy tys2) -> 
    if List.length tys1 = List.length tys2 then unify (List.combine tys1 tys2) (subs)
    else raise (TypeError ("Unification failed: Tuple types " ^ print_type t1 ^ " and " ^ print_type t2 ^ " are of unequal length"))
  (* Replace quantified variable by a fresh variable
     Is this instantiation?? *)
  | (ForallTy (id, _), _) | (_, ForallTy (id, _)) -> ((id, fresh_var_ty()) :: subs)
  (* Nothing happens with base types, done *)
  | (IntTy, IntTy) | (BoolTy, BoolTy) | (StrTy, StrTy) | (UnitTy, UnitTy) -> subs
  | _ -> raise (TypeError ("Unification failed: " ^ print_type t1 ^ " and " ^ print_type t2 ^ " cannot be unified."))

(* Main unification function that iterates over set of constraints 
   Keeps track of list of substitutions, each is immediately applied to list of constraints *)
and unify (constraints : constraints) (subs : substitutions) : substitutions = 
  match constraints with
  | [] -> subs
  | (t1, t2) :: rest ->
    let new_subs = reduce t1 t2 subs in
    (* apply substitution immediately after a new one is added *)
    let rest_subst = List.map (fun (t1, t2) -> (subst t1 new_subs, subst t2 new_subs)) rest 
    in unify rest_subst new_subs

(* Generalize type by quantifying each type variable that appears 
   free in type but not in the typing context *)
let generalize (ty : typ) (env : env) : typ =
  (* get all free vars in given type *)
  let free_vars_ty = free_var ty in
  (* get all free variables from typing context *)
  let free_vars_env = List.concat (List.map (fun (_, t) -> free_var t) env) in
  (* quantify: free variable in given type but not free in typing context *)
  let to_quant = List.filter (fun v -> not (List.mem v free_vars_env)) free_vars_ty in
  List.fold_right (fun v acc -> ForallTy (v, acc)) to_quant ty

(* Collecting Set of Constraints *)
let rec expr_constraints (e : expr) (env : env) : typ * constraints = match e with
  | IfExpr (c, t, e) -> 
    let (c_ty, c_c) = expr_constraints c env in 
    let (t_ty, t_c) = expr_constraints t env in
    let (e_ty, e_c) = expr_constraints e env in
    (* c(ondition) has to be bool and t(hen) and e(lse) branches have to be same type *) 
    let if_constraint = [(c_ty, BoolTy); (t_ty, e_ty)] in 
    let constraints = c_c @ t_c @ e_c @ if_constraint in
    (t_ty, constraints)
  | AppExpr (e1, e2) -> 
    let (e1_ty, e1_c) = expr_constraints e1 env in 
    let (e2_ty, e2_c) = expr_constraints e2 env in 
    let result_type = fresh_var_ty() in 
    let app_constraint = [(e1_ty, FuncTy (e2_ty, result_type))] in
    (result_type, e1_c @ e2_c @ app_constraint)
  | BinopExpr (e1, bop, e2) ->
    let (e1_ty, e1_c) = expr_constraints e1 env in
    let (e2_ty, e2_c) = expr_constraints e2 env in
    let binop_constraint = 
      (match bop with
      | Add | Subtract | Mult | Div | Modulo -> [(e1_ty, IntTy); (e2_ty, IntTy)]
      | LtComp -> [(e1_ty, IntTy); (e2_ty, IntTy)]
      | EqComp -> [(e1_ty, e2_ty)]
      | Concatenate -> [(e1_ty, StrTy); (e2_ty, StrTy)]
      | AndComp | OrComp -> [(e1_ty, BoolTy); (e2_ty, BoolTy)]) in
    let return_type = 
      (match bop with
      | Add | Subtract | Mult | Div | Modulo -> IntTy
      | LtComp | EqComp | AndComp | OrComp -> BoolTy
      | Concatenate -> StrTy) in
    (return_type, e1_c @ e2_c @ binop_constraint)
  | UnopExpr (uop, ex) ->
    let (ex_ty, ex_c) = expr_constraints ex env in
    let unop_constraints =
      (match uop with
      | NotComp -> [(ex_ty, BoolTy)]
      | Neg -> [(ex_ty, IntTy)]) in
    let return_type =
      (match uop with
      | NotComp -> BoolTy
      | Neg -> IntTy) in
    (return_type, ex_c @ unop_constraints)
  | IntExpr _ -> (IntTy, [])
  | TrueExpr | FalseExpr -> (BoolTy, [])
  | StrExpr _ -> (StrTy, [])
  | IdExpr id ->
    (match List.assoc_opt id env with
    | Some ty -> (ty, [])
    | None -> raise (TypeError ("Unbound variable: " ^ id))) 
  | UnitExpr -> (UnitTy, [])
  (* all branch output exprs should match
     the types of each string in each branch should match ex *)
  (* | MatchExpr (ex, branch_ls) ->  *)
  | LetExpr (id, params, ty_opt, e1, e2) ->
    (* - handle params ... 
       - generalize? *)
    let (e1_ty, e1_c) = expr_constraints e1 env in
    let new_env = (id, e1_ty) :: env in 
    let (e2_ty, e2_c) = expr_constraints e2 new_env in 
    let let_constraint = 
      (match ty_opt with
      | Some t -> [(t, e1_ty)]
      | None -> []) in
    (e2_ty, let_constraint @ e1_c @ e2_c)
  (* | LetRecExpr (id, params, ty_opt, e1, e2) ->   *)
  (* | TupleExpr (ex_ls) ->   *)
  (* | FunExpr (params, ty_opy, ex) -> *)
  | _ -> raise (TypeError "Wrong/ Unimplemented")


let rec binding_constraints (b : binding) (env : env) : env * constraints = match b with
  | LetBind (id, params, ty_opt, ex) ->
    (* collect constraints for the expression *)
    (* - handle params ... 
       - generalize?
       I'm not sure what I'm doing here *)
    let (ex_ty, ex_c) = expr_constraints ex env in
    let let_constraint = 
      (match ty_opt with
      | Some t -> [(t, ex_ty)]
      | None -> []) in
    let out_env = (id, generalize ex_ty env) :: env in
    (out_env, let_constraint @ ex_c)
  (* | LetRecBind (id, params, ty_opt, ex) ->
    (* handle recursion by introducing a type var for name being defined *)
    let rec_var = fresh_var_ty() in
    let rec_env = (id, rec_var) :: env in *)
  (* type definitions add a new name to the env for each constructor *)
  (* | TypeBind (id, constructors) ->  *)
  | _ -> raise (TypeError "Error/ Unimplemented")

let rec program_constraints (p : program) (env : env) (c : constraints) = match p with
  (* collect constraints in each binding *)
  | b :: rest -> 
    let (env_b, c_b) = binding_constraints b env in 
    program_constraints rest env_b c_b
  (* finished collecting constraints --> unify *)
  | [] -> 
    let s = unify c [] in 
    (* Apply resulting substitution s to all types in env bindings 
    for each (string * typ) pair in env, apply (string * subst (typ)(s)) *)
    let out_env = List.map (fun (id, ty) -> (id, subst (ty)(s))) env in
    out_env

(* Return environment with type bindings if successful *)
let type_check (p : program) : env =
  program_constraints p [] [] 