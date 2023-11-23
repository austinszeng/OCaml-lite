open Ast

(* HM Inference Rules:

(x, t) in G         G |- e1 : t1 -> t2    G |- e2 : t1
----------- (Var)   ---------------------------------- (App)
G |- x : t                   G |- e1 e2 : t2


{(x, t1)} U G |- e : t2          G |- e1 : s    {(x, s)} U G |- e2 : t
----------------------- (Abs)    ------------------------------------- (Let)
G |- & x . e : t1 -> t2                G |- let x = e1 in e2 : t


G |- e : forall a. s           G |- e : s    a not free in G
-------------------- (Inst)    ----------------------------- (Gen)
   G |- e : s[t/a]                  G |- e : forall a. s
*)

(* store set of expr e to typ t bindings *)
type env = (string * typ) list



let rec type_check_binding (b : binding) =
  match b with

let type_check (p : program) =
  List.map type_check_binding program

(* STLC TYPING RULES: (for ref)

  (x, t) in G
  -----------
  G |- x : t
  
  G |- e1 : t1 -> t2    G |- e2 : t1
  ----------------------------------
           G |- e1 e2 : t2
  
     G U {(x, t1)} |- e : t2       (where U is set union)
  ------------------------------
  G |- (& x : t1 . e) : t1 -> t2
  
  ------------      ----------------      -----------------      --------------
  G |- i : int      G |- true : bool      G |- false : bool      G |- () : unit
  
  (* Gamma is a set of pairs: (name, type) *)
  type gamma_context = (string * typ) list
  let rec type_check (gam : gamma_context) (expr : lc_expr) : typ = 
    match expr with
    | EVar id -> (try List.assoc id gam with Not_found -> raise (TypeError ("Variable " ^ id ^ " not bound.")))
    | EApp (e1, e2) -> 
        let e1ty = type_check gam e1 in
        let e2ty = type_check gam e2 in 
        (match e1ty with 
          | FuncTy (t1, t2) when t1 = e2ty -> t2
          (* Need more descriptive error message ... *)
          | _ -> raise (TypeError "EApp error"))
    | ELambda (id, t1, e) -> FuncTy (t1, type_check ((id, t1) :: gam) e)
    | EInt _ -> IntTy
    | ETrue -> BoolTy
    | EFalse -> BoolTy
    | EUnit -> UnitTy
  
  
  let rec reduce ?(verbose : bool = false) (expr : lc_expr) : lc_expr =
    if irr expr
    then expr
    else
      let () = if verbose then print_endline (print_lc_expr expr) else () in
      reduce ~verbose:verbose (reduction_step expr)
*)