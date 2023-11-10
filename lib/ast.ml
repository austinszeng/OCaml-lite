type program = binding list

and binding =
  | LetBind of string * param list * typ option * expr
  | LetRecBind of string * param list * typ option * expr
  | TypeBind of string * (string * typ option) list
  
and param = 
  | VarParam of string
  | VarTyParam of string * typ

and expr =
  | LetExpr of string * param list * typ option * expr * expr
  | LetRecExpr of string * param list * typ option * expr * expr
  | IfExpr of expr * expr * expr
  | FunExpr of param list * typ option * expr
  | AppExpr of expr * expr
  | TupleExpr of expr list
  | BinopExpr of expr * binop * expr
  | UnopExpr of unop * expr
  | IntExpr of int
  | TrueExpr 
  | FalseExpr
  | StrExpr of string
  | IdExpr of string
  | UnitExpr 
  | MatchExpr of expr * match_branch list

and binop =
  | Add | Subtract | Mult | Div | Modulo | LtComp | EqComp 
  | Concatenate | AndComp | OrComp

and unop =
  | NotComp | Neg

and typ =
  | IntTy
  | BoolTy
  | StrTy
  | UnitTy
  | FuncTy of typ * typ
  | TupleTy of typ list
  | UserTy of string

and match_branch = 
  | MatchBr of string * pattern_vars option * expr

and pattern_vars =
  | PatternVar of string
  | PatternMultiVars of string list;;


(* Print functions for testing *)
let rec print_program (p : program) : string = 
  let rec print_list (f) (ls : binding list) : string = 
    match ls with
      | [] -> ""
      | h::t -> f h ^ ";" ^ print_list f t
  in "[" ^ print_list print_binding p ^ "]"

and print_binding = function
  | LetBind (id, params, typ_opt, expr) ->
    "let " ^ id ^ " " ^ print_params params ^ print_type_opt typ_opt ^ " = " ^ print_expr expr ^ ";;\n"
  | LetRecBind (id, params, typ_opt, expr) ->
    "let rec " ^ id ^ " " ^ print_params params ^ print_type_opt typ_opt ^ " = " ^ print_expr expr ^ ";;\n"
  | TypeBind (id, type_defs) ->
    "type " ^ id ^ " = " ^  print_type_defs type_defs ^ ";;\n"

and print_type_opt = function
  | Some typ -> " : " ^ print_type typ
  | None -> ""

and print_type_defs type_defs =
  String.concat "\n| " (List.map (fun (id, ty_opt) ->
    match ty_opt with
    | Some ty -> id ^ " of " ^ print_type ty
    | None -> id) type_defs)

and print_params params = 
  String.concat " " (List.map print_param params)

and print_param = function
  | VarParam id -> id
  | VarTyParam (id, ty) -> "(" ^ id ^ " : " ^ print_type ty ^ ")"

and print_expr = function
  | LetExpr (id, params, typ_opt, e1, e2) ->
    "let " ^ id ^ " " ^ print_params params ^ print_type_opt typ_opt ^ " = " ^ print_expr e1 ^ " in " ^ print_expr e2
  | LetRecExpr (id, params, typ_opt, e1, e2) ->
    "let rec " ^ id ^ " " ^ print_params params ^ print_type_opt typ_opt ^ " = " ^ print_expr e1 ^ " in " ^ print_expr e2
  | IfExpr (cond, then_expr, else_expr) ->
    "if " ^ print_expr cond ^ " then " ^ print_expr then_expr ^ " else " ^ print_expr else_expr
  | FunExpr (params, typ_opt, e) ->
    "fun " ^ print_params params ^ print_type_opt typ_opt ^ " => " ^ print_expr e
  | AppExpr (e1, e2) -> print_expr e1 ^ " " ^ print_expr e2
  | TupleExpr expr_list -> "(" ^ String.concat ", " (List.map print_expr expr_list) ^ ")"
  | BinopExpr (e1, op, e2) -> print_expr e1 ^ " " ^ print_binop op ^ " " ^ print_expr e2
  | UnopExpr (op, e) -> print_unop op ^ " " ^ print_expr e
  | IntExpr i -> string_of_int i
  | TrueExpr -> "true"
  | FalseExpr -> "false"
  | StrExpr s -> s
  | IdExpr id -> id
  | UnitExpr -> "()"
  | MatchExpr (e, branches) ->
    "match " ^ print_expr e ^ " with\n" ^ print_match_branches branches

and print_binop = function
  | Add -> "+"
  | Subtract -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Modulo -> "mod"
  | LtComp -> "<"
  | EqComp -> "="
  | Concatenate -> "^"
  | AndComp -> "&&"
  | OrComp -> "||"

and print_unop = function
  | NotComp -> "not"
  | Neg -> "~"

and print_type = function
  | IntTy -> "int"
  | BoolTy -> "bool"
  | StrTy -> "string"
  | UnitTy -> "unit"
  | UserTy id -> id 
  | FuncTy (ty1, ty2) ->
    print_type ty1 ^ " -> " ^ print_type ty2
  | TupleTy ls ->
    "(" ^ String.concat (" * ") (List.map print_type ls) ^ ")"

and print_match_branches branches =
  String.concat ("\n") (List.map print_match_branch branches)

and print_match_branch (MatchBr(id, pattern_vars_opt, e)) =
  "| " ^ id ^ print_pattern_vars_opt pattern_vars_opt ^ " => " ^ print_expr e

and print_pattern_vars pattern_vars =
  match pattern_vars with
  | PatternVar id -> id
  | PatternMultiVars ids -> "(" ^ String.concat (", ") (ids) ^ ")"

and print_pattern_vars_opt = function
  | Some pattern_vars -> print_pattern_vars pattern_vars
  | None -> ""