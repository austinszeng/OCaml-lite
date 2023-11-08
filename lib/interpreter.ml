open Ast
open Parser

(* and expr =
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
  | UnitExpr (* ( ) *)
  | MatchExpr of expr * match_branch list *)

type value =
  | IntVal of int
  | BoolVal of bool
  | StrVal of string
  | UnitVal
  | TupleVal of value list
  (* | FunVal of  *)

let rec print_value : value -> string = function
  | IntVal i -> string_of_int i
  | BoolVal b -> string_of_bool b
  | StrVal s -> s
  | UnitVal -> "( )"
  | TupleVal (v1 :: v2) -> print_value v1 ^ ", " ^ print_value v2

(* refer to expressions hw *)
(* let rec interp (e : expr) : expr =
  ... *)