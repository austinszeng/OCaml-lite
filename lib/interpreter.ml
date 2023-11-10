open Ast
open Parser

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
  | UnitVal -> "()"
  | TupleVal ls ->
    "(" ^ String.concat (", ") (List.map print_value ls) ^ ")"

(* refer to expressions hw *)
(* let rec interp (e : expr) : expr =
  ... *)