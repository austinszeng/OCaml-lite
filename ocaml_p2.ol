(*
Example OCaml-lite program. This is a re-implementation of the functions from
for "OCaml Homework, Part 2". Some functions are not included because they used
List module functions.


TO-DO: Refer to 10/23 Notes to implement lists...
*)

type bstree =
  | Lf
  | Br of int * bstree * bstree ;;

let rec insert (t : bstree) (i : int) : bstree =
  match t with
  | Lf => Br (i, Lf, Lf)
  | Br (n, left, right) =>
    if i = n then t
    else if i < n then Br (n, insert left i, right)
    else Br (n, left, insert right i) ;;

type arith =
  | Plus of arith * arith
  | Times of arith * arith
  | Negate of arith
  | Var of string
  | Num of int ;;

(* My version had an error, so this is from solution *)
let rec without (t : bstree) (i : int) : bstree =
  let rec add_left nt tr =
    match tr with
    | Lf => nt
    | Br (n, l, r) => Br (n, add_left nt l, r) in
  match t with
  | Lf => Lf
  | Br (n, left, right) =>
    if i < n then Br (n, without left i, right)
    else if i > n then Br (n, left, without right i)
    else match left with
      | Lf => right
      | Br (m, l, r) => Br (m, l, add_left r right) ;;

(* Return in-order list of bst *)
let rec flatten (t : bstree) : int list =
  match t with 
  | Lf => []
  | Br (n, Lf, Lf) => [n]
  | Br (n, Lf, right) => [n] @ flatten right
  | Br (n, left, right) => flatten left @ [n] @ flatten right ;;

(* Replace Var = id with v*)
let rec subst (id : string) (v : arith) (expr : arith) : arith =
  match expr with 
  | Plus (e1, e2) => 
    let e1' = subst id v e1 in
    let e2' = subst id v e2 in
    Plus (e1', e2')
  | Times (e1, e2) => 
    let e1' = subst id v e1 in
    let e2' = subst id v e2 in
    Times (e1', e2')
  | Negate e1 => 
    let e1' = subst id v e1 in Negate (e1')
  | Num i => expr
  | Var s => if s = id then v else expr ;;

(* Print propositions *)
type prop =
| And of prop * prop
| Or of prop * prop
| Not of prop
| Sym of string ;;

let rec pprint (p : prop) : string =
  match p with 
  | And (p1, p2) => "(" ^ pprint p1 ^ " && " ^ pprint p2 ^ ")"
  | Or (p1, p2) => "(" ^ pprint p1 ^ " || " ^ pprint p2 ^ ")"
  | Not p1 => "(~" ^ pprint p1 ^ ")"
  | Sym s => "(" ^ s ^ ")" ;;
