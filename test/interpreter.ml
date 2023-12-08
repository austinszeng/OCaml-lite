open OUnit2
open Ocaml_lite.Ast
open Ocaml_lite.Interpreter

(* interp inputs should be a list of let bindings
   output is the environment, so need to check environment if id, value pair exists *)

(* Use id of bind to find associated value in env *)
let rec find_val id env = 
    match env with 
    | (env_id, value) :: rest -> if id = env_id then value else find_val id rest
    | [] -> assert_failure ("Could not find " ^ id ^ " in environment")

let interp_tests = "test suite for the interpreter" >::: [
    "Int value" >::
    (fun _ ->
        assert_equal ~printer:print_value
            (find_val ("t") (interp ([LetBind ("t", [], Some(IntTy), IntExpr 2)])))
            (VInt 2));

    "Bool value" >::
    (fun _ ->
        assert_equal ~printer:print_value
            (find_val ("t") (interp ([LetBind ("t", [], Some(BoolTy), TrueExpr)])))
            (VBool (true)));

    "String value" >::
    (fun _ ->
        assert_equal ~printer:print_value
            (find_val ("t") (interp ([LetBind ("t", [], Some(IntTy), BinopExpr(StrExpr "2", Concatenate, StrExpr "3i"))])))
            (VStr ("23i")));

    "Unit value" >::
    (fun _ ->
    assert_equal ~printer:print_value
        (find_val ("t") (interp ([LetBind ("t", [], Some(UnitTy), UnitExpr)])))
                (VUnit));

    "Tuple value" >::
    (fun _ ->
    assert_equal ~printer:print_value
        (find_val ("t") (interp ([LetBind ("t", [], Some(TupleTy([IntTy; StrTy])), TupleExpr([IntExpr 5; StrExpr "55"]))])))
                (VTuple ([VInt (5); VStr ("55")])));

    "Bool binop comparison" >::
    (fun _ ->
        assert_equal ~printer:print_value
        (find_val ("t") (interp ([LetBind ("t", [], Some(BoolTy), BinopExpr(TrueExpr, OrComp, FalseExpr))])))
            (VBool (true)));

    "\"Not\" unop expression" >::
    (fun _ ->
        assert_equal ~printer:print_value
        (find_val ("t") (interp ([LetBind ("t", [], Some(BoolTy), UnopExpr (NotComp, TrueExpr))])))
            (VBool (false)));

    "Negate unop expression" >::
    (fun _ ->
    assert_equal ~printer:print_value
    (find_val ("t") (interp ([LetBind ("t", [], Some(IntTy), UnopExpr (Neg, IntExpr 2))])))
                (VInt (-2)));

    "If expression" >::
    (fun _ ->
    assert_equal ~printer:print_value
        (find_val ("t") (interp ([LetBind ("t", [], Some(IntTy), IfExpr(TrueExpr, IntExpr 4, IntExpr 5))])))
                (VInt (4)));

    "plus" >::
    (fun _ -> 
    assert_equal ~printer:print_value
    (find_val ("t") (interp ([LetBind ("t", [], Some(IntTy), BinopExpr(IntExpr 1, Add, IntExpr 2))])))
        (VInt 3));

    "times" >::
    (fun _ -> assert_equal ~printer:print_value
    (find_val ("t") (interp ([LetBind ("t", [], Some(IntTy), BinopExpr(IntExpr 5, Mult, IntExpr 2))])))
        (VInt 10));
]