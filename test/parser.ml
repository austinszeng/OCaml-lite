open OUnit2
open Ocaml_lite.Ast
open Ocaml_lite.Parser

(* Helper function to make every test input a binding*)
let wrap_input code =
    "let t = (" ^ code ^ ");;"

(* Helper function to parse expressions as a program*)
let wrap_expr expr =
    (* let t = <expr>*)
    [LetBind ("t", [], None, expr)]

let parse_tests = "test suite for parser" >::: [
    "Let bind without type explicitly assigned" >::
    (fun _ -> 
        let input = "let x = 1;;" in
        let expected = [LetBind ("x", [], None, IntExpr 1)] in 
        assert_equal ~printer:print_program 
            (parse input)
            expected);
    
    "Let bind with bool type" >::
    (fun _ ->
        let input = "let cond : bool = true;;" in
        let expected = [LetBind ("cond", [], Some(BoolTy), TrueExpr)] in
        assert_equal ~printer:print_program
            (parse input) 
            expected);

    "Type bind" >::
    (fun _ ->
        let input = 
        "type VarOrVarInt =
            | Var of string
            | VarInt of string * int
            | Error ;;" in
        let expected = [TypeBind ("VarOrVarInt", 
            [("Var", Some(StrTy)); 
            ("VarInt", Some(TupleTy ([StrTy; IntTy]))); 
            ("Error", None)])] in
        assert_equal ~printer:print_program 
            (parse input) 
            expected);

    "Lt Binary operator expression" >::
    (fun _ ->
        let input = wrap_input("11 < 9") in
        let expected = wrap_expr(BinopExpr (IntExpr(11), LtComp, IntExpr(9))) in
        assert_equal ~printer:print_program
            (parse input)
            expected);    

    "Concat Binary operator expression" >::
    (fun _ ->
        let input = wrap_input("\"Jeff\" ^ \"rey\"") in
        let expected = wrap_expr(BinopExpr (StrExpr("Jeff"), Concatenate, StrExpr("rey"))) in
        assert_equal ~printer:print_program
            (parse input) 
            expected); 

    "Unary not comparison expression" >::
    (fun _ ->
        let input = wrap_input("not true") in
        let expected = wrap_expr(UnopExpr (NotComp, TrueExpr)) in
        assert_equal ~printer:print_program
            (parse input) 
            expected); 

    "Unary negate expression" >::
    (fun _ ->
        let input = wrap_input("~100") in
        let expected = wrap_expr(UnopExpr (Neg, IntExpr (100))) in
        assert_equal ~printer:print_program 
            (parse input)
            expected);

    (* "Let expression" >::
    (fun _ -> 
        let input = wrap_input("let y : int = 5 in (10 + y)") in
        let expected = wrap_expr(LetExpr ("y", [], Some(IntTy), IntExpr(5), BinopExpr (IntExpr(10), Add, IdExpr("y")))) in 
        assert_equal ~printer:print_program
            (parse input) 
            expected); *)

    "Match expression" >::
    (fun _ ->
        let input = wrap_input(
            "match x with
                | x => true
                | y => false ") in
        let expected = wrap_expr(MatchExpr (IdExpr ("x"), 
            [MatchBr ("x", None, TrueExpr);
            MatchBr ("y", None, FalseExpr)])) in
        assert_equal ~printer:print_program
            (parse input) 
            expected);

    "Associativity" >::
    (fun _ ->
        let input = wrap_input("1 + 2 + 3") in
        let expected = wrap_expr(BinopExpr (BinopExpr(IntExpr(1), Add, IntExpr(2)), Add, IntExpr(3))) in
        assert_equal ~printer:print_program
            (parse input) 
            expected);

    "Parentheses" >::
    (fun _ ->
        let input = wrap_input("1 + 2 + (3 + 4)") in
        let expected = wrap_expr(BinopExpr (BinopExpr(IntExpr(1), Add, IntExpr(2)), Add, BinopExpr(IntExpr(3), Add, IntExpr(4)))) in
        assert_equal ~printer:print_program 
            (parse input) 
            expected);

    "Precedence" >::
    (fun _ ->
        let input = wrap_input("1 * 2 + 3 * 4") in
        let expected = wrap_expr(BinopExpr (BinopExpr(IntExpr(1), Mult, IntExpr(2)), Add, BinopExpr(IntExpr(3), Mult, IntExpr(4)))) in
        assert_equal ~printer:print_program 
            (parse input) 
            expected);

    "Hanging operator" >::
    (fun _ -> try
        let _ = parse ("1 + 3 +") in
        assert_failure "'1 + 3 +' passed the parser"
    with
    | ParseError _ -> assert_bool "" true
    | _ -> assert_failure "Unexpected error");

    "Leading operator" >::
    (fun _ -> try
        let _ = parse ("* 1 + 3") in
        assert_failure "'* 1 + 3' passed the parser"
        with
        | ParseError _ -> assert_bool "" true
        | _ -> assert_failure "Unexpected error");

]