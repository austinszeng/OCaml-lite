open OUnit2
open Ocaml_lite.Parser

let parse_tests = "test suite for parser" >::: [
    "Let bind without type explicitly assigned" >::
    (fun _ -> 
        let input = "let x = 1;;" in
        let expected = LetBind ("x", [], None, IntExpr 1) in 
        assert_equal ~printer:print_binding 
            (parse input) 
            expected);
    
    "Let bind with bool type" >::
    (fun _ ->
        let input = "let cond : bool = true;;" in
        let expected = LetBind ("cond", [], Some(BoolTy), TrueExpr) in
        assert_equal ~printer:print_binding
            (parse input) 
            expected);

    "Type bind" >::
    (fun _ ->
        let input = 
        "type VarOrVarInt =
            | Var of string
            | VarInt of string * int
            | Error ;;" in
        let expected = TypeBind ("VarOrVarInt", 
            [("Var", Some(StrTy)); 
            ("VarInt", Some(TupleTy ([StrTy; IntTy]))); 
            ("Error", None)]) in
        assert_equal ~printer:print_binding 
            (parse input) 
            expected);

    "Lt Binary operator expression" >::
    (fun _ ->
        let input = "11 < 9;;" in
        let expected = BinopExpr (IntExpr(11), LtComp, IntExpr(9)) in
        assert_equal ~printer:print_expr 
            (parse input) 
            expected);    

    "Concat Binary operator expression" >::
    (fun _ ->
        let input = "\"Jeff\" ^ \"rey\";;" in
        let expected = BinopExpr (StrExpr("Jeff"), Concatenate, StrExpr("rey")) in
        assert_equal ~printer:print_expr 
            (parse input) 
            expected); 

    "Unary not comparison expression" >::
    (fun _ ->
        let input = "not true;;" in
        let expected = UnopExpr (NotComp, TrueExpr) in
        assert_equal ~printer:print_expr 
            (parse input) 
            expected); 

    "Unary negate expression" >::
    (fun _ ->
        let input = "~100" in
        let expected = UnopExpr (Neg, IntExpr (100)) in
        assert_equal ~printer:print_expr 
            (parse input)
            expected);

    "Let expression" >::
    (fun _ -> 
        let input = "let y : int = 5 in (10 + y);;" in
        let expected = LetExpr ("y", [], Some(IntTy), IntExpr(5), BinopExpr (IntExpr(10), Add, IdExpr("y"))) in 
        assert_equal ~printer:print_expr 
            (parse input) 
            expected);

    "Match branch" >::
    (fun _ -> 
        let input = "x => 1;;" in
        let expected = MatchBr ("x", None, IntExpr (1)) in
        assert_equal ~printer:print_match_branch 
            (parse input) 
            expected);

    "Match expression" >::
    (fun _ ->
        let input = 
            "match x with
                | x => true
                | y => false ;;" in
        let expected = MatchExpr (IdExpr ("x"), 
            [MatchBr ("x", None, TrueExpr);
            MatchBr ("y", None, FalseExpr)]) in
        assert_equal ~printer:print_expr 
            (parse input) 
            expected);

    "Associativity" >::
    (fun _ ->
        let input = "1 + 2 + 3;;" in
        let expected = BinopExpr (BinopExpr(IntExpr(1), Add, IntExpr(2)), Add, IntExpr(3)) in
        assert_equal ~printer:print_expr 
            (parse input) 
            expected);

    "Parentheses" >::
    (fun _ ->
        let input = "1 + 2 + (3 + 4);;" in
        let expected = BinopExpr (BinopExpr(IntExpr(1), Add, IntExpr(2)), Add, BinopExpr(IntExpr(3), Add, IntExpr(4))) in
        assert_equal ~printer:print_expr 
            (parse input) 
            expected);

    "Precedence" >::
    (fun _ ->
        let input = "1 * 2 + 3 * 4;;" in
        let expected = BinopExpr (BinopExpr(IntExpr(1), Mult, IntExpr(2)), Add, BinopExpr(IntExpr(3), Mult, IntExpr(4))) in
        assert_equal ~printer:print_expr 
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