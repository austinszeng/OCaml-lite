open OUnit2
open Ocaml_lite.Ast
open Ocaml_lite.Interpreter

(* TO-DO: Rewrite all tests to be independent of parser and typechecker *)
let interp_tests = "test suite for the interpreter" >::: [
    "Int value" >::
    (fun _ ->
        assert_equal ~printer:print_value
            (interp ([LetBind ("t", [], Some(IntTy), IntExpr 2)])) 
            VInt (2));

    "Bool value" >::
    (fun _ ->
        assert_equal ~printer:print_value
            (interp (parse "true;;")) 
            VBool (true));

    "String value" >::
    (fun _ ->
        assert_equal ~printer:print_value
            (interp (parse "\"2\" ^ \"3i\";;")) 
            VStr ("23i"));

    "Unit value" >::
    (fun _ ->
    assert_equal ~printer:print_value
        (interp (parse "();;")) 
                VUnit);

    "Tuple value" >::
    (fun _ ->
    assert_equal ~printer:print_value
        (interp (parse "(5, \"55\");;")) 
                VTuple ([VInt (5); VStr ("55")]));

    "Bool binop comparison" >::
    (fun _ ->
        assert_equal ~printer:print_value
            (interp (parse "true || false;;")) 
            VBool (true));

    "\"Not\" unop expression" >::
    (fun _ ->
        assert_equal ~printer:print_value
            (interp (parse "not true;;")) 
            VBool (false));

    "Negate unop expression" >::
    (fun _ ->
    assert_equal ~printer:print_value
        (interp (parse "~2;;")) 
                VInt (-2));
                
    "Match expression" >::
    (fun _ ->
        assert_equal ~printer:print_value
            (interp (parse 
                "match 5 with
                | 5 => true
                | 6 => false;;")) 
            VBool (true));

    "If expression" >::
    (fun _ ->
    assert_equal ~printer:print_value
        (interp (parse "if true then 4 else 5;;")) 
                VInt (4));

    "plus" >::
    (fun _ -> 
    assert_equal ~printer:print_value
        (interp (parse ("1 + 2")))
        (VInt 3));

    "times" >::
    (fun _ -> assert_equal ~printer:print_value
        (interp (parse ("5 * 2")))
        (VInt 10));
]