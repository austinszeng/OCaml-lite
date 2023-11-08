open OUnit2
open Ocaml_lite.Interpreter

let interp_tests = "test suite for the interpreter" >::: [
    "Int value" >::
    (fun _ ->
        assert_equal ~printer:print_value
            (interp (parse "2;;")) 
            IntVal (2));

    "Bool value" >::
    (fun _ ->
        assert_equal ~printer:print_value
            (interp (parse "true;;")) 
            BoolVal (true));

    "String value" >::
    (fun _ ->
        assert_equal ~printer:print_value
            (interp (parse "\"2\" ^ \"3i\";;")) 
            StrVal ("23i"));

    "Unit value" >::
    (fun _ ->
    assert_equal ~printer:print_value
        (interp (parse "();;")) 
                UnitVal);

    "Tuple value" >::
    (fun _ ->
    assert_equal ~printer:print_value
        (interp (parse "(5, \"55\");;")) 
                TupleVal ([IntVal (5); StrVal ("55")]));

    "Bool binop comparison" >::
    (fun _ ->
        assert_equal ~printer:print_value
            (interp (parse "true || false;;")) 
            BoolVal (true));

    "\"Not\" unop expression" >::
    (fun _ ->
        assert_equal ~printer:print_value
            (interp (parse "not true;;")) 
            BoolVal (false));

    "Negate unop expression" >::
    (fun _ ->
    assert_equal ~printer:print_value
        (interp (parse "~2;;")) 
                IntVal (-2));
                
    "Match expression" >::
    (fun _ ->
        assert_equal ~printer:print_value
            (interp (parse 
                "match 5 with
                | 5 => true
                | 6 => false;;")) 
            BoolVal (true));

    "If expression" >::
    (fun _ ->
    assert_equal ~printer:print_value
        (interp (parse "if true then 4 else 5;;")) 
                IntVal (4));

    "plus" >::
    (fun _ -> 
    assert_equal ~printer:print_value
        (interp (parse ("1 + 2")))
        (IntVal 3));

    "times" >::
    (fun _ -> assert_equal ~printer:print_value
        (interp (parse ("5 * 2")))
        (IntVal 10));
]