open OUnit2
open Ocaml_lite.Parser

let parse_tests = "test suite for parser" >::: [
  
    (* "id" >::
    (fun _ -> assert_equal ~printer:print_lc_expr
        (ELambda ("x", IntTy, (EVar "x")))
        (parse "&x : int.x"));
    "application" >::
    (fun _ -> assert_equal ~printer:print_lc_expr
        (EApp (ELambda ("x", BoolTy, (EVar "x")), EVar "y"))
        (parse "(&x : bool.x) y"));
    "apply inside lambda" >::
    (fun _ -> assert_equal ~printer:print_lc_expr
        (ELambda ("x", UnitTy, EApp (EVar "x", EVar "y")))
        (parse "&x : unit.x y"));
    "naming" >::
    (fun _ -> assert_equal ~printer:print_lc_expr
        (EApp (ELambda ("fun", FuncTy (UnitTy, FuncTy (IntTy, BoolTy)), (EApp (EVar "fun", EVar "v"))),
               ELambda ("x", UnitTy, ELambda ("y", FuncTy (IntTy, BoolTy), EVar "x"))))
        (parse "fun = &x : unit. &y : int -> bool. x; fun v")); *)
  ]