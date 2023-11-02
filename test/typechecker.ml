open OUnit2
open Ocaml_lite.Typechecker

let type_tests = "test suite for the typechecker" >::: [
  "Int expression" >::
  (fun _ -> 
    assert_equal ~printer:print_type
      (type_check (IntExpr (5))) 
      (IntTy));

  "Str expression" >::
  (fun _ -> 
    assert_equal ~printer:print_type 
      (type_check (StrExpr ("bbb"))) 
      (StrTy));

  "True expression" >::
  (fun _ -> 
    assert_equal ~printer:print_type 
      (type_check (TrueExpr)) 
      (BoolTy));

  "False expression" >::
  (fun _ -> 
    assert_equal ~printer:print_type 
      (type_check (FalseExpr)) 
      (BoolTy));

  "Unit expression" >::
  (fun _ -> 
    assert_equal ~printer:print_type 
      (type_check (UnitExpr)) 
      (UnitTy));

  "Let bind int type" >::
  (fun _ -> 
    let input ="let x : int = 5;;" in
    assert_equal ~printer:print_type
      (type_check (parse (input)) 
      (IntTy)));

  "Let bind pair type" >::
  (fun _ -> 
    let input = "let pair : string * bool = (\"s\", true);;" in
    assert_equal ~printer:print_type 
      (type_check (parse (input)) 
      (PairTy (string, bool))));

  "Let bind function type" >::
  (fun _ -> 
    let input = "let f : int -> int = fun x = x + 5;;" in
    assert_equal ~printer:print_type 
      (type_check (parse (input)) 
      (FuncTy (int, int))));

  "Let rec bind function type" >::
  (fun _ -> 
    let input = 
    "let rec factorial : int -> int = fun x ->
      if x <= 0 then 1
      else x * factorial (x - 1);;" in
    assert_equal ~printer:print_type 
      (type_check (parse (input)) 
      (FuncTy (int, int))));
  
  "Param" >::
  (fun _ -> 
    let input = VarTyParam ("x", BoolTy) in
    assert_equal ~printer:print_type 
      (type_check (input) 
      (BoolTy)));
]
