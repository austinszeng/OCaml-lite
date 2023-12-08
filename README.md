# OCaml-lite

"Building an interpreter for a small but complete programming language. Specifically, ...a subset of OCaml dubbed OCaml-lite."

```Reed College's CSCI384 (Programming Language Design and Implementation) course "final project" spanning the last 6 weeks of the semester.```

## Notes (12/7/23)
On this submission, I have finished programming the interpreter. It passes all of the tests I wrote, but given more time I would write a lot more interesting tests for the edge cases. I did not have enough time during this last week of the project to address the typechecker feedback I received...so that portion of this project is still practically an outline, unfortunately. I attempted to address some of the feedback from the parsing portion of the project (marked below), but did not end up fully fixing the errors relating to `FunExpr` and `MatchExpr`.

## TO-DOs / Bugs
This section includes feedback I've gotten on submissions.

### Parser
- [x] Implement `<expr> <expr>` branch (function application) of `<expr>` grammar 
- [ ] Write parser tests to test function application function (currently, it just doesn't break anything)
- [x] `fun` is not parsed correctly because `=>` is not recognized as a terminator for parameter lists
- [x] Write test case for `FunExpr`
- [ ] `fun` arguments (`param list`) not being captured for some reason...`parse_params_list` -> `parse_param` throws an error that the current token is `=>`
- [ ] `parse_let_expr` captures the name being bound in the var list...e.g., `let x = 1 in x` is parsed as `let x x 1 = x in x`
- [x] `=>` is also missing in match branches
- [ ] Match branches can't parse multiple pattern variables
- [ ] Tuple types can't parse function types of multiple arguments e.g., `int -> int -> int`

### Typechecker
- [ ] Add built in functions
- [ ] Add parameters to the context when typechecking bindings
- [ ] Unification needs to proactively apply substitutions
- [ ] Instantiation should create a fresh variable for `id` then replace each instance of `id` in the quantified type with that new variable
- [ ] Occurs check is slightly wrong for quantified types: e.g., `t0` does appear free in `forall t1. t1 -> t0`...

### Interpreter
- [x] `print_value` function 
- [x] Finish writing test ocaml-lite program `ocaml_p2.ol`
- [ ] Format and write more tests

## Programming with OCaml-lite
I have written a program `ocaml_p2.ol` that I believe follows OCaml-lite syntax, but I have not attempted to run the executable because of the bugs still prevalent (especially in my incomplete typechecker).