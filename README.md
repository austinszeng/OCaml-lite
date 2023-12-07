# OCaml-lite

"Building an interpreter for a small but complete programming language. Specifically, ...a subset of OCaml dubbed OCaml-lite."

```Reed College's CSCI384 (Programming Language Design and Implementation) course "final project" spanning the last 6 weeks of the semester.```

Run executable with:
```dun exec ocaml_lite -- <filename>```

## Notes (11/23/23)
Typechecker is very unfinished. From referencing the notes and the structure of others' code, I think I at least have an outline of how it should look, but I struggled a lot in connecting everything together and fully understanding how to piece together all the pieces of the algorithm and forming a strong understanding of what it's doing. For example, generalization, implementation and application of inference rules, and more basic things like what to return and what helper functions I need. I think I understood the unification process enough from the notes to write the functions, but I was only able to write some parts of the constraint collection/ type inference functions. Overall, my current understanding of typechecking is weak and I need to spend more time to review the specifics of the language we're implementing.

## TO-DOs / Bugs
This section compiles feedback I've gotten on submissions. I'm not sure how much I can address since I also need to implement the interpreter during this last week...

### Parser
- [x] Implement `<expr> <expr>` branch (function application) of `<expr>` grammar 
- [ ] Write parser tests to test function application function (currently, it just doesn't break anything)
- [x] `fun` is not parsed correctly because `=>` is not recognized as a terminator for parameter lists
- [x] Write test case for `FunExpr`
- [ ] `fun` arguments (`param list`) not being captured for some reason...`parse_params_list` -> `parse_param` throws an error that the current token is `=>`
- [ ] `parse_let_expr` captures the name being bound in the var list...e.g., `let x = 1 in x` is parsed as `let x x 1 = x in x`
- [ ] `=>` is also missing in match branches
- [ ] Match branches can't parse multiple pattern variables
- [ ] Tuple types can't parse function types of multiple arguments e.g., `int -> int -> int`

### Typechecker
- [ ] Add built in functions
- [ ] Add parameters to the context when typechecking bindings
- [ ] Unification needs to proactively apply substitutions
- [ ] INstantiation should create a fresh variable for `id` then replace each instance of `id` in the quantified type with that new variable
- [ ] Occurs check is slightly wrong for quantified types: e.g., `t0` does appear free in `forall t1. t1 -> t0`...

### Interpreter
- [ ] `print_value` function in `ast.ml`
- [ ] Finish writing test ocaml-lite program `ocaml_p2.ol`