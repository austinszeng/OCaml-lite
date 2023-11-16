# Instructions
for building a Hindley-Milner type system (unification based type inference algorithm)

OCaml-lite has three built-in functions: `int_of_string`, `string_of_int`, and `print_string`. These are parsed just like regular functions, but they should be built into your type inference and interpretation algorithms, so they are available to all OCaml-lite programs even if they haven't been defined. The built-in functions have the following behavior.
- `int_of_string : int -> string` takes an integer and returns a string representing that integer.
- `string_of_int : string -> int` takes a string and returns an integer represented by that string. If the given string is not an integer, then this function throws an error.
- `print_string : string -> unit` takes a string and prints it to stdout (followed by a newline) as a side effect. This is equivalent to the OCaml function print_endline.

For reference, here are the type inference rules for Hindley-Milner:
```
(x, t) in G         G |- e1 : t1 -> t2    G |- e2 : t1
----------- (Var)   ---------------------------------- (App)
G |- x : t                   G |- e1 e2 : t2


{(x, t1)} U G |- e : t2          G |- e1 : s    {(x, s)} U G |- e2 : t
----------------------- (Abs)    ------------------------------------- (Let)
G |- & x . e : t1 -> t2                G |- let x = e1 in e2 : t


G |- e : forall a. s           G |- e : s    a not free in G
-------------------- (Inst)    ----------------------------- (Gen)
   G |- e : s[t/a]                  G |- e : forall a. s
```
Recall from class that we want to find the most general type for each expression, so we should apply the Gen rule as soon as possible and to as many type variables as possible.

Here are some (informal) typing rules for the various constructs we have in OCaml:

- A top-level let binding (`let id = e;;`) can be handled in exactly the same way as a let binding inside an expression.
- Recursive rules are handled by introducing a type variable for the name currently being defined, then typechecking as normal.
- Type definitions add a new name to the typing context for each constructor.
- User-provided type annotations are added as additional constraints.
- `if e1 then e2 else e3` requires that `e1 : bool` and that `e2` and `e3` have matching types.
- A tuple `(e1, e2, ..., en)` has type `t1 * t2 * ... * tn` where `e1 : t1, e2 : t2, ..., en : tn`.
- Binary operations have their expected type semantics. Specifically:
    - `+`, `-`, `*`, `/`, and `mod` all require both arguments to be `int` and the return value also has type `int`.
    -`<` requires both arguments to be `int` and returns `bool`.
    - `=` requires the types of the two arguments to match (but they can be of any type) and returns `bool`.
    - `^` requires both arguments to be `string` and returns `string`
    - `&&` and `||` require both arguments to be `bool` and return `bool`.
- Unary operations are also as expected: `not` expects a `bool` and returns a `bool`, and `~` expects an `int` and returns an `int`.
- Constants all of the appropriate type (`12 : int`, `"hi" : string`, `() : unit`, etc.)
- `match e with | p1 -> e1 | ... | pn -> en` requires that the types of `e1, e2, ..., en` all match, and that the types of `e, p1, p2, ..., pn` all match. The type of a pattern is the type returned by the associated constructor. Variables bound inside patterns need to be given types in the associated branch of the match expression, and those types need to match the types of the associated constructor
