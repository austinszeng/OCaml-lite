open Lexer
open Ast

exception ParseError of string

(* Annotated grammar

<program> ::= [<binding> ;;]+

List of bindings separated by ;;.

<binding> ::= let $id [<param>]* [: <type>] = <expr>
            | let rec $id [<param>]* [: <type>] = <expr>
            | type $id = ['|' $id [of <type>]]+

A binding can either be a value binding or a type binding. In either case, these 
look almost exactly like OCaml, but type definitions require a | before the first constructor.

<param> ::= $id
          | ( $id : <type> )

Parameters are exactly like OCaml--they are either a name by itself, or a name associated with a type.

<expr> ::= let $id [<param>]* [: <type>] = <expr> in <expr>
         | let rec $id [<param>]* [: <type>] = <expr> in <expr>
         | if <expr> then <expr> else <expr>
         | fun [<param>]+ [: <type>] => <expr>
         | <expr> <expr>
         | ( <expr> [, <expr>]+ )
         | <expr> <binop> <expr>
         | <unop> <expr>
         | ( <expr> )
         | $int
         | true
         | false
         | $string
         | $id
         | ( )
         | match <expr> with ['|' <match_branch>]+

<binop> ::= + | - | * | / | mod | < | = | ^ | && | ||

<unop> ::= not | ~

Expressions in OCaml-lite are a subset of OCaml expressions with a few small tweaks.
However, We use ~ rather than - for unary minus to make parsing easier and
and we use => in anonymous function definitions.

<type> ::= <type> -> <type>
         | ( <type> )
         | <type> * <type>
         | int
         | bool
         | string
         | unit
         | $id

Types are exactly like OCaml except that we can't parse type variables (like 'a).

<match_branch> ::= $id [<pattern_vars>] => <expr>

<pattern_vars> ::= $id
                 | ( $id [, $id ]+)

Pattern matching is a bit more restricted than in OCaml. In particular, we do not allow nested patterns and
each pattern consists of exactly a single constructor followed by names for that constructor's data. 
Note that match also uses => instead of -> for consistency with fun.*)


