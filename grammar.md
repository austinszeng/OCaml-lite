## Grammar
Note:
- `[ A ]` for some sequence of symbols A means that A is optional
- `[ A ]*` means that A may appear any number of times (including zero)
- `[ A ]+` means that A may appear any number of times but must appear at least once

```
<program> ::= [<binding> ;;]+

<binding> ::= let $id [<param>]* [: <type>] = <expr>
            | let rec $id [<param>]* [: <type>] = <expr>
            | type $id = ['|' $id [of <type>]]+

<param> ::= $id
          | ( $id : <type> )

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

Note: 
Handle function application <expr> <expr> in a different rule 
A ::= AB | B
where B are base expressions outside operator precedence hierarchy

<binop> ::= + | - | * | / | mod | < | = | ^ | && | ||

<unop> ::= not | ~

<type> ::= <type> -> <type>
         | ( <type> )
         | <type> * <type>
         | int
         | bool
         | string
         | unit
         | $id

<match_branch> ::= $id [<pattern_vars>] => <expr>

<pattern_vars> ::= $id
                 | ( $id [, $id ]+)
```



## Precedences

(Highest precedence)
- not
- ~
- *, /, mod
- +, -, ^
- <, =
- &&
- ||

(Lowest precedence)

Note that a few of these precedences are loose. For example, the string concatenation operator ^ can really be parsed at any precedence above = because the program will be ill-typed if an expression includes both ^ and any numerical operators. If you find it easier to modify the precedences above you're free to do so, as long as you preserve the parse trees of well-typed expressions.