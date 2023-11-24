# HM Inference

## Notes:
- Gen introduces quantifiers
- Inst rules removes quantifiers by substituting a type var with a concrete (mono)type
- Let binding introduces a polytype
- Function abstraction introduces a monotype and cannot be polymorphic

## Rules:
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