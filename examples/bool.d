bool : * := (A : *) -> A -> A -> A.

true : bool := \(A : *) (x y : A). x.

false : bool := \(A : *) (x y : A). y.

if : (A : *) -> bool -> A -> A -> A := \(A : *) (b : bool). b A.

b_neg := \(b : bool).
    if bool b false true.

b_and := \(a b : bool).
    if bool a b false.

b_or :=
    \(a b : bool). if bool a true b.

b_imp :=
    \(a b : bool). if bool a b true.

