bool : * := (A : *) -> A -> A -> A.

true : bool := \A x y. x.

false : bool := \A x y. y.

if : (A : *) -> bool -> A -> A -> A := \A b. b A.

b_neg : bool -> bool := \b. if bool b false true.

b_and : bool -> bool -> bool :=
    \a b. if bool a b false.

b_or : bool -> bool -> bool :=
    \a b. if bool a true b.

b_imp : bool -> bool -> bool :=
    \a b. if bool a b true.

