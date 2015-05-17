%token <string> ID
%token EOF
%token EVAL LOAD
%token LPAR RPAR COLON BACKSLASH COLONEQUAL DOT STAR ARROW
%token BANG

%{ open Concrete %}

%start <Concrete.command> command
%%

command:
    | LOAD; x = ID; DOT
        { Load x }
    | EVAL; e0 = expr0; COLON; e1 = expr0; DOT
        { Eval (e0, e1) }
    | x = ID; COLON; t = expr0; COLONEQUAL; e = expr0; DOT
        { Define (x, t, e) }
    | EOF 
        { Done }

(*
  Ambigous grammar of expressions:
    e ::= id list(e) | * | \ list(id). e | (id : e) e | e -> e | (e)
  Unambiguated grammar:
    e0 ::= \ list(id) . e0 | e0
    e1 ::= e2 | e2 -> e1 | (list(id) : e1) e1
    e2 ::= e3 list(e3)
    e3 ::= id | * | ( e0 )
*)

expr0:
    | BACKSLASH; xs = nonempty_list(ID); DOT; e = expr0
        { let lam x e = Lam (x, e) in
          List.fold_right lam xs e }
    | e = expr1
        { e }

expr1:
    | e0 = expr2; ARROW; e1 = expr1
        { Arrow (e0, e1) }
    | BANG; xs = nonempty_list(ID); COLON; e0 = expr0; DOT; e1 = expr1
        { let pi x e = Pi (x, e0, e) in
          let fv = free e0 in
          let shadows x = StringSet.mem x fv in
          if List.exists shadows xs
          then failwith "simultaneous binders in pi type does not support \
                         shadowing variable in its type"
          else List.fold_right pi xs e1 }
    | e = expr2
        { e }

expr2:
    | e = expr3; es = list(expr3)
        { App (e, es) }

expr3:
    | x = ID
        { Var x }
    | STAR
        { Star }
    | LPAR; e = expr0; RPAR
        { e }

