%token <string> ID
%token EOF
%token EVAL LOAD FORALL
%token LPAR RPAR COLON BACKSLASH COLONEQUAL DOT COMMA STAR ARROW

%{ open Concrete %}

%start <Concrete.command> command
%%

command:
    | LOAD; x = ID; DOT
        { Load x }
    | EVAL; e0 = expr0; COLON; e1 = expr0; DOT
        { Eval (e0, Some e1) }
    | EVAL; e0 = expr0; DOT
        { Eval (e0, None) }
    | x = ID; COLON; t = expr0; COLONEQUAL; e = expr0; DOT
        { Define (x, Some t, e) }
    | x = ID; COLONEQUAL; e = expr0; DOT
        { Define (x, None, e) }
    | x = ID; COLON; t = expr0; DOT
        { Constant (x, t) }
    | EOF 
        { Done }

expr0:
    | BACKSLASH; x = ID; COLON; a = expr0; DOT; e = expr0
        { Lam (x, a, e) }
    | BACKSLASH; bs = nonempty_list(binder); DOT; e = expr0
        { iterated_multi_lam bs e }
    | e = expr1
        { e }

expr1:
    | e0 = expr2; ARROW; e1 = expr1
        { Arrow (e0, e1) }
    | LPAR; x = ID; COLON; a0 = expr0; RPAR; ARROW; a1 = expr1
        { Pi (x, a0, a1) }
    | FORALL; bs = nonempty_list(binder); COMMA; a = expr1
        { iterated_multi_pi bs a }
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

binder :
    | LPAR; x = nonempty_list(ID); COLON; a = expr0; RPAR
        { (x, a) }

