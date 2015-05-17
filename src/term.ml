
type t =
    | Box
    | Star
    | Var   of string * int * t list
    | Const of string * t list
    | Lam   of string * t * t
    | Pi    of string * t * t
    | Redex of t * t list

let shift diff =
    let rec shift ix = function
    | Box -> Box
    | Star -> Star
    | Var (y, iy, es) ->
        if iy >= ix then
            Var (y, iy + diff, shift_list ix es)
        else
            Var (y, iy, shift_list ix es)
    | Lam (y, e0, e1) ->
        Lam (y, shift ix e0, shift (1 + ix) e1)
    | Pi (y, e0, e1) ->
        Pi (y, shift ix e0, shift (1 + ix) e1)
    | Redex (e, es) ->
        Redex (shift ix e, shift_list ix es)
    | Const (c, es) ->
        Const (c, shift_list ix es)

    and shift_list ix ns = List.map (shift ix) ns
    in shift 0

let down = shift (-1)
let up_by n = shift n
let up = up_by 1

let rec subst ix n m = 
    match m with
    | Box -> Box
    | Star -> Star
    | Var (y, iy, es) ->
        if iy = ix then
            Redex (n, subst_list ix n es)
        else if iy > ix then
            Var (y, iy - 1, subst_list ix n es)
        else
            Var (y, iy, subst_list ix n es)
    | Const (c, es) ->
        Const (c, subst_list ix n es)
    | Redex (e, es) ->
        Redex (subst ix n e, subst_list ix n es)
    | Lam (y, e0, e1) ->
        Lam (y, subst ix n e0, subst (1 + ix) (up n) e1)
    | Pi (x, e0, e1) ->
        Pi (x, subst ix n e0, subst (1 + ix) (up n) e1)

and subst_list ix n ms = List.map (subst ix n) ms

let nth gamma ix = 
    up_by (ix + 1) (List.nth gamma ix)

let rec raw_string = function
    | Box -> "#"
    | Star -> "*"
    | Var (x, ix, es) ->
        Printf.sprintf "%s[%d]%s" x ix (raw_string_list es)
    | Const (c, es) ->
        c ^ raw_string_list es
    | Lam (x, e0, e1) ->
        Printf.sprintf "(\\%s : %s. %s)" x (raw_string e0) (raw_string e1)
    | Pi (x, e0, e1) ->
        Printf.sprintf "(!%s : %s. %s)" x (raw_string e0) (raw_string e1)
    | Redex (e, es) ->
        Printf.sprintf "((%s) @ %s)" (raw_string e) (raw_string_list es)
    
and raw_string_list = function
    | [] -> ""
    | [x] -> "(" ^ raw_string x ^ ")"
    | (x::xs) -> " (" ^ raw_string x ^ ")" ^ raw_string_list xs


