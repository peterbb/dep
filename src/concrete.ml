

type t =
    | Star
    | Var   of string
    | App   of t * t list
    | Lam   of string * t
    | Pi    of string * t * t
    | Arrow of t * t

type command =
    | Done
    | Load of string
    | Eval of t * t
    | Define of string * t * t

let union_list xs = List.fold_left StringSet.union StringSet.empty xs

let rec free = function
    | Star -> StringSet.empty
    | Var x -> StringSet.singleton x
    | App (x, es) -> StringSet.union (free x) (list_free es)
    | Lam (x, e) -> StringSet.remove x (free e)
    | Pi (x, e0, e1) ->
        StringSet.union (free e0) (StringSet.remove x (free e1))
    | Arrow (e0, e1) -> StringSet.union (free e0) (free e1)
and list_free xs = union_list (List.map free xs)

let index x ys =
    let rec loop i = function
        | [] -> raise Not_found
        | y::ys -> if x = y then i else loop (1 + i) ys
    in loop 0 ys

let to_term delta = 
    let rec to_term gamma = function
    | Star -> Term.Star
    | Var x -> begin match index (Some x) gamma with
        | i -> Term.Var (x, i, [])
        | exception Not_found -> 
            if StringSet.mem x delta
            then Term.Const (x, [])
            else failwith ("free variable " ^ x)
        end
    | App (e, es) ->
        Term.Redex (to_term gamma e, List.map (to_term gamma) es)
    | Lam (x, e) ->
        Term.Lam (x, to_term (Some x::gamma) e)
    | Pi (x, e0, e1) ->
        Term.Pi (x, to_term gamma e0, to_term (Some x::gamma) e1)
    | Arrow (e0, e1) ->
        Term.Pi ("_", to_term gamma e0, to_term (None::gamma) e1)
    in to_term []
    

