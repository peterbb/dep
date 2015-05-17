
open Term

let rec whnf delta tm = match tm with
    | Box
    | Star
    | Var (_, _, _) 
    | Lam (_, _, _)
    | Pi (_, _, _) -> 
        tm
    | Const (c, es) ->
        let (e, _) = String_map.find c delta in
        Redex (e, es) |> whnf delta
    | Redex (x, []) -> 
        whnf delta x
    | Redex (Var (x, ix, ns), ms) ->
        Var (x, ix, ns @ ms)
    | Redex (Const (c, ns), ms) ->
        Const (c, ns @ ms) |> whnf delta
    | Redex (Redex (n, ns), ms) ->
        Redex (n, ns @ ms) |> whnf delta
    | Redex (Lam (x, _, e), n :: ns) -> 
        Redex (subst 0 n e, ns) |> whnf delta 
    (* tm should be well-typed, so this should not happen *)
    | Redex (Box, _)
    | Redex (Star, _)
    | Redex (Pi (_, _, _), _) ->
        assert false

exception Not_equal of Term.t * Term.t

let rec equal delta tm0 tm1 =
    match whnf delta tm0, whnf delta tm1 with
    | Box, Box
    | Star, Star ->
        ()
    | Var (x, ix, ns), Var (y, iy, ms) ->
        begin if (ix <> iy) then
            raise (Not_equal (tm0, tm1))
        end;
        begin try List.iter2 (equal delta) ns ms with
        | Invalid_argument s -> raise (Not_equal (tm0, tm1))
        end
    | Lam (_, t0, e0), Lam (_, t1, e1) ->
        equal delta t0 t1;
        equal delta e0 e1
    | Pi (_, n0, n1), Pi (_, m0, m1) ->
        equal delta n0 m0;
        equal delta n1 m1
    | tm0, tm1 ->
        raise (Not_equal (tm0, tm1))

(* For now, repeated application of whnf *)
let rec nf delta tm = match whnf delta tm with
    | Box -> Box
    | Star -> Star
    | Var (x, ix, es) ->
        Var (x, ix, List.map (nf delta) es)
    | Lam (x, e0, e1) ->
        Lam (x, nf delta e0, nf delta e1)
    | Pi (x, e0, e1) ->
        Pi (x, nf delta e0, nf delta e1)
    
