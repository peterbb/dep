
open Term

exception Error of string
let type_error msg = raise (Error msg)

let print_state gamma tm ty =
    let printf = Printf.printf in
    let len = List.length gamma in
    let print_var i ty = printf "  %d: %s\n" (len - i - 1) (Term.raw_string ty) in
    printf "\n";
    List.iteri print_var (List.rev gamma);
    printf "-----------------------------\n";
    printf "  %s  :: %s\n" (Term.raw_string tm) (Term.raw_string ty);
    printf "\n"

let rec check delta gamma tm ty =
    print_state gamma tm ty;
    match tm with
    | Box 
    | Redex (Box, []) ->
        type_error "sort not typable"
    | Star
    | Redex (Star, []) ->
        begin match Eval.whnf delta ty with
        | Box -> ()
        | _ -> type_error "expected some type/term, got *"
        end
    | Var (x, ix, ns) ->
        let t = Term.up_by (ix + 1) (List.nth gamma ix) in
        check_app delta gamma ns t ty
    | Const (c, ns) ->
        let (_, ty0) = String_map.find c delta in
        check_app delta gamma ns ty0 ty
    | Lam (_, t, e) ->
        begin match Eval.whnf delta ty with
        | Pi (_, t0, t1) ->
            Eval.equal delta t t0;
            check delta (t0::gamma) e t1
        | _ -> type_error "expcted something else, got lambda expressions"
        end
    | Pi (_, t0, t1)
    | Redex (Pi (_, t0, t1), []) ->
        let ty' = Eval.whnf delta ty in
        begin if (ty' <> Box && ty' <> Star) then
            type_error "expected something else, not pi expression"
        end;
        check_type_or_kind delta gamma t0;
        check delta (t0::gamma) t1 ty'
    | Redex (Var (x, ix, ns), ms) ->
        check delta gamma (Var (x, ix, ns @ ms)) ty
    | Redex (Const (c, ns), ms) ->
        check delta gamma (Const (c, ns @ ms)) ty
    | Redex (Redex (n, ns), ms) ->
        check delta gamma (Redex (n, ns @ ms)) ty
    | Redex (Lam (_, _,_), _) ->
        failwith "no type inference, can't check terms \
                  on the for (\\x.M)N"
    | Redex (Pi (_, _, _), _::_)
    | Redex (Star, _::_)
    | Redex (Box, _::_) ->
        type_error "illegal application"
        
and check_app delta gamma tms ty0 ty1 = match tms with
    | [] -> 
        begin try Eval.equal delta ty0 ty1 with
        | Eval.Not_equal (t0, t1) ->
            Printf.sprintf "expected %s, got term of type %s: \
                           difference %s and %s\n"
            (Term.raw_string ty0) (Term.raw_string ty1)
            (Term.raw_string t0) (Term.raw_string t1)
            |> type_error
        end
    | (t :: tms) -> begin match Eval.whnf delta ty0 with
        | Pi (_, ty00, ty01) ->
            check delta gamma t ty00;
            check_app delta gamma tms (subst 0 t ty01) ty1
        | _ -> failwith "expected pi type"
        end

and check_type_or_kind delta gamma tm =
    try check delta gamma tm Star with
    | Eval.Not_equal (_, _) 
    | Error _ -> check delta gamma tm Box
