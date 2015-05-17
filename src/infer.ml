

open Term

exception Error of string
let type_error msg = raise (Error msg)

let level = ref 0
let indent () = level := 1 + !level
let outdent () = level := !level - 1
let with_indent thunk =
    indent ();
    let t = thunk () in
    outdent ();
    t

let report delta tm =
    let pf = Printf.printf in
    pf "checking %*s%s\n" !level "" (raw_string tm)

let whnf_pi delta t = 
    match Eval.whnf delta t with
    | Pi (x, t0, t1) -> (x, t0, t1)
    | _ -> failwith "expected a pi type"

let rec infer delta gamma tm =
    try match tm with
    | Box -> type_error "# has no type"
    | Star -> Box
    | Var (x, ix, es) ->
        with_indent (fun () ->
            infer_app delta gamma es (nth gamma ix)
        )
    | Const (c, es) ->
        with_indent (fun () -> 
            infer_app delta gamma es (String_map.find c delta |> snd)
        )
    | Lam (x, t, e) ->
        with_indent (fun () ->
            let _ = infer_universe delta gamma t in
            Pi (x, t, infer delta (t :: gamma) e)
        )
    | Pi (x, t0, t1) ->
        with_indent (fun () ->
            let _ = infer_universe delta gamma t0 in
            infer_universe delta (t0 :: gamma) t1
        )
    | Redex (e0, es) ->
        with_indent (fun () ->
            let t = infer delta gamma e0 in
            infer_app delta gamma es t
        )
    with
    | Error msg ->
        Printf.sprintf "while checking %s.\n%s" (raw_string tm) msg
        |> type_error 

and infer_app delta gamma es a = match es with
    | [] ->
        a
    | e::es ->
        let t0' = infer delta gamma e in
        let (_, t0, t1) = whnf_pi delta a in
        Eval.equal delta t0 t0';
        infer_app delta gamma es (Term.subst 0 e t1)

and infer_universe delta gamma t = 
    match Eval.whnf delta (infer delta gamma t) with
    | Star -> Star
    | Box -> Box
    | _ -> failwith "expected a universe, got something else"

