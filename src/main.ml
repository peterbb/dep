


type file = { 
    in_channel : in_channel;
    lexbuf : Lexing.lexbuf
}


let open_file filename =
    let open Lexing in
    let in_channel = open_in filename in
    let lexbuf = Lexing.from_channel in_channel in
    lexbuf.lex_curr_p <- {
        pos_fname = filename;
        pos_lnum = 0;
        pos_bol = 0;
        pos_cnum = 0
    };
    { in_channel; lexbuf }

let relative_open_file file filename =
    let open Lexing in
    let dir = Filename.dirname file.lexbuf.lex_curr_p.pos_fname in
    open_file (Filename.concat dir filename)

let close_file file = close_in file.in_channel

let read_command file =
    try Parser.command Lexer.read file.lexbuf with
    | Parser.Error ->
        let open Lexing in 
        let p = file.lexbuf.lex_curr_p in
        Printf.fprintf stderr "parse error in %s line %d at token %s\n"
           p.pos_fname p.pos_lnum (Lexing.lexeme file.lexbuf);
        exit (-1)

let check delta pe pt =
    let dom = String_map.dom delta in
    let t = Concrete.to_term dom pt in
    let e = Concrete.to_term dom pe in
    Check.check_type_or_kind delta [] t;
    Check.check delta [] e t;
    (e, t)
    
let rec toplevel file delta = let open Concrete in
    match read_command file with
    | Done -> delta
    | Define (x, pt, pe) ->
        (if String_map.mem x delta then
            "redefining constant " ^ x |> failwith);
        let e, t = check delta pe pt in
        toplevel file (String_map.add x (e, t) delta)
    | Eval (pe, pt) ->
        let e, t = check delta pe pt in
        Printf.printf "eval %s : %s ===>\t%s\n"
            (Term.raw_string e) (Term.raw_string t)
            (Eval.nf delta e |> Term.raw_string);
        toplevel file delta
    | Load x ->
        let f = relative_open_file file (x ^ ".d") in 
        let new_delta = toplevel f delta in
        close_file f;
        toplevel file new_delta

let main = function
    | [| progname; filepath |] ->
        Printf.printf "-- Interpreter started (%s). Loading %s.\n"
             progname filepath;
        let file = open_file filepath in
        begin try toplevel file String_map.empty with
        | Eval.Not_equal (e0, e1) -> 
            Printf.printf "type checking failed, expected %s and %s \
                           to be equal.\n"
                (Term.raw_string e0) (Term.raw_string e1);
            exit (-1)
        end;
        close_file file;
        Printf.printf "-- Bye\n"
    | _ ->
        Printf.printf "usage: %s filename\n" Sys.executable_name

let () = main Sys.argv

