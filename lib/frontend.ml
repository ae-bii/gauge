open Ppxlib

let parse_structure ~filename source =
  let lexbuf = Lexing.from_string source in
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
  Parse.implementation lexbuf

let contains_rec structure =
  let open Parsetree in
  List.exists (fun item ->
    match item.pstr_desc with
    | Pstr_value (Asttypes.Recursive, _bindings) -> true
    | _ -> false
  ) structure

let infer_from_source ~filename source =
  try
    let str = parse_structure ~filename source in
    if contains_rec str then Cost_model.on else Cost_model.o1
  with _ -> Cost_model.ounk
