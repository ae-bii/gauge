open Gauge

let () =
  match Array.to_list Sys.argv with
  | _ :: file :: _ ->
	  let code = Ast_utils.read_file file in
	  let declared = Contracts.extract_complexity_annotation code in
	  let inferred = Infer.infer_of_string_code code in
	  Report.report file inferred declared
  | _ ->
	  Printf.printf "Usage: gauge <source-file>\n";
	  exit 1

