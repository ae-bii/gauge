open Gauge

let () =
  match Array.to_list Sys.argv with
  | _ :: file :: _ ->
	  let code = Ast_utils.read_file file in
	  let declared = Contracts.extract_complexity_annotation code in
	  let inferred_infer = Infer.infer_of_string_code code in
	  let inferred_frontend = Frontend.infer_from_source ~filename:file code in
	  Printf.printf "infer: %s\n" (Cost_model.to_string inferred_infer);
	  Printf.printf "frontend: %s\n" (Cost_model.to_string inferred_frontend);
	  Report.report file inferred_infer declared
  | _ ->
	  Printf.printf "Usage: gauge <source-file>\n";
	  exit 1

