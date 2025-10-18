open Gauge

let () =
	match Array.to_list Sys.argv with
	| _ :: file :: _ ->
			let code = Ast_utils.read_file file in
			let declared = Contracts.extract_complexity_annotations code in
			let inferred = Infer.infer_all_of_string_code code in
			(* Print a summary line for the whole file *)
			let overall = Infer.infer_of_string_code code in
			Printf.printf "overall: %s\n" (Cost_model.to_string overall);
			(* Report per-function *)
			let has_mismatch = Report.report_many inferred declared in
			if has_mismatch then exit 1
	| _ ->
			Printf.printf "Usage: gauge <source-file>\n";
			exit 1

