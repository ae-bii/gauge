open Cost_model

let report name inferred declared_opt =
  let inferred_s = to_string inferred in
  let declared_s = match declared_opt with None -> "(none)" | Some s -> s in
  Printf.printf "%s: inferred=%s declared=%s\n" name inferred_s declared_s
