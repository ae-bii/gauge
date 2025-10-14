open Cost_model

let report name inferred declared_opt =
  let inferred_s = to_string inferred in
  match declared_opt with
  | None -> Printf.printf "%s: inferred=%s declared=%s\n" name inferred_s "(none)"
  | Some s ->
      (match of_string s with
      | None -> Printf.printf "%s: inferred=%s declared=(%s) [couldn't parse declared]\n" name inferred_s s
      | Some declared_cost ->
          let ok =
            (inferred.degree < 0 && declared_cost.degree < 0)
            || (inferred.degree = declared_cost.degree && inferred.log = declared_cost.log)
          in
          if ok then
            Printf.printf "%s: inferred=%s declared=%s [OK]\n" name inferred_s s
          else
            Printf.printf "%s: inferred=%s declared=%s [MISMATCH]\n" name inferred_s s)
