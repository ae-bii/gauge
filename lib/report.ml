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


let report_many inferred_map declared_map =
  (* inferred_map : (string * Cost_model.cost) list
     declared_map : (string * string) list
     Print per-function lines. If a declared annotation refers to a name not present,
     still print it with inferred=(none). *)
  let lookup_decl name = try Some (List.assoc name declared_map) with _ -> None in
  let printed = ref [] in
  List.iter (fun (name, cost) ->
    let declared = lookup_decl name in
    report name cost declared;
    printed := name :: !printed
  ) inferred_map;
  (* also print declarations that weren't matched to any inferred function *)
  List.iter (fun (dname, ds) -> if not (List.exists ((=) dname) !printed) then
    Printf.printf "%s: inferred=%s declared=%s [DECLARED-BUT-NOT-FOUND]\n" dname "(none)" ds
  ) declared_map
