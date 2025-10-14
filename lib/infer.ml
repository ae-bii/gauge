open Ppxlib
open Parsetree
open Asttypes

let seq_cost = Cost_model.seq_cost
let mul_cost = Cost_model.mul_cost
let o1 = Cost_model.o1
let on = Cost_model.on

let combine_seq costs = List.fold_left seq_cost o1 costs

let rec expr_cost (e : expression) : Cost_model.cost =
  let child_costs () =
    let cs = ref [] in
    let push c = cs := c :: !cs in
    let add_expr ex = push (expr_cost ex) in
    let add_option = function None -> () | Some ex -> add_expr ex in
    let add_list l = List.iter add_expr l in
    begin
      match e.pexp_desc with
      | Pexp_let (_, vbs, body) ->
          add_list (List.map (fun vb -> vb.pvb_expr) vbs); add_expr body
      | Pexp_sequence (e1, e2) -> add_expr e1; add_expr e2
      | Pexp_tuple l -> add_list l
      | Pexp_construct (_, c) -> (match c with None -> () | Some ex -> add_expr ex)
  | Pexp_apply (f, args) -> add_expr f; List.iter (fun (_, a) -> add_expr a) args
      | Pexp_match (e0, cases) -> add_expr e0; List.iter (fun c -> add_expr c.pc_rhs) cases
      | Pexp_try (e0, cases) -> add_expr e0; List.iter (fun c -> add_expr c.pc_rhs) cases
      | Pexp_ifthenelse (cnd, t, fo) -> add_expr cnd; add_expr t; add_option fo
      | Pexp_for (_, _, _, _, body) -> add_expr body
      | Pexp_while (body, cond) -> add_expr body; add_expr cond
      | _ -> ()
    end;
    !cs
  in
  let inner = combine_seq (child_costs ()) in
  match e.pexp_desc with
  | Pexp_for (_, _, _, _, body) | Pexp_while (body, _) ->
      let body_cost = expr_cost body in
      mul_cost on body_cost
  | Pexp_apply (f, _args) ->
      (* treat List.map, List.iter, List.fold_* as traversals (depth 1) *)
      (match f.pexp_desc with
      | Pexp_ident { txt = Longident.Ldot (Lident "List", _); _ } -> mul_cost on inner
      | _ -> inner)
  | _ -> inner

let structure_item_cost (item : structure_item) : Cost_model.cost =
  match item.pstr_desc with
  | Pstr_value (rec_flag, vbs) ->
      let binding_cost vb = expr_cost vb.pvb_expr in
      let max_bind = List.fold_left Cost_model.max_cost o1 (List.map binding_cost vbs) in
      (match rec_flag with
      | Recursive -> mul_cost on max_bind
      | Nonrecursive -> max_bind)
  | Pstr_eval (e, _) -> expr_cost e
  | _ -> o1

let infer_of_string_code source : Cost_model.cost =
  try
    let str = Frontend.parse_structure ~filename:"<input>" source in
    (* Debug: print parsed structure *)
    let () = () in

    (* collect top-level functions: name -> expr *)
    let functions : (string * bool * expression) list =
      List.fold_left (fun acc item ->
        match item.pstr_desc with
        | Pstr_value (rec_flag, vbs) ->
            let is_rec = (match rec_flag with Asttypes.Recursive -> true | _ -> false) in
            let add vb acc =
              match vb.pvb_pat.ppat_desc with
              | Ppat_var { txt = name; _ } -> (name, is_rec, vb.pvb_expr) :: acc
              | _ -> acc
            in
            List.fold_right add vbs acc
        | _ -> acc
        ) [] str in

    (* environment lookup by name *)
    let lookup_of_map map name =
      try Some (List.assoc name map) with _ -> None
    in

    (* compute cost for a function body given an env lookup for callee costs *)
  let rec expr_cost_with_env ?self_name env e =
      let child_costs () =
        let cs = ref [] in
        let push c = cs := c :: !cs in
        let add_expr ex = push (expr_cost_with_env env ex) in
        let add_option = function None -> () | Some ex -> add_expr ex in
        let add_list l = List.iter add_expr l in
        begin
          match e.pexp_desc with
          | Pexp_let (_, vbs, body) ->
              add_list (List.map (fun vb -> vb.pvb_expr) vbs); add_expr body
          | Pexp_sequence (e1, e2) -> add_expr e1; add_expr e2
          | Pexp_tuple l -> add_list l
          | Pexp_construct (_, c) -> (match c with None -> () | Some ex -> add_expr ex)
          | Pexp_apply (f, args) -> add_expr f; List.iter (fun (_, a) -> add_expr a) args
          | Pexp_match (e0, cases) -> add_expr e0; List.iter (fun c -> add_expr c.pc_rhs) cases
          | Pexp_try (e0, cases) -> add_expr e0; List.iter (fun c -> add_expr c.pc_rhs) cases
          | Pexp_ifthenelse (cnd, t, fo) -> add_expr cnd; add_expr t; add_option fo
          | Pexp_for (_, _, _, _, body) -> add_expr body
          | Pexp_while (body, cond) -> add_expr body; add_expr cond
          | _ -> ()
        end;
        !cs
      in
      let inner = combine_seq (child_costs ()) in
    match e.pexp_desc with
      | Pexp_for (_, _, _, _, body) | Pexp_while (body, _) ->
          let body_cost = expr_cost_with_env env body in
          mul_cost on body_cost
      | Pexp_apply (f, args) ->
        (match f.pexp_desc with
        | Pexp_ident { txt = Longident.Lident name; _ } ->
            let callee_src = (try Pprintast.string_of_expression f with _ -> "<?>") in
            let args_src =
              let strs = List.map (fun (_, a) -> try String.trim (Pprintast.string_of_expression a) with _ -> "<?>") args in
              String.concat "," strs
            in
            let lookup = match env name with Some c -> Cost_model.to_string c | None -> "(none)" in
            Printf.printf "[infer-debug] apply callee=%s name=%s self=%s args=[%s] env_lookup=%s\n"
              (String.trim callee_src) name (match self_name with None -> "(none)" | Some s -> s) args_src lookup;
            (* detect self call: treat as multiplicative by default *)
            if Some name = self_name then mul_cost on inner else
            (match env name with
             | Some callee_cost -> seq_cost inner callee_cost
             | None -> inner)
        | Pexp_ident { txt = Longident.Ldot (Lident "List", _); _ } ->
            let callee_src = (try Pprintast.string_of_expression f with _ -> "<?>") in
            Printf.printf "[infer-debug] apply List.* callee=%s args_count=%d\n" (String.trim callee_src) (List.length args);
            mul_cost on inner
        | _ ->
            let callee_src = (try Pprintast.string_of_expression f with _ -> "<?>") in
            Printf.printf "[infer-debug] apply other callee=%s\n" (String.trim callee_src);
            inner)
      | _ -> inner
    in
   (* compute initial map: naive local costs (treat calls as unknown -> inner)
     pass self_name so recursive calls can be recognized in following steps *)
    let initial_map = List.map (fun (name, is_rec, expr) ->
      let c = expr_cost_with_env (fun _ -> None) ?self_name:(if is_rec then Some name else None) expr in
      let c = if is_rec then mul_cost on c else c in
      (name, c)
    ) functions in

    (* iterate to fixed point: update each function cost using current map for lookup *)
    let rec iterate map iter =
      if iter <= 0 then map else
      let updated =
        List.map (fun (name, is_rec, expr) ->
          let env name = lookup_of_map map name in
          let cost = expr_cost_with_env env ?self_name:(if is_rec then Some name else None) expr in
          let cost = if is_rec then mul_cost on cost else cost in
          (name, cost)
        ) functions
      in
      if updated = map then map else iterate updated (iter - 1)
    in

  let final_map = iterate initial_map 10 in
  let _ = final_map in

    (* include top-level evaluation expressions as well *)
    let top_costs = List.fold_left (fun acc item ->
      match item.pstr_desc with
      | Pstr_eval (e, _) -> (expr_cost_with_env (lookup_of_map final_map) e) :: acc
      | _ -> acc
    ) [] str in

    (* combine costs from functions and top-level expressions *)
    let all_costs = (List.map snd final_map) @ top_costs in
    List.fold_left Cost_model.max_cost o1 all_costs
  with _ -> Cost_model.ounk
