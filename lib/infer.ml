open Ppxlib
open Parsetree
open Asttypes

let seq_cost = Cost_model.seq_cost
let mul_cost = Cost_model.mul_cost
let o1 = Cost_model.o1
let on = Cost_model.on

let combine_seq costs = List.fold_left seq_cost o1 costs

let string_contains s sub =
  let len_s = String.length s and len_sub = String.length sub in
  if len_sub = 0 then true else
  let rec aux i =
    if i + len_sub > len_s then false
    else if String.sub s i len_sub = sub then true
    else aux (i+1)
  in aux 0

(* simple cost estimator that ignores interprocedural calls *)
let rec expr_cost (e : expression) : Cost_model.cost =
  let child_costs () =
    let cs = ref [] in
    let push c = cs := c :: !cs in
    let add_expr ex = push (expr_cost ex) in
    let add_option = function None -> () | Some ex -> add_expr ex in
    let add_list l = List.iter add_expr l in
    begin
      match e.pexp_desc with
      | Pexp_let (_, vbs, body) -> add_list (List.map (fun vb -> vb.pvb_expr) vbs); add_expr body
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
  | Pexp_for (_, _, _, _, body) | Pexp_while (body, _) -> mul_cost on (expr_cost body)
  | Pexp_apply (f, _args) ->
      (match f.pexp_desc with
      | Pexp_ident { txt = Longident.Ldot (Lident "List", _); _ } -> mul_cost on inner
      | _ -> inner)
  | _ -> inner

(* helper: detect if an expression contains an application to a given simple name *)
let contains_apply_to_name name (e : expression) : bool =
  let rec walk exp =
    match exp.pexp_desc with
    | Pexp_apply (f, args) ->
        (match f.pexp_desc with
        | Pexp_ident { txt = Longident.Lident n; _ } when n = name -> true
        | _ -> List.exists (fun (_, a) -> walk a) args || walk f)
    | Pexp_let (_, vbs, body) -> List.exists (fun vb -> walk vb.pvb_expr) vbs || walk body
    | Pexp_sequence (a,b) -> walk a || walk b
    | Pexp_tuple l -> List.exists walk l
    | Pexp_construct (_, Some ex) -> walk ex
    | Pexp_match (e0, cases) -> walk e0 || List.exists (fun c -> walk c.pc_rhs) cases
    | Pexp_try (e0, cases) -> walk e0 || List.exists (fun c -> walk c.pc_rhs) cases
    | Pexp_ifthenelse (cnd, t, fo) -> walk cnd || walk t || (match fo with None -> false | Some f -> walk f)
    | Pexp_for (_, _, _, _, body) -> walk body
    | Pexp_while (body, cond) -> walk body || walk cond
    | _ -> false
  in
  let ast_found = try walk e with _ -> false in
  if ast_found then true else
  let src = try Pprintast.string_of_expression e with _ -> "" in
  let pat1 = name ^ "(" in
  let pat2 = name ^ " " in
  string_contains src pat1 || string_contains src pat2

(* heuristic: extract simple names that follow the text "let rec " in the printed expression *)
let rec_extract_names (src : string) : string list =
  let len = String.length src in
  let rec aux i acc =
    try
      let j = String.index_from src i 'l' in
      if j + 8 < len && String.sub src j 8 = "let rec " then
        let k = j + 8 in
        let buf = Buffer.create 8 in
        let rec read pos =
          if pos < len then
            let ch = src.[pos] in
            if (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || (ch = '_' ) then (Buffer.add_char buf ch; read (pos+1))
            else (Buffer.contents buf, pos)
          else (Buffer.contents buf, pos)
        in
        let (nm, _) = read k in
        if nm = "" then aux (j+1) acc else aux (j+1) (nm::acc)
      else aux (j+1) acc
    with _ -> acc
  in aux 0 []

(* compute cost for a function body given an env lookup for callee costs; optional self_name
   marks a local helper/recursive name that should be treated as a self-call when invoked *)
let rec expr_cost_with_env_with (self_name : string option) (env : string -> Cost_model.cost option) (e : expression) : Cost_model.cost =
  let child_costs () =
    let cs = ref [] in
    let push c = cs := c :: !cs in
    let add_expr ex = push (expr_cost_with_env_with self_name env ex) in
    let add_option = function None -> () | Some ex -> add_expr ex in
    let add_list l = List.iter add_expr l in
    begin
      match e.pexp_desc with
    | Pexp_let (rec_flag, vbs, body) ->
      (match rec_flag with
      | Recursive ->
        (* collect simple var names from the recursive group *)
        let simple_names = List.fold_left (fun acc vb -> match vb.pvb_pat.ppat_desc with Ppat_var { txt = n; _ } -> n :: acc | _ -> acc) [] vbs in
        (* build a local env that conservatively maps group names to O(n) *)
        let local_env n = if List.exists (fun x -> x = n) simple_names then Some on else env n in
        (* evaluate each binding with the local env; if the binding contains calls to the group,
         treat it as multiplicative *)
        List.iter (fun vb ->
        match vb.pvb_pat.ppat_desc with
        | Ppat_var { txt = name; _ } ->
                    let c = expr_cost_with_env_with (Some name) local_env vb.pvb_expr in
                    push c
        | _ -> add_expr vb.pvb_expr
        ) vbs;
        (* evaluate body with the local env; if it calls any group name, treat as multiplicative *)
              let body_cost = expr_cost_with_env_with None local_env body in
              push body_cost
      | Nonrecursive -> add_list (List.map (fun vb -> vb.pvb_expr) vbs))
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
  | Pexp_for (_, _, _, _, body) | Pexp_while (body, _) -> mul_cost on (expr_cost_with_env_with self_name env body)
    | Pexp_apply (f, _args) ->
      (match f.pexp_desc with
      | Pexp_ident { txt = Longident.Lident name; _ } ->
      if Some name = self_name then mul_cost on inner else
          (match env name with
          | Some callee_cost -> seq_cost inner callee_cost
          | None -> inner)
      | Pexp_ident { txt = Longident.Ldot (Lident "List", _); _ } -> mul_cost on inner
      | _ -> inner)
  | _ -> inner

(* wrapper with no self_name *)
let expr_cost_with_env env e = expr_cost_with_env_with None env e

let infer_of_string_code source : Cost_model.cost =
  try
    let str = Frontend.parse_structure ~filename:"<input>" source in

    (* collect top-level functions: name -> (is_rec, expr) *)
    let functions : (string * bool * expression) list =
      List.fold_left (fun acc item ->
        match item.pstr_desc with
        | Pstr_value (rec_flag, vbs) ->
            let is_rec = (match rec_flag with Asttypes.Recursive -> true | _ -> false) in
            let add vb acc = match vb.pvb_pat.ppat_desc with
              | Ppat_var { txt = name; _ } -> (name, is_rec, vb.pvb_expr) :: acc
              | _ -> acc
            in List.fold_right add vbs acc
        | _ -> acc
      ) [] str in

    let lookup_of_map map name = try Some (List.assoc name map) with _ -> None in

    (* initial naive map: no interprocedural info (env returns None) *)
    let initial_map =
      List.map (fun (name, is_rec, expr) ->
        let expr_src = (try Pprintast.string_of_expression expr with _ -> "<?>") in
        let base = expr_cost_with_env (fun _ -> None) expr in
        let base = if is_rec then mul_cost on base else base in
        (* consider local `let rec` helpers heuristically *)
        let local_names = rec_extract_names expr_src in
        let c = List.fold_left (fun acc ln ->
          if contains_apply_to_name ln expr then
            let c_local = expr_cost_with_env_with (Some ln) (fun _ -> None) expr in
            Cost_model.max_cost acc (mul_cost on c_local)
          else acc
        ) base local_names in
        (name, c)
      ) functions in

    (* fixed-point iteration: update costs using env lookup from current map *)
    let rec iterate map iter =
      if iter <= 0 then map else
      let updated =
        List.map (fun (name, is_rec, expr) ->
          let env n = lookup_of_map map n in
          let expr_src = (try Pprintast.string_of_expression expr with _ -> "<?>") in
          let base = expr_cost_with_env env expr in
          let base = if is_rec then mul_cost on base else base in
          let local_names = rec_extract_names expr_src in
          let c = List.fold_left (fun acc ln ->
            if contains_apply_to_name ln expr then
              let c_local = expr_cost_with_env_with (Some ln) env expr in
              Cost_model.max_cost acc (mul_cost on c_local)
            else acc
          ) base local_names in
          (name, c)
        ) functions
      in
      if updated = map then map else iterate updated (iter - 1)
    in

    let final_map = iterate initial_map 10 in

    (* include top-level evaluation expressions as well *)
    let top_costs = List.fold_left (fun acc item ->
      match item.pstr_desc with
      | Pstr_eval (e, _) -> (expr_cost_with_env (lookup_of_map final_map) e) :: acc
      | _ -> acc
    ) [] str in

    let all_costs = (List.map snd final_map) @ top_costs in
    List.fold_left Cost_model.max_cost o1 all_costs
  with _ -> Cost_model.ounk
