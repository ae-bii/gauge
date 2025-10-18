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

(* helper: count the number of direct calls to a given function name in an expression *)
let count_direct_calls_to_name name (e : expression) : int =
  let count = ref 0 in
  let rec walk exp =
    match exp.pexp_desc with
    | Pexp_apply (f, args) ->
        (match f.pexp_desc with
        | Pexp_ident { txt = Longident.Lident n; _ } when n = name -> 
            incr count;
            List.iter (fun (_, a) -> walk a) args
        | _ -> walk f; List.iter (fun (_, a) -> walk a) args)
    | Pexp_let (_, vbs, body) -> List.iter (fun vb -> walk vb.pvb_expr) vbs; walk body
    | Pexp_sequence (a,b) -> walk a; walk b
    | Pexp_tuple l -> List.iter walk l
    | Pexp_construct (_, Some ex) -> walk ex
    | Pexp_match (e0, cases) -> walk e0; List.iter (fun c -> walk c.pc_rhs) cases
    | Pexp_try (e0, cases) -> walk e0; List.iter (fun c -> walk c.pc_rhs) cases
    | Pexp_ifthenelse (cnd, t, fo) -> walk cnd; walk t; (match fo with None -> () | Some f -> walk f)
    | Pexp_for (_, _, _, _, body) -> walk body
    | Pexp_while (body, cond) -> walk body; walk cond
    | Pexp_function (_, _, function_body) ->
        (match function_body with
        | Pfunction_cases (cases, _, _) -> List.iter (fun c -> walk c.pc_rhs) cases
        | Pfunction_body body_expr -> walk body_expr)
    | _ -> ()
  in
  (try walk e with _ -> ()); !count

let collect_local_rec_names (e : expression) : string list =
  let acc = ref [] in
  let rec walk exp =
    match exp.pexp_desc with
    | Pexp_let (rec_flag, vbs, body) ->
        (match rec_flag with
        | Recursive ->
            List.iter (fun vb -> match vb.pvb_pat.ppat_desc with
              | Ppat_var { txt = name; _ } -> acc := name :: !acc
              | _ -> ()) vbs;
            List.iter (fun vb -> walk vb.pvb_expr) vbs;
            walk body
        | Nonrecursive -> List.iter (fun vb -> walk vb.pvb_expr) vbs; walk body)
    | Pexp_sequence (a,b) -> walk a; walk b
    | Pexp_apply (f, args) -> walk f; List.iter (fun (_, a) -> walk a) args
    | Pexp_tuple l -> List.iter walk l
    | Pexp_construct (_, Some ex) -> walk ex
    | Pexp_match (e0, cases) -> walk e0; List.iter (fun c -> walk c.pc_rhs) cases
    | Pexp_try (e0, cases) -> walk e0; List.iter (fun c -> walk c.pc_rhs) cases
    | Pexp_ifthenelse (cnd, t, fo) -> walk cnd; walk t; (match fo with None -> () | Some f -> walk f)
    | Pexp_for (_, _, _, _, body) -> walk body
    | Pexp_while (body, cond) -> walk body; walk cond
    | _ -> ()
  in
  (try walk e with _ -> ()); !acc

(* printed-expression fallback: find occurrences of "let rec NAME" in the printed expression *)
let rec_extract_names_from_string (src : string) : string list =
  let res = ref [] in
  let len = String.length src in
  let is_ident_char c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c = '_' ) || (c >= '0' && c <= '9') in
  let rec aux i =
    if i >= len then () else
    try
      let j = String.index_from src i 'l' in
      if j + 8 < len && String.sub src j 8 = "let rec " then
        let k = j + 8 in
        let buf = Buffer.create 8 in
        let rec read pos =
          if pos < len then
            let ch = src.[pos] in
            if is_ident_char ch then (Buffer.add_char buf ch; read (pos+1))
            else (Buffer.contents buf, pos)
          else (Buffer.contents buf, pos)
        in
        let (nm, _) = read k in
        if nm <> "" then res := nm :: !res;
        aux (j+1)
      else aux (j+1)
    with _ -> ()
  in aux 0; !res

(* find the pvb_expr for a simple var-binding named `name` inside expression `e` *)
let find_local_binding_expr (name : string) (e : expression) : expression option =
  let found = ref None in
  let rec walk exp =
    match exp.pexp_desc with
    | Pexp_let (_rec_flag, vbs, body) ->
        List.iter (fun vb -> match vb.pvb_pat.ppat_desc with
          | Ppat_var { txt = n; _ } when n = name -> found := Some vb.pvb_expr
          | _ -> (walk vb.pvb_expr)) vbs;
        walk body
    | Pexp_sequence (a,b) -> walk a; walk b
    | Pexp_apply (f, args) -> walk f; List.iter (fun (_, a) -> walk a) args
    | Pexp_tuple l -> List.iter walk l
    | Pexp_construct (_, Some ex) -> walk ex
    | Pexp_match (e0, cases) -> walk e0; List.iter (fun c -> walk c.pc_rhs) cases
    | Pexp_try (e0, cases) -> walk e0; List.iter (fun c -> walk c.pc_rhs) cases
    | Pexp_ifthenelse (cnd, t, fo) -> walk cnd; walk t; (match fo with None -> () | Some f -> walk f)
    | Pexp_for (_, _, _, _, body) -> walk body
    | Pexp_while (body, cond) -> walk body; walk cond
    | _ -> ()
  in
  (try walk e with _ -> ()); !found

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
      | Pexp_function (_, _, function_body) ->
          (* handle function/lambda: extract cases from function_body *)
          (match function_body with
          | Pfunction_cases (cases, _, _) -> List.iter (fun c -> add_expr c.pc_rhs) cases
          | Pfunction_body body_expr -> add_expr body_expr)
      | _ -> ()
    end;
    !cs
  in
  let inner = combine_seq (child_costs ()) in
  match e.pexp_desc with
  | Pexp_for (_, _, _, _, body) | Pexp_while (body, _) -> mul_cost on (expr_cost_with_env_with self_name env body)
  | Pexp_apply (f, args) ->
      (match f.pexp_desc with
      | Pexp_ident { txt = Longident.Lident name; _ } ->
          (* Handle list operators *)
          if name = "@" then
            (* List append: O(n) in the length of left operand *)
            seq_cost inner on
          else if name = "::" then
            (* List cons: O(1) *)
            inner
          else if Some name = self_name then mul_cost on inner else
          (match env name with
          | Some callee_cost -> seq_cost inner callee_cost
          | None -> inner)
      | Pexp_ident { txt = Longident.Ldot (Lident "List", fn_name); _ } ->
          (* Handle specific List module functions with known complexity *)
          (match fn_name with
          (* O(1) operations *)
          | "hd" | "tl" | "cons" -> inner
          
          (* O(n) operations that don't take function arguments *)
          | "length" | "rev" | "flatten" | "sort" | "sort_uniq" ->
              seq_cost inner on
          
          (* O(n) operations that take a function argument - evaluate the function *)
          | "map" | "iter" | "filter" | "filter_map" | "find" | "find_opt" 
          | "find_map" | "exists" | "for_all" | "partition" ->
              let base = mul_cost on inner in
              let ast_local_names = collect_local_rec_names e in
              let arg_calls =
                List.fold_left (fun acc (_, a) ->
                  match a.pexp_desc with
                  | Pexp_ident { txt = Longident.Lident an; _ } ->
                      (* named function: look up in env or evaluate if local recursive helper *)
                      let acc =
                        match env an with
                        | Some c -> Cost_model.max_cost acc (mul_cost on c)
                        | None -> acc
                      in
                      if List.exists ((=) an) ast_local_names then
                        let c_local = match find_local_binding_expr an e with
                          | Some be -> expr_cost_with_env_with (Some an) (fun _ -> None) be
                          | None -> expr_cost_with_env_with (Some an) (fun _ -> None) e
                        in
                        Cost_model.max_cost acc (mul_cost on c_local)
                      else acc
                  | _ ->
                      (* any other expression (including lambdas): evaluate and multiply by O(n) *)
                      let arg_cost = match a.pexp_desc with
                        | Pexp_function (_, _, function_body) ->
                            (* for inline lambdas, extract and evaluate the body *)
                            (match function_body with
                            | Pfunction_cases (cases, _, _) -> 
                                let case_costs = List.map (fun c -> expr_cost_with_env_with None env c.pc_rhs) cases in
                                List.fold_left Cost_model.max_cost Cost_model.o1 case_costs
                            | Pfunction_body body_expr -> expr_cost_with_env_with None env body_expr)
                        | _ -> expr_cost_with_env_with None env a
                      in
                      Cost_model.max_cost acc (mul_cost on arg_cost)
                ) base args
              in arg_calls
          
          (* O(n^2) operations - typically involve nested iteration *)
          | "concat" | "concat_map" ->
              mul_cost on (mul_cost on inner)
          
          (* Fold operations: O(n) * cost of the accumulator function *)
          | "fold_left" | "fold_right" ->
              let base = mul_cost on inner in
              let ast_local_names = collect_local_rec_names e in
              let arg_calls =
                List.fold_left (fun acc (_, a) ->
                  match a.pexp_desc with
                  | Pexp_ident { txt = Longident.Lident an; _ } ->
                      let acc =
                        match env an with
                        | Some c -> Cost_model.max_cost acc (mul_cost on c)
                        | None -> acc
                      in
                      if List.exists ((=) an) ast_local_names then
                        let c_local = match find_local_binding_expr an e with
                          | Some be -> expr_cost_with_env_with (Some an) (fun _ -> None) be
                          | None -> expr_cost_with_env_with (Some an) (fun _ -> None) e
                        in
                        Cost_model.max_cost acc (mul_cost on c_local)
                      else acc
                  | _ ->
                      let arg_cost = match a.pexp_desc with
                        | Pexp_function (_, _, function_body) ->
                            (match function_body with
                            | Pfunction_cases (cases, _, _) -> 
                                let case_costs = List.map (fun c -> expr_cost_with_env_with None env c.pc_rhs) cases in
                                List.fold_left Cost_model.max_cost Cost_model.o1 case_costs
                            | Pfunction_body body_expr -> expr_cost_with_env_with None env body_expr)
                        | _ -> expr_cost_with_env_with None env a
                      in
                      Cost_model.max_cost acc (mul_cost on arg_cost)
                ) base args
              in arg_calls
          
          (* Default: treat as O(n) traversal *)
          | _ ->
              let base = mul_cost on inner in
              let ast_local_names = collect_local_rec_names e in
              let arg_calls =
                List.fold_left (fun acc (_, a) ->
              match a.pexp_desc with
              | Pexp_ident { txt = Longident.Lident an; _ } ->
                  (* named function: look up in env or evaluate if local recursive helper *)
                  let acc =
                    match env an with
                    | Some c -> Cost_model.max_cost acc (mul_cost on c)
                    | None -> acc
                  in
                  if List.exists ((=) an) ast_local_names then
                    let c_local = match find_local_binding_expr an e with
                      | Some be -> expr_cost_with_env_with (Some an) (fun _ -> None) be
                      | None -> expr_cost_with_env_with (Some an) (fun _ -> None) e
                    in
                    Cost_model.max_cost acc (mul_cost on c_local)
                  else acc
              | _ ->
                  (* any other expression (including lambdas): evaluate and multiply by O(n) *)
                  let arg_cost = match a.pexp_desc with
                    | Pexp_function (_, _, function_body) ->
                        (* for inline lambdas, extract and evaluate the body *)
                        (match function_body with
                        | Pfunction_cases (cases, _, _) -> 
                            let case_costs = List.map (fun c -> expr_cost_with_env_with None env c.pc_rhs) cases in
                            List.fold_left Cost_model.max_cost Cost_model.o1 case_costs
                        | Pfunction_body body_expr -> expr_cost_with_env_with None env body_expr)
                    | _ -> expr_cost_with_env_with None env a
                  in
                  Cost_model.max_cost acc (mul_cost on arg_cost)
            ) base args
          in arg_calls
          ) (* end match fn_name *)
      | _ -> inner)
  | _ -> inner

(* wrapper with no self_name *)
let expr_cost_with_env env e = expr_cost_with_env_with None env e

(* extract the body from a function expression (unwrap Pexp_function layers) *)
let rec unwrap_function_body (e : expression) : expression =
  match e.pexp_desc with
  | Pexp_function (_, _, function_body) ->
      (match function_body with
      | Pfunction_body body_expr -> unwrap_function_body body_expr
      | Pfunction_cases _ -> e)  (* Can't unwrap pattern-match functions *)
  | _ -> e

let infer_all_of_string_code source : (string * Cost_model.cost) list =
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
      ) [] str |> List.rev in

    let lookup_of_map map name = try Some (List.assoc name map) with _ -> None in

    (* initial naive map: no interprocedural info (env returns None) *)
    let initial_map =
      List.map (fun (name, is_rec, expr) ->
        (* For top-level functions, unwrap the fun -> wrappers to get the actual body *)
        let body = unwrap_function_body expr in
  let base = expr_cost_with_env (fun _ -> None) body in
        (* For recursive functions, multiply by O(n). If there are multiple 
           self-calls, multiply by an additional O(n) to approximate branching recursion. *)
        let base = 
          if is_rec then
            let self_call_count = count_direct_calls_to_name name body in
            if self_call_count >= 2 then
              (* Multiple recursive calls: approximate as O(n^2) *)
              mul_cost on (mul_cost on base)
            else
              (* Single recursive call: O(n) *)
              mul_cost on base
          else base
        in
        (* consider local `let rec` helpers heuristically *)
  let printed = (try Pprintast.string_of_expression expr with _ -> "") in
  let local_names =
    let ast_names = collect_local_rec_names expr in
    let str_names = rec_extract_names_from_string printed in
    (* merge and deduplicate: prefer ast_names order then str_names *)
    let tbl = Hashtbl.create 8 in
    List.iter (fun x -> Hashtbl.replace tbl x ()) ast_names;
    List.iter (fun x -> if not (Hashtbl.mem tbl x) then Hashtbl.replace tbl x ()) str_names;
    Hashtbl.fold (fun k _ acc -> k :: acc) tbl []
  in
        let c = List.fold_left (fun acc ln ->
          if contains_apply_to_name ln expr then
            let c_local = match find_local_binding_expr ln expr with
              | Some be -> expr_cost_with_env_with (Some ln) (fun _ -> None) be
              | None -> on
            in
            Cost_model.max_cost acc c_local
          else acc
        ) base local_names in
        (name, c)
      ) functions in

    (* fixed-point iteration: update costs using env lookup from current map *)
    let rec iterate map iter =
      if iter <= 0 then map else
      let updated =
        List.map (fun (name, _is_rec, expr) ->
          let env n = lookup_of_map map n in
          let body = unwrap_function_body expr in
          let base = expr_cost_with_env env body in
          (* don't multiply by O(n) during iteration - recursion is already accounted for in initial map *)
          let printed = (try Pprintast.string_of_expression expr with _ -> "") in
          let local_names =
            let ast_names = collect_local_rec_names expr in
            let str_names = rec_extract_names_from_string printed in
            let tbl = Hashtbl.create 8 in
            List.iter (fun x -> Hashtbl.replace tbl x ()) ast_names;
            List.iter (fun x -> if not (Hashtbl.mem tbl x) then Hashtbl.replace tbl x ()) str_names;
            Hashtbl.fold (fun k _ acc -> k :: acc) tbl []
          in
          let c = List.fold_left (fun acc ln ->
            if contains_apply_to_name ln expr then
              let c_local = match find_local_binding_expr ln expr with
                | Some be -> expr_cost_with_env_with (Some ln) env be
                | None -> on
              in
              Cost_model.max_cost acc c_local
            else acc
          ) base local_names in
          (name, c)
        ) functions
      in
      if updated = map then map else iterate updated (iter - 1)
    in

    let final_map = iterate initial_map 10 in

    (* include top-level evaluation expressions as well, name them __file__-N *)
    let top_costs =
      let counter = ref 0 in
      List.fold_left (fun acc item ->
        match item.pstr_desc with
        | Pstr_eval (e, _) ->
            let k = Printf.sprintf "__file__-%d" !counter in
            incr counter;
            (k, expr_cost_with_env (lookup_of_map final_map) e) :: acc
        | _ -> acc
      ) [] str in

    final_map @ top_costs
  with _ -> []

let infer_of_string_code source : Cost_model.cost =
  let all = infer_all_of_string_code source in
  if all = [] then Cost_model.ounk else
  List.fold_left (fun acc (_, c) -> Cost_model.max_cost acc c) Cost_model.o1 all
