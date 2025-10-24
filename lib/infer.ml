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

(* helper: detect if recursive calls operate on different parts of the data (tree-like)
   vs overlapping subproblems (fibonacci-like).
   Heuristic: 
   - Tree-like: recursive calls on different pattern-bound variables (e.g., rec left, rec right)
   - Fibonacci-like: recursive calls on transformations of same variable (e.g., rec (n-1), rec (n-2)) *)
let is_tree_like_recursion name (e : expression) : bool =
  (* Extract the base variable from a recursive call argument *)
  let rec get_base_var exp =
    match exp.pexp_desc with
    | Pexp_ident { txt = Longident.Lident v; _ } -> Some v
    | Pexp_apply (f, args) ->
        (match f.pexp_desc with
        | Pexp_ident { txt = Longident.Lident fn; _ } when fn = name ->
            (match args with
            | [] -> None
            | (_, first) :: _ -> get_base_var first)
        | _ ->
            (match args with
            | [] -> None
            | (_, first) :: _ -> get_base_var first))
    | _ -> None
  in
  (* Collect all recursive calls in the expression *)
  let rec collect_rec_calls exp =
    match exp.pexp_desc with
    | Pexp_apply (f, args) ->
        (match f.pexp_desc with
        | Pexp_ident { txt = Longident.Lident n; _ } when n = name ->
            [exp]  (* Found a recursive call *)
        | _ ->
            (* Not a recursive call, but continue searching in function and args *)
            collect_rec_calls f @ List.concat_map (fun (_, a) -> collect_rec_calls a) args)
    | Pexp_match (_, cases) -> List.concat_map (fun c -> collect_rec_calls c.pc_rhs) cases
    | Pexp_let (_, vbs, body) ->
        List.concat_map (fun vb -> collect_rec_calls vb.pvb_expr) vbs @ collect_rec_calls body
    | Pexp_ifthenelse (cnd, t, fo) ->
        collect_rec_calls cnd @ collect_rec_calls t @ (match fo with Some f -> collect_rec_calls f | None -> [])
    | Pexp_sequence (a, b) -> collect_rec_calls a @ collect_rec_calls b
    | Pexp_tuple exps -> List.concat_map collect_rec_calls exps
    | Pexp_construct (_, Some e) -> collect_rec_calls e
    | Pexp_function (_, _, fb) ->
        (match fb with
        | Pfunction_cases (cases, _, _) -> List.concat_map (fun c -> collect_rec_calls c.pc_rhs) cases
        | Pfunction_body body -> collect_rec_calls body)
    | _ -> []
  in
  (* Check if we have multiple recursive calls on different variables *)
  let rec_calls = collect_rec_calls e in
  let vars = List.filter_map get_base_var rec_calls in
  let unique_vars = List.sort_uniq String.compare vars in
  (* Tree-like if we have 2+ calls on 2+ different variables *)
  List.length rec_calls >= 2 && List.length unique_vars >= 2

(* helper: count the maximum number of self-calls along any single execution path *)
let count_max_calls_per_path name (e : expression) : int =
  let rec walk exp =
    match exp.pexp_desc with
    | Pexp_apply (f, args) ->
        (match f.pexp_desc with
        | Pexp_ident { txt = Longident.Lident n; _ } when n = name -> 
            1 + (List.fold_left (fun acc (_, a) -> max acc (walk a)) 0 args)
        | _ -> 
            let from_f = walk f in
            (* For function application arguments, ADD the counts because all args are evaluated.
               e.g., f (rec x) (rec y) has 2 recursive calls on the same path. *)
            let from_args = List.fold_left (fun acc (_, a) -> acc + walk a) 0 args in
            max from_f from_args)
    | Pexp_let (_, vbs, body) -> 
        let from_bindings = List.fold_left (fun acc vb -> max acc (walk vb.pvb_expr)) 0 vbs in
        let from_body = walk body in
        max from_bindings from_body
    | Pexp_sequence (a,b) -> walk a + walk b  (* Sequential: add them *)
    | Pexp_tuple l -> List.fold_left (fun acc e -> max acc (walk e)) 0 l
    | Pexp_construct (_, Some ex) -> walk ex
    | Pexp_match (e0, cases) -> 
        let from_e0 = walk e0 in
        let from_cases = List.fold_left (fun acc c -> max acc (walk c.pc_rhs)) 0 cases in
        from_e0 + from_cases
    | Pexp_try (e0, cases) -> 
        let from_e0 = walk e0 in
        let from_cases = List.fold_left (fun acc c -> max acc (walk c.pc_rhs)) 0 cases in
        max from_e0 from_cases
    | Pexp_ifthenelse (cnd, t, fo) -> 
        let from_cnd = walk cnd in
        let from_then = walk t in
        let from_else = match fo with None -> 0 | Some f -> walk f in
        from_cnd + (max from_then from_else)  (* Condition + max of branches *)
    | Pexp_for (_, _, _, _, body) -> walk body
    | Pexp_while (body, cond) -> max (walk body) (walk cond)
    | Pexp_function (_, _, function_body) ->
        (match function_body with
        | Pfunction_cases (cases, _, _) -> List.fold_left (fun acc c -> max acc (walk c.pc_rhs)) 0 cases
        | Pfunction_body body_expr -> walk body_expr)
    | _ -> 0
  in
  try walk e with _ -> 0

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
      | Nonrecursive ->
        (* Build environment tracking costs of non-recursive let bindings *)
        let binding_costs = ref [] in
        List.iter (fun vb ->
          match vb.pvb_pat.ppat_desc with
          | Ppat_var { txt = name; _ } ->
              let c = expr_cost_with_env_with None env vb.pvb_expr in
              binding_costs := (name, c) :: !binding_costs;
              push c
          | _ -> add_expr vb.pvb_expr
        ) vbs;
        (* Create extended environment including local bindings *)
        let extended_env n =
          try Some (List.assoc n !binding_costs)
          with Not_found -> env n
        in
        (* Evaluate body with extended environment *)
        let body_cost = expr_cost_with_env_with self_name extended_env body in
        push body_cost
      )
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
      | Pexp_field (e, _) -> add_expr e  (* Record field access: process the record expr *)
      | Pexp_setfield (e1, _, e2) -> add_expr e1; add_expr e2  (* Mutable field update *)
      | Pexp_record (fields, base) ->
          (* Record creation: process all field values *)
          List.iter (fun (_, e) -> add_expr e) fields;
          (match base with None -> () | Some e -> add_expr e)
      | _ -> ()
    end;
    !cs
  in
  let inner = combine_seq (child_costs ()) in
  match e.pexp_desc with
  | Pexp_for (_, _, _, _, body) | Pexp_while (body, _) -> mul_cost on (expr_cost_with_env_with self_name env body)
  (* Record field access is O(1) *)
  | Pexp_field (_, _) -> inner
  (* Mutable record field update is O(1) *)
  | Pexp_setfield (_, _, _) -> inner
  (* Record creation is O(1) per field, so O(1) overall for constant fields *)
  | Pexp_record (_, _) -> inner
  | Pexp_apply (f, args) ->
      (match f.pexp_desc with
      | Pexp_ident { txt = Longident.Lident name; _ } ->
          (* Handle ref operations *)
          if name = "!" then
            (* Dereference: O(1) *)
            inner
          else if name = ":=" then
            (* Assignment: O(1) *)
            inner
          else if name = "ref" then
            (* Ref creation: O(1) *)
            inner
          (* Handle list operators *)
          else if name = "@" then
            (* List append: O(n) in the length of left operand *)
            seq_cost inner on
          else if name = "::" then
            (* List cons: O(1) *)
            inner
          (* Handle string concatenation operator *)
          else if name = "^" then
            (* String concatenation: O(n) in the combined length *)
            seq_cost inner on
          else if Some name = self_name then mul_cost on inner else
          (match env name with
          | Some callee_cost -> seq_cost inner callee_cost
          | None -> inner)
      | Pexp_ident { txt = Longident.Ldot (Lident "String", fn_name); _ } ->
          (* Handle specific String module functions with known complexity *)
          (match fn_name with
          (* O(1) operations *)
          | "length" | "get" | "unsafe_get" -> inner
          
          (* O(n) operations - string traversal/copying *)
          | "make" | "init" | "sub" | "blit" | "concat" | "cat"
          | "iter" | "iteri" | "map" | "mapi" | "fold_left" | "fold_right"
          | "trim" | "escaped" | "uppercase_ascii" | "lowercase_ascii" 
          | "capitalize_ascii" | "uncapitalize_ascii" ->
              seq_cost inner on
          
          (* String comparison and search - O(n) in string length *)
          | "equal" | "compare" | "contains" | "starts_with" | "ends_with"
          | "index" | "index_opt" | "rindex" | "rindex_opt" 
          | "index_from" | "index_from_opt" | "rindex_from" | "rindex_from_opt" ->
              seq_cost inner on
          
          (* String split operations - O(n) *)
          | "split_on_char" ->
              seq_cost inner on
          
          (* Default: treat as O(n) *)
          | _ -> seq_cost inner on
          )
      | Pexp_ident { txt = Longident.Ldot (Lident "Bytes", fn_name); _ } ->
          (* Handle Bytes module - similar complexity to String *)
          (match fn_name with
          (* O(1) operations *)
          | "length" | "get" | "set" | "unsafe_get" | "unsafe_set" | "create" -> inner
          
          (* O(n) operations *)
          | "make" | "init" | "sub" | "blit" | "concat" | "cat"
          | "iter" | "iteri" | "map" | "mapi" | "fold_left" | "fold_right"
          | "trim" | "escaped" | "uppercase_ascii" | "lowercase_ascii" 
          | "capitalize_ascii" | "uncapitalize_ascii" | "extend" | "fill" ->
              seq_cost inner on
          
          (* Default: treat as O(n) *)
          | _ -> seq_cost inner on
          )
      | Pexp_ident { txt = Longident.Ldot (Lident "Array", fn_name); _ } ->
          (* Handle Array module - arrays have O(1) random access unlike lists *)
          (match fn_name with
          (* O(1) operations *)
          | "length" | "get" | "set" | "unsafe_get" | "unsafe_set" -> inner
          
          (* O(n) creation and copying operations *)
          | "make" | "init" | "create_float" | "make_matrix" 
          | "copy" | "append" | "sub" | "to_list" | "of_list" ->
              seq_cost inner on
          
          (* O(n) operations that take a function argument - evaluate the function *)
          | "map" | "mapi" | "iter" | "iteri" | "fold_left" | "fold_right"
          | "fold_left_map" | "exists" | "for_all" | "find" | "find_opt"
          | "find_map" | "mem" | "memq" ->
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
          
          (* O(n) in-place operations *)
          | "fill" | "blit" | "sort" | "stable_sort" | "fast_sort" ->
              seq_cost inner on
          
          (* O(n^2) concat operations *)
          | "concat" | "concat_map" ->
              mul_cost on (mul_cost on inner)
          
          (* Default: treat as O(n) *)
          | _ -> seq_cost inner on
          )
      | Pexp_ident { txt = Longident.Ldot (Lident "List", fn_name); _ } ->
          (* Handle specific List module functions with known complexity *)
          (match fn_name with
          (* O(1) operations *)
          | "hd" | "tl" | "cons" -> inner
          
          (* O(n) operations that don't take function arguments *)
          | "length" | "rev" | "flatten" | "sort" | "sort_uniq" 
          | "nth" | "nth_opt" | "init" | "append" ->
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
      | Pexp_ident { txt = Longident.Ldot (Lident "Hashtbl", fn_name); _ } ->
          (* Handle Hashtbl module - hash tables with O(1) average case operations *)
          (match fn_name with
          (* O(1) operations - core hash table operations *)
          | "create" | "clear" | "reset" | "length" 
          | "add" | "find" | "find_opt" | "find_all" | "mem"
          | "remove" | "replace" | "randomize" ->
              inner
          
          (* O(n) operations - iterate over all entries *)
          | "iter" | "fold" | "map" | "filter_map_inplace" | "to_seq" 
          | "to_seq_keys" | "to_seq_values" | "add_seq" | "replace_seq"
          | "of_seq" | "stats" ->
              let base = mul_cost on inner in
              (* Check if there's a function argument and evaluate it *)
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
          
          (* Default: treat as O(1) for hash table operations *)
          | _ -> inner
          )
      | Pexp_ident { txt = Longident.Ldot (Lident "Map", fn_name); _ } ->
          (* Handle Map module - balanced trees with O(log n) operations *)
          (match fn_name with
          (* O(1) operations *)
          | "is_empty" | "cardinal" -> inner
          
          (* O(log n) operations - tree operations *)
          | "empty" | "add" | "singleton" | "remove" | "find" | "find_opt"
          | "find_first" | "find_first_opt" | "find_last" | "find_last_opt"
          | "mem" | "min_binding" | "min_binding_opt" | "max_binding" 
          | "max_binding_opt" | "choose" | "choose_opt" | "split" ->
              seq_cost inner { degree = 0; log = 1 } (* O(log n) *)
          
          (* O(n) operations - iterate over all entries *)
          | "iter" | "fold" | "map" | "mapi" | "filter" | "filter_map"
          | "partition" | "bindings" | "to_seq" | "to_seq_from"
          | "add_seq" | "of_seq" | "equal" | "compare" | "for_all"
          | "exists" ->
              let base = mul_cost on inner in
              (* Check if there's a function argument and evaluate it *)
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
          
          (* O(n + m) operations - merge two maps *)
          | "union" | "merge" ->
              mul_cost on (mul_cost on inner)
          
          (* Default: treat as O(log n) for tree operations *)
          | _ -> seq_cost inner { degree = 0; log = 1 }
          )
      | Pexp_ident { txt = Longident.Ldot (Lident "Set", fn_name); _ } ->
          (* Handle Set module - balanced trees with O(log n) operations, similar to Map *)
          (match fn_name with
          (* O(1) operations *)
          | "is_empty" | "cardinal" -> inner
          
          (* O(log n) operations - tree operations *)
          | "empty" | "add" | "singleton" | "remove" | "mem" 
          | "min_elt" | "min_elt_opt" | "max_elt" | "max_elt_opt"
          | "choose" | "choose_opt" | "find" | "find_opt"
          | "find_first" | "find_first_opt" | "find_last" | "find_last_opt"
          | "split" ->
              seq_cost inner { degree = 0; log = 1 } (* O(log n) *)
          
          (* O(n) operations - iterate over all elements *)
          | "iter" | "fold" | "map" | "filter" | "filter_map" | "partition"
          | "elements" | "to_list" | "of_list" | "to_seq" | "to_seq_from"
          | "add_seq" | "of_seq" | "for_all" | "exists" | "equal" | "compare"
          | "subset" | "disjoint" ->
              let base = mul_cost on inner in
              (* Check if there's a function argument and evaluate it *)
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
          
          (* O(n + m) operations - combine two sets *)
          | "union" | "inter" | "diff" ->
              mul_cost on (mul_cost on inner)
          
          (* Default: treat as O(log n) for tree operations *)
          | _ -> seq_cost inner { degree = 0; log = 1 }
          )
      | Pexp_ident { txt = Longident.Ldot (Lident "Queue", fn_name); _ } ->
          (* Queue module - FIFO queue with amortized O(1) operations *)
          (match fn_name with
          (* O(1) amortized operations - core queue operations *)
          | "create" | "add" | "push" | "take" | "take_opt" | "pop" | "pop_opt"
          | "peek" | "peek_opt" | "top" | "top_opt" | "is_empty" 
          | "length" | "clear" ->
              inner
          
          (* O(n) operations - iterate over all elements *)
          | "iter" | "fold" | "transfer" | "to_seq" | "add_seq" | "of_seq" ->
              let base = mul_cost on inner in
              (* Check if there's a function argument and evaluate it *)
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
          
          (* Default: treat as O(1) for queue operations *)
          | _ -> inner
          )
      | Pexp_ident { txt = Longident.Ldot (Lident "Stack", fn_name); _ } ->
          (* Stack module - LIFO stack with O(1) operations *)
          (match fn_name with
          (* O(1) operations - core stack operations *)
          | "create" | "push" | "pop" | "pop_opt" | "top" | "top_opt"
          | "is_empty" | "length" | "clear" ->
              inner
          
          (* O(n) operations - iterate over all elements *)
          | "iter" | "fold" | "to_seq" | "of_seq" ->
              let base = mul_cost on inner in
              (* Check if there's a function argument and evaluate it *)
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
          
          (* Default: treat as O(1) for stack operations *)
          | _ -> inner
          )
      | Pexp_ident { txt = Longident.Ldot (Lident "Seq", fn_name); _ } ->
          (* Seq module - lazy sequences with deferred computation *)
          (match fn_name with
          (* O(1) operations - sequence constructors (lazy, not evaluated yet) *)
          | "empty" | "return" | "cons" | "singleton" ->
              inner
          
          (* O(1) operations - sequence inspection (first element only) *)
          | "is_empty" | "uncons" | "head" | "head_exn" ->
              inner
          
          (* O(n) operations - must consume/traverse entire sequence *)
          | "iter" | "fold_left" | "iteri" | "fold_lefti" 
          | "for_all" | "exists" | "find" | "find_opt" | "find_map"
          | "length" | "compare" | "equal" ->
              let base = mul_cost on inner in
              (* Check if there's a function argument and evaluate it *)
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
          
          (* O(1) lazy operations - create transformed sequence without evaluation *)
          | "map" | "mapi" | "filter" | "filter_map" | "scan"
          | "take" | "drop" | "take_while" | "drop_while"
          | "tail" | "group" | "memoize" | "once"
          | "transpose" | "zip" | "interleave" | "sorted_merge"
          | "product" | "concat_map" | "flat_map" | "cycle" ->
              inner
          
          (* O(n+m) operations - append/concat sequences (lazy) *)
          | "append" | "concat" ->
              inner
          
          (* O(n) conversions from eager data structures *)
          | "of_list" | "of_array" | "of_string" | "of_bytes" ->
              mul_cost on inner
          
          (* O(n) conversions to eager data structures (must consume sequence) *)
          | "to_list" | "uniq" | "sort" | "sort_uniq" ->
              mul_cost on inner
          
          (* Default: treat as O(1) for lazy sequence operations *)
          | _ -> inner
          )
      | Pexp_ident { txt = Longident.Ldot (Lident "Buffer", fn_name); _ } ->
          (* Buffer module - efficient string building with amortized O(1) append *)
          (match fn_name with
          (* O(1) operations - buffer management *)
          | "create" | "reset" | "clear" | "length" ->
              inner
          
          (* O(1) amortized operations - adding to buffer *)
          | "add_char" | "add_string" | "add_bytes" | "add_substring" | "add_subbytes"
          | "add_substitute" | "add_channel" ->
              inner
          
          (* O(n) operations - extracting content *)
          | "contents" | "to_bytes" | "sub" ->
              mul_cost on inner
          
          (* O(n) operations - buffer operations *)
          | "nth" | "truncate" ->
              inner  (* nth is O(1), truncate is O(1) *)
          
          (* Default: treat as O(1) for buffer operations *)
          | _ -> inner
          )
      
      | Pexp_ident { txt = Longident.Ldot (Lident "Option", fn_name); _ } ->
          (* Option module - O(1) operations on optional values *)
          (match fn_name with
          (* O(1) operations - basic accessors and constructors *)
          | "none" | "some" | "value" | "get" | "is_none" | "is_some" | "equal" | "compare" ->
              inner
          
          (* O(1) operations - transformations (wrapper around inner function) *)
          | "map" | "bind" | "join" | "iter" | "fold" ->
              inner  (* The cost is determined by the function passed in *)
          
          (* O(1) operations - conversion and combination *)
          | "to_list" | "to_seq" ->
              inner  (* Either empty or single element *)
          
          | "to_result" ->
              inner  (* Just wraps in Result *)
          
          (* Default: treat as O(1) *)
          | _ -> inner
          )
      
      | Pexp_ident { txt = Longident.Ldot (Lident "Result", fn_name); _ } ->
          (* Result module - O(1) operations on result values *)
          (match fn_name with
          (* O(1) operations - constructors and accessors *)
          | "ok" | "error" | "value" | "get_ok" | "get_error" | "is_ok" | "is_error" ->
              inner
          
          (* O(1) operations - transformations *)
          | "map" | "map_error" | "bind" | "join" | "iter" | "iter_error" | "fold" ->
              inner  (* The cost is determined by the function passed in *)
          
          (* O(1) operations - conversion *)
          | "to_option" | "to_list" | "to_seq" ->
              inner  (* Either empty or single element *)
          
          (* O(1) operations - equality and comparison *)
          | "equal" | "compare" ->
              inner
          
          (* Default: treat as O(1) *)
          | _ -> inner
          )
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
        (* For recursive functions, multiply by O(n). Distinguish between:
           1. Single recursive call per path: O(n) - linear recursion
           2. Multiple calls per path with disjoint work (tree-like): O(n) - each node visited once
           3. Multiple calls per path with overlapping work (fibonacci-like): O(n^k) - exponential *)
        let base = 
          if is_rec then
            (* Count max self-calls along any single execution path *)
            let max_calls_per_path = count_max_calls_per_path name body in
            if max_calls_per_path >= 2 then
              (* Multiple recursive calls per path: check if tree-like or fibonacci-like *)
              if is_tree_like_recursion name body then
                (* Tree-like: each node visited once despite multiple calls, so O(n) *)
                mul_cost on base
              else
                (* Fibonacci-like: overlapping subproblems, approximate as O(n^k) *)
                let rec power_mul base_cost k =
                  if k <= 1 then base_cost
                  else mul_cost on (power_mul base_cost (k-1))
                in
                power_mul on max_calls_per_path
            else
              (* Single recursive call per path: O(n) *)
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
