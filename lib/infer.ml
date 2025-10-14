open Ppxlib
open Parsetree
open Asttypes

let int_max a b = if a > b then a else b
let max_of_list xs = List.fold_left int_max 0 xs

let rec expr_loop_depth (e : expression) : int =
  let child_depths () =
    let depths = ref [] in
    let push d = depths := d :: !depths in
    let add_expr ex = push (expr_loop_depth ex) in
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
    !depths
  in
  let nested = max_of_list (child_depths ()) in
  match e.pexp_desc with
  | Pexp_for _ | Pexp_while _ -> 1 + nested
  | Pexp_apply (f, _args) ->
      (* treat List.map, List.iter, List.fold_* as traversals (depth 1) *)
      (match f.pexp_desc with
    | Pexp_ident { txt = Longident.Ldot (Lident "List", _); _ } -> int_max 1 nested
    | _ -> nested)
  | _ -> nested

let structure_item_depth (item : structure_item) : int =
  match item.pstr_desc with
  | Pstr_value (rec_flag, vbs) ->
      let binding_depth vb = expr_loop_depth vb.pvb_expr in
      let max_bind = max_of_list (List.map binding_depth vbs) in
      (match rec_flag with
      | Recursive -> int_max 1 max_bind
      | Nonrecursive -> max_bind)
  | Pstr_eval (e, _) -> expr_loop_depth e
  | _ -> 0

let infer_of_string_code source : Cost_model.cost =
  try
    let str = Frontend.parse_structure ~filename:"<input>" source in
    let depths = List.map structure_item_depth str in
    let max_depth = max_of_list depths in
    if max_depth >= 2 then Cost_model.on2
    else if max_depth = 1 then Cost_model.on
    else Cost_model.o1
  with _ -> Cost_model.ounk
