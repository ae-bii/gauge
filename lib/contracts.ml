let extract_complexity_annotation (code : string) : string option =
  try
    let lines = String.split_on_char '\n' code in
    let rec find = function
      | [] -> None
      | l :: ls ->
        let i = try Some (String.index l '@') with _ -> None in
        (match i with
        | None -> find ls
        | Some idx ->
            let rest = String.sub l idx (String.length l - idx) in
            (* look for token after @complexity *)
            let parts = String.split_on_char ' ' rest in
            (match parts with
            | [] -> None
            | _ :: tok :: _ -> Some (String.trim tok)
            | _ -> None))
    in
    find lines
  with _ -> None
