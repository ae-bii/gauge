let extract_complexity_annotations (code : string) : (string * string) list =
  (* Returns a list of (name, token) where name is either a function name
     or the special "__file__" key when the annotation didn't name a binding.
     Accepted formats (in a comment line containing @complexity):
       @complexity O(n)
       @complexity foo: O(n)
       @complexity foo : O(n)
  *)
  let lines = String.split_on_char '\n' code in
  let parse_line l =
    try
      let idx = String.index l '@' in
      let rest = String.sub l idx (String.length l - idx) in
      let rest = String.trim rest in
      if String.length rest < 11 then None else
      let lower = String.sub rest 0 (min (String.length rest) 11) in
      if not (lower = "@complexity") then None else
      let after = String.trim (String.sub rest 11 (String.length rest - 11)) in
      if after = "" then None else
      (* If there is a ':' before the first '(' or 'O', treat left side as name. *)
      let find_split s =
        try Some (String.index s ':') with _ -> None
      in
      let sanitize tok =
        let tok = String.trim tok in
        (* If token contains an O(...) shape, extract that exact substring. *)
        try
          let start = String.index tok 'O' in
          let rest = String.sub tok start (String.length tok - start) in
          try
            let close = String.index rest ')' in
            String.sub rest 0 (close+1)
          with _ -> String.trim rest
        with _ ->
          (* otherwise take the first whitespace-separated token and strip trailing punctuation *)
          let parts = String.split_on_char ' ' tok in
          let raw = List.hd parts in
          let len = String.length raw in
          let rec trim_trail i =
            if i <= 0 then "" else
            let c = raw.[i-1] in
            if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_' || c = '(' || c = ')' then
              String.sub raw 0 i
            else trim_trail (i-1)
          in
          trim_trail len

      in
      match find_split after with
      | Some colon ->
          let name = String.sub after 0 colon |> String.trim in
          let tok_raw = String.sub after (colon+1) (String.length after - (colon+1)) |> String.trim in
          let tok = sanitize tok_raw in
          if tok = "" then None else Some (name, tok)
  | None -> Some ("__file__", sanitize after)
    with _ -> None
  in
  let rec loop acc = function
    | [] -> List.rev acc
    | l::ls -> (match parse_line l with Some a -> loop (a::acc) ls | None -> loop acc ls)
  in
  loop [] lines


let extract_complexity_annotation (code : string) : string option =
  (* Backwards-compatible: return the first unnamed or first file-level annotation. *)
  try
    let anns = extract_complexity_annotations code in
    match anns with
    | [] -> None
    | ("__file__", tok) :: _ -> Some tok
    | (_, tok) :: _ -> Some tok
  with _ -> None
