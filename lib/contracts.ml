let extract_complexity_annotation (_code : string) : string option =
  (* Stub: look for a substring like "@complexity O(n)" *)
  try
    let i = String.index _code '@' in
    let rest = String.sub _code i (String.length _code - i) in
    Some rest
  with _ -> None
