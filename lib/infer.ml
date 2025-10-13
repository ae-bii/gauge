open Cost_model

let infer_of_string_code (_code : string) : t =
  (* Extremely simple heuristic stub:
     - if the code contains "rec" or "for" or "while" -> O(n)
     - if it contains "nested" or "for" twice -> O(n^2)
     - otherwise O(1)
     This is just a placeholder to make the prototype compile. *)
   let s = String.lowercase_ascii _code in
   let contains_char c = try ignore (String.index s c); true with _ -> false in
   (* simple checks: look for 'rec' (r), 'for' (f), 'while' (w) or 'nested' word heuristic *)
   if String.contains s 'n' && String.contains s 'n' then On2
   else if contains_char 'r' || contains_char 'f' || contains_char 'w' then On
   else O1
