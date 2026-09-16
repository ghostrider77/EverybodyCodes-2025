module CharMap = Map.Make(Char)


let parse_input = function
  | first :: _ :: rest ->
      let names = String.split_on_char ',' first in
      let parse_line line =
        Scanf.sscanf line "%c > %s" (fun c cs -> (c, List.of_seq @@ Seq.filter ((<>) ',') @@ String.to_seq cs)) in
      let rules =
        List.fold_left (fun acc line -> let (k, ls) = parse_line line in CharMap.add k ls acc) CharMap.empty rest in
      (names, rules)
  | _ -> failwith "Malformed input."


let find_name_that_complies_to_the_rules names rules =
  let is_letter_pair_allowed a b =
    match CharMap.find_opt a rules with
      | None -> true
      | Some letters -> List.mem b letters in
  let can_name_be_constructed name =
    let chars = String.to_seq name in
    let pairs = Seq.zip chars (Seq.drop 1 chars) in
    Seq.for_all (fun (a, b) -> is_letter_pair_allowed a b) pairs in

  List.find can_name_be_constructed names
