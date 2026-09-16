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


let can_name_be_constructed name rules =
  let is_letter_pair_allowed a b =
    match CharMap.find_opt a rules with
      | None -> true
      | Some letters -> List.mem b letters in
  let chars = String.to_seq name in
  let pairs = Seq.zip chars (Seq.drop 1 chars) in
  Seq.for_all (fun (a, b) -> is_letter_pair_allowed a b) pairs


let find_name_that_complies_to_the_rules names rules =
  List.find (Fun.flip can_name_be_constructed rules) names


let get_index_sum_of_all_allowed_names names rules =
  names
    |> List.filter_mapi (fun ix name -> if can_name_be_constructed name rules then Some ix else None)
    |> List.fold_left (fun acc ix -> acc + (ix + 1)) 0
