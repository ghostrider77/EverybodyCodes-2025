module CharMap = Map.Make(Char)
module StringSet = Set.Make(String)

type prefix_set = { length : int; prefixes : string list }


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
      | None -> false
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


let calc_possible_prefixes name rules lower upper =
  let get_or_else chr default =
    Option.fold ~none:default ~some:Fun.id (CharMap.find_opt chr rules) in

  let extend length prefix =
    let last_char = prefix.[length - 1] in
    let next_chars = get_or_else last_char [] in
    List.map (fun chr -> Printf.sprintf "%s%c" prefix chr) next_chars in

  let extend_all_prefix {length; prefixes} =
    let next_prefixes = List.concat_map (extend length) prefixes in
    {length = length + 1; prefixes = next_prefixes} in

  if not (can_name_be_constructed name rules) then StringSet.empty
  else
    {length = String.length name; prefixes = [name]}
      |> Seq.iterate extend_all_prefix
      |> Seq.drop_while (fun {length; _} -> length < lower)
      |> Seq.take_while (fun {length; _} -> length <= upper)
      |> Seq.fold_left (fun acc {prefixes; _} -> StringSet.add_seq (List.to_seq prefixes) acc) StringSet.empty


let calc_nr_of_unique_prefixes names rules lower upper =
  names
    |> List.fold_left (fun acc n -> StringSet.union acc (calc_possible_prefixes n rules lower upper)) StringSet.empty
    |> StringSet.cardinal
