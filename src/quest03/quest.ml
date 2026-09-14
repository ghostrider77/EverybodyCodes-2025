module IntMap = Map.Make(Int)
module IntSet = Set.Make(Int)


let parse_input line =
  line
    |> String.split_on_char ','
    |> List.map int_of_string


let find_sum_of_unique_elements crates =
  IntSet.fold (+) (IntSet.of_list crates) 0


let find_sum_of_smallest_set_of_given_size crates k =
  List.(crates |> sort_uniq Stdlib.compare |> take k |> fold_left (+) 0)


let find_maximum_occurrence crates =
  let increment = function
    | None -> Some 1
    | Some v -> Some (v + 1) in
  let occurrences = List.fold_left (fun acc n -> IntMap.update n increment acc) IntMap.empty crates in
  IntMap.fold (fun _ count acc -> max count acc) occurrences 0
