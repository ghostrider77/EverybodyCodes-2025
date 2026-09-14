module IntSet = Set.Make(Int)


let parse_input line =
  line
    |> String.split_on_char ','
    |> List.map int_of_string


let find_sum_of_unique_elements crates =
  IntSet.fold (+) (IntSet.of_list crates) 0
