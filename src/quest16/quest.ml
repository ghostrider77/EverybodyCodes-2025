let parse_input lines =
  lines
    |> String.split_on_char ','
    |> List.map int_of_string


let calc_nr_of_blocks xs limit =
  List.fold_left (fun acc x -> acc + (limit / x)) 0 xs
