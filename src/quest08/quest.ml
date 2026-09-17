let parse_input line =
  line
    |> String.split_on_char ','
    |> List.map int_of_string


let calc_nr_times_thread_passes_through_center n nails =
  if n mod 2 = 1 then 0
  else
    let half_circle_size = n / 2 in
    let are_opposites a b =
      half_circle_size = abs (a - b) in
    let ns = List.to_seq nails in
    Seq.fold_left (fun acc (a, b) -> if are_opposites a b then acc + 1 else acc) 0 @@ Seq.zip (Seq.drop 1 ns) ns
