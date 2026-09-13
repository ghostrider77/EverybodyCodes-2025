let () =
  let line = read_line () in
  let x = Quest.read_number line in
  let result = Quest.ComplexNumber.to_string @@ Quest.perform_cycle_k_times x 3 in
  print_endline result
