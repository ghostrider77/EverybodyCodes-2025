let () =
  let line = read_line () in
  let nails = Quest.parse_input line in
  let nr_nails = 256 in
  let result = Quest.find_max_number_of_crossings nr_nails nails in
  print_int result; print_newline ()
