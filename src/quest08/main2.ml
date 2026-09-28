let () =
  let line = read_line () in
  let nails = Quest.parse_input line in
  let result = Quest.calc_total_nr_intersections nails in
  print_int result; print_newline ()
