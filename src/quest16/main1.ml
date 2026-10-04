let () =
  let line = read_line () in
  let ns = Quest.parse_input line in
  let result = Quest.calc_nr_of_blocks ns 90 in
  print_int result; print_newline ()
