let () =
  let lines = In_channel.input_lines stdin in
  let grid = Quest.parse_input lines in
  let result = Quest.calc_largest_destruction grid in
  print_int result; print_newline ()
