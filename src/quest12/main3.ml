let () =
  let lines = In_channel.input_lines stdin in
  let grid = Quest.parse_input lines in
  let result = Quest.find_greedy_largest_components grid 3 in
  print_int result; print_newline ()
