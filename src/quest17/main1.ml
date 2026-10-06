let () =
  let lines = In_channel.input_lines stdin in
  let grid = Quest.parse_input lines in
  let radius = 10.0 in
  let result = Quest.calc_sum_of_cells_within_radius grid radius in
  print_int result; print_newline ()
