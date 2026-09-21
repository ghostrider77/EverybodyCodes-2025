let () =
  let lines = In_channel.input_lines stdin in
  let a, b, c = Quest.parse_input lines in
  let result = Quest.calc_degree_of_similarity a b c in
  print_int result; print_newline ()
