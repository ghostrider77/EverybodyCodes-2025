let () =
  let lines = In_channel.input_lines stdin in
  let dnas = Quest.parse_input lines in
  let result = Quest.calc_degree_of_similarity_sum dnas in
  print_int result; print_newline ()
