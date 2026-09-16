let () =
  let lines = In_channel.input_lines stdin in
  let names, rules = Quest.parse_input lines in
  let result = Quest.get_index_sum_of_all_allowed_names names rules in
  print_int result; print_newline ()
