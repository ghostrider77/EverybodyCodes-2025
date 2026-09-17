let () =
  let lines = In_channel.input_lines stdin in
  let names, rules = Quest.parse_input lines in
  let result = Quest.calc_nr_of_unique_prefixes names rules 7 11 in
  print_int result; print_newline ()
