let () =
  let lines = In_channel.input_lines stdin in
  let flock = Quest.parse_input lines in
  let result = Quest.get_nr_rounds_to_balance_the_flock flock in
  print_int result; print_newline ()
