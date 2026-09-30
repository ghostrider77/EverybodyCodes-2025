let () =
  let lines = In_channel.input_lines stdin in
  let flock = Quest.parse_input lines in
  let result = Quest.calc_nr_rounds_in_second_phase_only flock in
  print_int result; print_newline ()
