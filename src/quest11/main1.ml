let () =
  let lines = In_channel.input_lines stdin in
  let flock = Quest.parse_input lines in
  let nr_rounds = 10 in
  let result = Quest.flock_rearrangement flock nr_rounds in
  print_int result; print_newline ()
