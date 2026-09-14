let () =
  let lines = In_channel.input_lines stdin in
  let wheels = Quest.parse_input lines in
  let result = Quest.calc_number_of_full_turns_of_first_gear wheels 10_000_000_000_000 in
  print_int result; print_newline ()
