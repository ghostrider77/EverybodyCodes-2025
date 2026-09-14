let () =
  let lines = In_channel.input_lines stdin in
  let wheels = Quest.parse_cogwheel_pairs lines in
  let result = Quest.calc_number_of_full_turns_of_last_gear_for_paired_wheels wheels 100 in
  print_int result; print_newline ()
