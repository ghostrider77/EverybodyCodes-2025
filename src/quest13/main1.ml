let () =
  let lines = In_channel.input_lines stdin in
  let ns = Quest.parse_input lines in
  let dial = Quest.create_dial_from_numbers ns in
  let result = Quest.turn_dial dial 2025 in
  print_int result; print_newline ()
