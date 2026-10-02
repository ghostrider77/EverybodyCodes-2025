let () =
  let lines = In_channel.input_lines stdin in
  let ns = Quest.parse_input lines in
  let result = Quest.turn_dial ns 2025 in
  print_int result; print_newline ()
