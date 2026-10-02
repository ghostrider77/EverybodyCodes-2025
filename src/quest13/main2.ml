let () =
  let lines = In_channel.input_lines stdin in
  let rs = Quest.parse_ranges lines in
  let dial = Quest.create_dial_from_ranges rs in
  let result = Quest.turn_dial dial 20252025 in
  print_int result; print_newline ()
