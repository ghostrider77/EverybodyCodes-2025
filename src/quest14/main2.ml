let () =
  let lines = In_channel.input_lines stdin in
  let grid = Quest.parse_input lines in
  let nr_rounds = 2025 in
  let result = Quest.play_game grid nr_rounds in
  print_int result; print_newline ()
