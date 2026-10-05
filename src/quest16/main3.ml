let () =
  let line = read_line () in
  let blocks = Quest.parse_input line in
  let available_blocks = 202520252025000 in
  let result = Quest.get_wall_length blocks available_blocks in
  print_int result; print_newline ()
