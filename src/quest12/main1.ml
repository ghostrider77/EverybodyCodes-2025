let () =
  let lines = In_channel.input_lines stdin in
  let grid = Quest.parse_input lines in
  let start_barrels : Quest.coord list = [{x = 0; y = 0}] in
  let result = Quest.find_ignited_barrels grid start_barrels in
  print_int result; print_newline ()
