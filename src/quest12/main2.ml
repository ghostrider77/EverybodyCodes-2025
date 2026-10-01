let () =
  let lines = In_channel.input_lines stdin in
  let ({nr_rows; nr_cols; _} as grid) : Quest.grid = Quest.parse_input lines in
  let start_barrels : Quest.coord list = [{x = 0; y = 0}; {x = nr_rows - 1; y = nr_cols - 1}] in
  let result = Quest.find_ignited_barrels grid start_barrels in
  print_int result; print_newline ()
