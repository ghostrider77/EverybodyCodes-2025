let () =
  let line = read_line () in
  let crates = Quest.parse_input line in
  let result = Quest.find_maximum_occurrence crates in
  print_int result; print_newline ()
