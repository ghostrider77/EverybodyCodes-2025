let () =
  let line = read_line () in
  let heroes = Quest.parse_input line in
  let repeat = 1000 in
  let radius = 1000 in
  let result = Quest.count_nearby_mentor_novice_pairs heroes repeat radius in
  print_int result; print_newline ()
