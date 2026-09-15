let () =
  let line = read_line () in
  let heroes = Quest.parse_input line in
  let result = Quest.count_mentor_novice_pairs heroes Quest.SwordFighter in
  print_int result; print_newline ()
