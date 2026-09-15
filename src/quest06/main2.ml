let () =
  let line = read_line () in
  let heroes = Quest.parse_input line in
  let result1 = Quest.count_mentor_novice_pairs heroes Quest.SwordFighter in
  let result2 = Quest.count_mentor_novice_pairs heroes Quest.Archer in
  let result3 = Quest.count_mentor_novice_pairs heroes Quest.Magician in
  let result = result1 + result2 + result3 in
  print_int result; print_newline ()
