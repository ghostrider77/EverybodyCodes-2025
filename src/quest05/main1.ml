let () =
  let line = read_line () in
  let _, ns = Quest.parse_input line in
  let result = Quest.get_sword_quality ns in
  print_endline result
