let () =
  let lines = In_channel.input_lines stdin in
  let names, rules = Quest.parse_input lines in
  let result = Quest.find_name_that_complies_to_the_rules names rules in
  print_endline result
