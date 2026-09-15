let () =
  let lines = In_channel.input_lines stdin in
  let swords = List.map Quest.parse_input lines in
  let result = Quest.calc_sword_list_checksum swords in
  print_int result; print_newline ()
