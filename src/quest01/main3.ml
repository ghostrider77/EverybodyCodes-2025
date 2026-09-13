let () =
  let lines = In_channel.input_lines stdin in
  let names, instructions = Quest.parse_input lines in
  let result = Quest.follow_circular_instructions_with_swaps names instructions in
  print_endline result
