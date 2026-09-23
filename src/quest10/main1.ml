let () =
  let lines = In_channel.input_lines stdin in
  let board, dragon_position = Quest.parse_input lines in
  let result = Quest.get_nr_sheeps_the_dragon_can_eat board dragon_position 4 in
  print_int result; print_newline ()
