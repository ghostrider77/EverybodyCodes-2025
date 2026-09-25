let () =
  let lines = In_channel.input_lines stdin in
  let board, dragon_position = Quest.parse_input lines in
  let steps = 20 in
  let result = Quest.get_nr_sheep_the_dragon_can_eat_pt_2 board dragon_position steps in
  print_int result; print_newline ()
