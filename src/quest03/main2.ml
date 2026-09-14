let () =
  let line = read_line () in
  let crates = Quest.parse_input line in
  let result = Quest.find_sum_of_smallest_set_of_given_size crates 20 in
  print_int result; print_newline ()
