let () =
  let line = read_line () in
  let blocks = Quest.parse_input line in
  let result = Quest.calc_product_of_recreated_numbers blocks in
  print_int result; print_newline ()
