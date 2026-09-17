let () =
  let line = read_line () in
  let nails = Quest.parse_input line in
  let nr_nails = 32 in
  let result = Quest.calc_nr_times_thread_passes_through_center nr_nails nails in
  print_int result; print_newline ()
