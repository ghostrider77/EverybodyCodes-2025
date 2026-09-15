let () =
  let lines = In_channel.input_lines stdin in
  let swords = List.map (Fun.compose snd Quest.parse_input) lines in
  let qualities = List.map Quest.get_sword_quality swords in
  let max_quality = List.fold_left (max) min_int qualities in
  let min_quality = List.fold_left (min) max_int qualities in
  let result = max_quality - min_quality in
  print_int result; print_newline ()
