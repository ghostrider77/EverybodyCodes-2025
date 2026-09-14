let parse_input lines =
  List.map int_of_string lines


let calc_number_of_full_turns_of_last_gear wheels first_wheel_turns =
  let first = List.hd wheels in
  let last = List.hd (List.rev wheels) in
  int_of_float @@ (float (first_wheel_turns * first) /. float last)


let calc_number_of_full_turns_of_first_gear wheels last_wheel_turns =
  let first = List.hd wheels in
  let last = List.hd (List.rev wheels) in
  int_of_float @@ ceil @@ (float (last_wheel_turns * last) /. float first)
