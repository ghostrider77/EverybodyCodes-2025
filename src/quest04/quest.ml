let parse_input lines =
  List.map int_of_string lines


let parse_cogwheel_pairs lines =
  let parse line =
    if String.contains line '|' then Scanf.sscanf line "%d|%d" (fun a b -> (a, b))
    else
      let n = int_of_string line in
      (n, n) in

  List.map parse lines


let generate_pairs xs =
  let rec aux acc = function
    | a :: b :: rest -> aux ((a, b) :: acc) (b :: rest)
    | _ -> List.rev acc in
  aux [] xs


let calc_number_of_full_turns_of_last_gear wheels first_wheel_turns =
  let first = List.hd wheels in
  let last = List.hd (List.rev wheels) in
  int_of_float @@ (float (first_wheel_turns * first) /. float last)


let calc_number_of_full_turns_of_first_gear wheels last_wheel_turns =
  let first = List.hd wheels in
  let last = List.hd (List.rev wheels) in
  int_of_float @@ ceil @@ (float (last_wheel_turns * last) /. float first)


let calc_number_of_full_turns_of_last_gear_for_paired_wheels wheels first_wheel_turns =
  let pairs = generate_pairs wheels in
  let turns = List.fold_left (fun acc ((_, b1), (a2, _)) -> acc *. (float b1) /. (float a2)) 1.0 pairs in
  int_of_float @@ (float first_wheel_turns) *. turns
