type phase = First | Second


let parse_input =
  List.map int_of_string


let first_phase_round xs n =
  let arr = Array.of_list xs in
  for i = 0 to (n - 2) do
    if arr.(i + 1) < arr.(i) then
      begin
        arr.(i + 1) <- arr.(i + 1) + 1;
        arr.(i) <- arr.(i) - 1;
      end
  done;
  Array.to_list arr


let second_phase_round xs n =
  let arr = Array.of_list xs in
  for i = 0 to (n - 2) do
    if arr.(i + 1) > arr.(i) then
      begin
        arr.(i + 1) <- arr.(i + 1) - 1;
        arr.(i) <- arr.(i) + 1;
      end
  done;
  Array.to_list arr


let calc_flock_checksum xs =
  xs
    |> List.to_seq
    |> Seq.fold_lefti (fun acc ix x -> acc + (ix + 1) * x) 0


let flock_rearrangement xs nr_rounds =
  let n = List.length xs in
  let rec aux flock phase k =
    if k = nr_rounds then calc_flock_checksum flock
    else
      match phase with
        | First ->
            let flock' = first_phase_round flock n in
            if flock' = flock then aux flock' Second k
            else aux flock' First (k + 1)
        | Second ->
            let flock' = second_phase_round flock n in
            if flock' = flock then aux flock' Second nr_rounds
            else aux flock' Second (k + 1) in
  aux xs First 0
