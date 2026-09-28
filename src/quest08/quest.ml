module PairMap = Map.Make(
  struct
    type t = int * int
    let compare = compare
  end
)


let parse_input line =
  line
    |> String.split_on_char ','
    |> List.map int_of_string


let are_segments_crossing (a1, b1) (a2, b2) =
  if a1 = a2 || b1 = b2 then false
  else
    let point_on_side x =
      a1 < x && x < b1 in
    (point_on_side a2) <> (point_on_side b2)


let calc_nr_times_thread_passes_through_center n nails =
  if n mod 2 = 1 then 0
  else
    let half_circle_size = n / 2 in
    let are_opposites a b =
      half_circle_size = abs (a - b) in
    let ns = List.to_seq nails in
    Seq.fold_left (fun acc (a, b) -> if are_opposites a b then acc + 1 else acc) 0 @@ Seq.zip (Seq.drop 1 ns) ns


let calc_total_nr_intersections nails =
  let process (segments, cnt) p =
    let increment = function
      | None -> Some 1
      | Some c -> Some (c + 1) in
    let k = PairMap.fold (fun x cnt acc -> if are_segments_crossing x p then acc + cnt else acc) segments 0 in
    (PairMap.update p increment segments, cnt + k) in
  let ns = List.to_seq nails in
  let pairs = Seq.map (fun (a, b) -> (min a b, max a b)) @@ Seq.zip (Seq.drop 1 ns) ns in
  snd @@ Seq.fold_left process (PairMap.empty, 0) pairs
