type rank = Knight | Novice
type profession = SwordFighter | Archer | Magician
type hero = {profession : profession; rank : rank}

let hero_of_char = function
  | 'a' -> {profession = SwordFighter; rank = Novice}
  | 'A' -> {profession = SwordFighter; rank = Knight}
  | 'b' -> {profession = Archer; rank = Novice}
  | 'B' -> {profession = Archer; rank = Knight}
  | 'c' -> {profession = Magician; rank = Novice}
  | 'C' -> {profession = Magician; rank = Knight}
  | chr -> failwith (Printf.sprintf "Unknown profession %c" chr)


let parse_input str =
  str
    |> String.to_seq
    |> Seq.map hero_of_char
    |> List.of_seq


let count_mentor_novice_pairs heroes category =
  let selected_heroes = List.filter (fun {profession; _} -> profession = category) heroes in
  let (knights, novices) =
    selected_heroes
      |> List.mapi (fun ix h -> (ix, h))
      |> List.partition_map (fun (ix, {rank; _}) -> if rank = Knight then Left ix else Right ix) in
  let get_nr_novices knight_ix =
    List.length @@ List.drop_while (fun ix -> ix < knight_ix) novices in
  List.fold_left (fun acc knight_ix -> acc + get_nr_novices knight_ix) 0 knights


let repeat_and_filter_heroes heroes repeat category =
  let n = List.length heroes in
  heroes
    |> List.to_seq
    |> Seq.cycle
    |> Seq.take (n * repeat)
    |> Seq.mapi (fun ix x -> (ix, x))
    |> Seq.filter_map (fun (ix, {profession; rank}) -> if profession = category then Some (ix, rank) else None)
    |> List.of_seq


let remove_mentors queue current_ix radius =
  let left_limit = current_ix - radius in
  let rec aux () =
    match Queue.peek_opt queue with
      | Some ix when ix < left_limit ->
          Queue.drop queue;
          aux ()
      | _ -> () in
  aux ()


let count_nearby_mentor_novice_pairs_in_given_category heroes repeat radius category =
  let selected_heroes = repeat_and_filter_heroes heroes repeat category in
  let queue = Queue.create () in
  let rec aux acc = function
    | [] -> acc
    | (ix, rank) :: rest ->
        remove_mentors queue ix radius;
        match rank with
          | Knight ->
              Queue.push ix queue;
              aux acc rest
          | Novice ->
              let mentors_left = Queue.length queue in
              let mentors_right =
                rest
                  |> List.take_while (fun (j, _) -> j <= ix + radius)
                  |> List.fold_left (fun count (_, r) -> if r = Knight then count + 1 else count) 0 in
              aux (acc + mentors_left + mentors_right) rest in
  aux 0 selected_heroes


let count_nearby_mentor_novice_pairs heroes repeat radius =
  [SwordFighter; Archer; Magician]
    |> List.map (count_nearby_mentor_novice_pairs_in_given_category heroes repeat radius)
    |> List.fold_left (+) 0
