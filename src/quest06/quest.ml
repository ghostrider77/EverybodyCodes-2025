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
