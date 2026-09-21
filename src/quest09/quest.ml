type nucleotide = A | C | G | T
type scale = {id : int; dna : nucleotide list}


let nucleotide_of_char = function
  | 'A' -> A
  | 'C' -> C
  | 'G' -> G
  | 'T' -> T
  | chr -> failwith (Printf.sprintf "Unknown nucleotide %c" chr)


let parse_input lines =
  let parse line =
    let id, str = Scanf.sscanf line "%d:%s" (fun id s -> (id, s)) in
    {id; dna = str |> String.to_seq |> Seq.map nucleotide_of_char |> List.of_seq} in
  List.map parse lines


let calc_similarity {dna = s1; _} {dna = s2; _} =
  List.fold_left2 (fun acc n1 n2 -> if n1 = n2 then acc + 1 else acc) 0 s1 s2


let is_child_of_parents ({id = id1; dna = dna1}, {id = id2; dna = dna2}) {id = id3; dna = dna3} =
  if id1 = id3 || id2 = id3 then false
  else
    let parent_nucleotides = Seq.zip (List.to_seq dna1) (List.to_seq dna2) in
    Seq.for_all2 (fun (a, b) c -> c = a || c = b) parent_nucleotides (List.to_seq dna3)


let calc_degree_of_similarity_sum scales =
  let calc_score ((p1, p2) as parents) child =
    if is_child_of_parents parents child then
      let sim1 = calc_similarity p1 child in
      let sim2 = calc_similarity p2 child in
      sim1 * sim2
    else 0 in
  let dnas = List.to_seq scales in
  dnas
    |> Seq.product dnas
    |> Seq.filter (fun ({id = id1; _}, {id = id2; _}) -> id1 < id2)
    |> Seq.map (fun parents -> Seq.fold_left (fun acc child -> acc + calc_score parents child) 0 dnas)
    |> Seq.fold_left (+) 0
