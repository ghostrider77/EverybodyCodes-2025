type nucleotide = A | C | G | T
type scale = {dna : nucleotide list}


let nucleotide_of_char = function
  | 'A' -> A
  | 'C' -> C
  | 'G' -> G
  | 'T' -> T
  | chr -> failwith (Printf.sprintf "Unknown nucleotide %c" chr)


let parse_input = function
  | [line1; line2; line3] ->
      let parse line =
        let _, str = Scanf.sscanf line "%d:%s" (fun id s -> (id, s)) in
        {dna = str |> String.to_seq |> Seq.map nucleotide_of_char |> List.of_seq} in
      (parse line1, parse line2, parse line3)
  | _ -> failwith "Malformed input."


let calc_similarity s1 s2 =
  List.fold_left2 (fun acc n1 n2 -> if n1 = n2 then acc + 1 else acc) 0 s1 s2


let is_child_of_parents p1 p2 s =
  let parent_pairs = Seq.zip (List.to_seq p2) (List.to_seq p1) in
  Seq.for_all2 (fun (a, b) c -> c = a || c = b) parent_pairs (List.to_seq s)


let calc_degree_of_similarity {dna = s1; _} {dna = s2; _} {dna = s3; _} =
  if is_child_of_parents s2 s3 s1 then (calc_similarity s1 s2) * (calc_similarity s1 s3)
  else if is_child_of_parents s1 s3 s2 then (calc_similarity s2 s1) * (calc_similarity s2 s3)
  else (calc_similarity s3 s1) * (calc_similarity s3 s2)
