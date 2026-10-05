let parse_input lines =
  lines
    |> String.split_on_char ','
    |> List.map int_of_string


let calc_nr_of_blocks xs limit =
  List.fold_left (fun acc x -> acc + (limit / x)) 0 xs


let get_recreated_numbers blocks =
  let blocks = Array.of_list blocks in
  let n = Array.length blocks in
  let rec aux acc k =
    if k > n then List.rev acc
    else
      let multiplicity = blocks.(k - 1) in
      if multiplicity > 0 then
        let ixs = Seq.take_while (fun i -> i < n) @@ Seq.iterate (fun i -> i + k) (k - 1) in
        Seq.iter (fun ix -> blocks.(ix) <- blocks.(ix) - multiplicity) ixs;
        aux ((List.init multiplicity (Fun.const k)) @ acc) (k + 1)
      else aux acc (k + 1) in
  aux [] 1


let calc_product_of_recreated_numbers blocks =
  blocks
    |> get_recreated_numbers
    |> List.fold_left ( * ) 1


let get_wall_length blocks nr_available_blocks =
  let ns = get_recreated_numbers blocks in
  let rec aux low high =
    if low >= high then low
    else
      let mid = (low + high) / 2 in
      let used_blocks = calc_nr_of_blocks ns mid in
      if used_blocks = nr_available_blocks then low
      else if used_blocks > nr_available_blocks then aux low (mid - 1)
      else aux (mid + 1) high in
  aux 0 nr_available_blocks
