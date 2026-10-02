let parse_input =
  List.map int_of_string


let create_dial_from_numbers ns =
  let length = List.length ns in
  let dial = Array.make (length + 1) 1 in
  let ixs = Seq.init length (fun k -> if k mod 2 = 0 then k / 2 + 1 else length - k / 2) in
  Seq.iter2 (fun n ix -> dial.(ix) <- n) (List.to_seq ns) ixs;
  Iarray.of_array dial


let turn_dial ns k =
  let dial = create_dial_from_numbers ns in
  let n = Iarray.length dial in
  let ix = k mod n in
  Iarray.get dial ix
