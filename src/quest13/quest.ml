type range = {a : int; b : int}
type direction = Clockwise | Counterclockwise
type state = {left_ix : int; right_ix : int; direction: direction}


let parse_input =
  List.map int_of_string


let parse_ranges lines =
  let parse line =
    Scanf.sscanf line "%d-%d" (fun a b -> {a; b}) in
  List.map parse lines


let create_dial_from_numbers ns =
  let length = List.length ns in
  let dial = Array.make (length + 1) 1 in
  let ixs = Seq.init length (fun k -> if k mod 2 = 0 then k / 2 + 1 else length - k / 2) in
  Seq.iter2 (fun n ix -> dial.(ix) <- n) (List.to_seq ns) ixs;
  Iarray.of_array dial


let create_dial_from_ranges rs =
  let length = List.fold_left (fun acc {a; b} -> acc + b - a + 1) 0 rs in
  let dial = Array.make (length + 1) 1 in
  let rec aux ({left_ix; right_ix; direction} as state) = function
    | [] -> Iarray.of_array dial
    | {a; b} :: rest ->
        let len = b - a + 1 in
        let numbers = Seq.init len (fun k -> k + a) in
        match direction with
          | Clockwise ->
              let ixs = Seq.init len (fun k -> k + left_ix) in
              Seq.iter2 (fun n ix -> dial.(ix) <- n) numbers ixs;
              aux {state with left_ix = left_ix + len; direction = Counterclockwise} rest
          | Counterclockwise ->
              let ixs = Seq.init len (fun k -> right_ix - k) in
              Seq.iter2 (fun n ix -> dial.(ix) <- n) numbers ixs;
              aux {state with right_ix = right_ix - len; direction = Clockwise} rest in
  aux {left_ix = 1; right_ix = length; direction = Clockwise} rs


let turn_dial dial k =
  let n = Iarray.length dial in
  let ix = k mod n in
  Iarray.get dial ix
