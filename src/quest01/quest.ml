type instruction = Left of int | Right of int


let modulo a n =
  let r = a mod n in
  if r < 0 then r + n else r


let instruction_of_string str =
  let (direction, steps) = Scanf.sscanf str "%c%d" (fun chr k -> (chr, k)) in
  match direction with
    | 'L' -> Left steps
    | 'R' -> Right steps
    | _ -> failwith (Printf.sprintf "Unexpected direction %c." direction)


let parse_input lines =
  match lines with
    | [words; _; moves] ->
        let names = words |> String.split_on_char ',' |> Iarray.of_list in
        let instructions = moves |> String.split_on_char ',' |> List.map instruction_of_string in
        (names, instructions)
    | _ -> failwith "Malformed input."


let follow_instructions names instructions =
  let n = Iarray.length names in
  let rec aux position = function
    | [] -> Iarray.get names position
    | (Left steps) :: rest -> aux (max 0 (position - steps)) rest
    | (Right steps) :: rest -> aux (min (n - 1) (position + steps)) rest in
  aux 0 instructions


let follow_circular_instructions names instructions =
  let n = Iarray.length names in
  let rec aux position = function
    | [] -> Iarray.get names position
    | (Left steps) :: rest -> aux (modulo (position - steps) n) rest
    | (Right steps) :: rest -> aux (modulo (position + steps) n) rest in
  aux 0 instructions


let follow_circular_instructions_with_swaps names instructions =
  let names = Iarray.to_array names in
  let n = Array.length names in
  let rec aux = function
    | [] -> names.(0)
    | instr :: rest ->
        let ix = match instr with
          | Left steps -> modulo (-steps) n
          | Right steps -> modulo steps n in
        let top = names.(0) in
        names.(0) <- names.(ix);
        names.(ix) <- top;
        aux rest in
  aux instructions
