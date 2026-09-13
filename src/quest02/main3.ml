let () =
  let line = read_line () in
  let top_left = Quest.read_number line in
  let (tx, ty) = Quest.ComplexNumber.to_pair top_left in
  let xs = Seq.init 1001 (fun k -> tx + k) in
  let ys = Seq.init 1001 (fun k -> ty + k) in
  let coord_grid = Seq.product xs ys in
  let grid_numbers = Seq.map (fun (x, y) -> Quest.ComplexNumber.init x y) coord_grid in
  let result =
    Seq.fold_left
      (fun acc n -> if Option.is_some (Quest.perform_conditional_cycles n 100) then acc + 1 else acc)
      0
      grid_numbers in
  print_int result; print_newline ()
