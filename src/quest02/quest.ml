module ComplexNumber = struct
  type t = {a : int; b : int}

  let init a b = {a; b}

  let add {a = a1; b = b1} {a = a2; b = b2} =
    {a = a1 + a2; b = b1 + b2}

  let mul {a = a1; b = b1} {a = a2; b = b2} =
    {a = a1 * a2 - b1 * b2; b = a1 * b2 + b1 * a2}

  let div {a = a1; b = b1} {a = a2; b = b2} =
    {a = a1 / a2; b = b1 / b2}

  let of_string str =
    Scanf.sscanf str "[%d,%d]" (fun a b -> {a; b})

  let to_string {a; b} =
    Printf.sprintf "[%d,%d]" a b

  let to_pair {a; b} = (a, b)

end


let read_number line =
  Scanf.sscanf line "A=%s" (fun str -> ComplexNumber.of_string str)


let perform_full_cycle result d x =
  result
    |> ComplexNumber.mul result
    |> (Fun.flip ComplexNumber.div) d
    |> ComplexNumber.add x


let perform_cycle_k_times x k =
  let initial = ComplexNumber.init 0 0 in
  let d = ComplexNumber.init 10 10 in
  Seq.fold_left (fun acc _ -> perform_full_cycle acc d x) initial @@ Seq.init k Fun.id


let create_grid top_left =
  let (tx, ty) = ComplexNumber.to_pair top_left in
  let xs = Seq.init 101 (fun k -> tx + k * 10) in
  let ys = Seq.init 101 (fun k -> ty + k * 10) in
  Seq.product xs ys


let is_within_limit (z : ComplexNumber.t) =
  let upper = 1_000_000 in
  let lower = -upper in
  lower <= z.a && z.a <= upper && lower <= z.b && z.b <= upper


let perform_conditional_cycles x k =
  let divisor = ComplexNumber.init 100_000 100_000 in
  let rec aux acc ix =
    if ix = k then Some acc
    else
      let acc' = perform_full_cycle acc divisor x in
      if is_within_limit acc' then aux acc' (ix + 1)
      else None in
  let initial = ComplexNumber.init 0 0 in
  aux initial 0
