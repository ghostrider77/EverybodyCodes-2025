type segment = {left : int option; spine : int; right : int option}


let parse_input line =
  let id, numbers = Scanf.sscanf line "%d:%s" (fun id numbers -> (id, numbers)) in
  let ns = List.map int_of_string @@ String.split_on_char ',' numbers in
  (id, ns)


let build_fishbone xs =
  let fishbone = Dynarray.create () in
  let insert v =
    let value_can_be_inserted {left; spine; right} =
      (v < spine && Option.is_none left) || (v > spine && Option.is_none right) in
    match Dynarray.find_index value_can_be_inserted fishbone with
      | None -> Dynarray.add_last fishbone {left = None; spine = v; right = None}
      | Some ix ->
          let ({left; spine; _} as segment) = Dynarray.get fishbone ix in
          if v < spine && Option.is_none left then Dynarray.set fishbone ix {segment with left = Some v}
          else Dynarray.set fishbone ix {segment with right = Some v} in
  List.iter insert xs;
  Dynarray.to_list fishbone


let get_sword_quality xs =
  xs
    |> build_fishbone
    |> List.map (fun {spine; _} -> string_of_int spine)
    |> String.concat ""
    |> int_of_string


let get_detailed_sword_quality xs =
  let fishbone = build_fishbone xs in
  let get_value = Option.fold ~none:"" ~some:string_of_int in
  let get_level_number {left; spine; right} =
    int_of_string @@ Printf.sprintf "%s%d%s" (get_value left) spine (get_value right) in
  let spine_number =
    fishbone
      |> List.map (fun {spine; _} -> string_of_int spine)
      |> String.concat ""
      |> int_of_string in
  let level_numbers = List.map get_level_number fishbone in
  (spine_number, level_numbers)


let calc_sword_list_checksum swords =
  swords
    |> List.map (fun (id, xs) -> (id, get_detailed_sword_quality xs))
    |> List.sort (fun (id1, q1) (id2, q2) -> compare (q2, id2) (q1, id1))
    |> List.to_seq
    |> Seq.map fst
    |> Seq.fold_lefti (fun acc ix id -> acc + (ix + 1) * id) 0
