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
