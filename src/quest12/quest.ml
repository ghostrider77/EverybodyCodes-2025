type coord = {x : int; y : int}
type grid = {nr_rows : int; nr_cols : int; barrels : int iarray iarray}

module CoordSet = Set.Make(
  struct
    type t = coord
    let compare = compare
  end
)


let parse_input = function
  | [] -> failwith "Empty input."
  | (hd :: _) as lines ->
      let nr_rows = List.length lines in
      let nr_cols = String.length hd in
      let convert row =
        row |> String.to_seq |> Seq.map (fun c -> int_of_string @@ String.make 1 c) |> Iarray.of_seq in
      let barrels = Iarray.of_list @@ List.map convert lines in
      {nr_rows; nr_cols; barrels}


let get_neighbors {nr_rows; nr_cols; barrels} {x; y} =
  let is_valid {x = a; y = b} =
    0 <= a && a < nr_rows && 0 <= b && b < nr_cols in
  let get_value i j =
    Iarray.get (Iarray.get barrels i) j in
  let barrel = get_value x y in
  let ns = [{x; y = y - 1}; {x = x - 1; y}; {x; y = y + 1}; {x = x + 1; y}] in
  List.filter (fun ({x = a; y = b} as cell) -> is_valid cell && get_value a b <= barrel) ns


let find_ignited_barrels grid start_barrels =
  let queue = Queue.create () in
  Queue.add_seq queue (List.to_seq start_barrels);
  let rec aux visited =
    match Queue.take_opt queue with
      | None -> CoordSet.cardinal visited
      | Some coord ->
          let neighbors =
            coord
              |> get_neighbors grid
              |> List.filter (fun neighbor -> not (CoordSet.mem neighbor visited))
              |> List.to_seq in
          Queue.add_seq queue neighbors;
          aux (CoordSet.add_seq neighbors visited) in
  aux CoordSet.(empty |> add_seq (List.to_seq start_barrels))
