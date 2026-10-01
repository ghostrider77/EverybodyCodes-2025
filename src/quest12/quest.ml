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


let extract_largest_component components =
  match List.sort (fun a b -> compare (CoordSet.cardinal b) (CoordSet.cardinal a)) components with
    | [] -> (0, [])
    | largest :: rest ->
        let size = CoordSet.cardinal largest in
        let component_diffs = List.map (fun component -> CoordSet.diff component largest) rest in
        (size, component_diffs)


let find_component grid start_barrels =
  let queue = Queue.create () in
  Queue.add_seq queue (List.to_seq start_barrels);
  let rec aux visited =
    match Queue.take_opt queue with
      | None -> visited
      | Some coord ->
          let neighbors =
            coord
              |> get_neighbors grid
              |> List.filter (fun neighbor -> not (CoordSet.mem neighbor visited))
              |> List.to_seq in
          Queue.add_seq queue neighbors;
          aux (CoordSet.add_seq neighbors visited) in
  aux CoordSet.(empty |> add_seq (List.to_seq start_barrels))


let find_ignited_barrels grid start_barrels =
  let component = find_component grid start_barrels in
  CoordSet.cardinal component


let find_greedy_largest_components ({nr_rows; nr_cols; _} as grid) k =
  let rec aux components = function
    | [] ->
        let range = Seq.init k Fun.id in
        range
          |> Seq.fold_left (fun (s, c) _ -> let (n, c') = extract_largest_component c in (s + n, c')) (0, components)
          |> fst
    | coord :: rest ->
        if List.exists (fun component -> CoordSet.mem coord component) components then aux components rest
        else
          let component = find_component grid (List.singleton coord) in
          aux (component :: components) rest in
  let coords = Seq.map (fun (x, y) -> {x; y}) @@ Seq.product (Seq.init nr_rows Fun.id) (Seq.init nr_cols Fun.id) in
  aux [] (List.of_seq coords)
