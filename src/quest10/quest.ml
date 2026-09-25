type cell = {x : int; y : int}

module CellSet = Set.Make(
  struct
    type t = cell
    let compare = compare
  end
)

type board = {nr_rows : int; nr_cols : int; sheep : CellSet.t; hideouts: CellSet.t}


let parse_input = function
  | [] -> failwith "Empty input."
  | ((row :: _) as lines) ->
      let nr_cols = String.length row in
      let nr_rows = List.length lines in
      let nonempty_cells =
        lines
          |> List.to_seq
          |> Seq.mapi (fun x line -> Seq.map (fun (y, chr) -> ({x; y}, chr)) (String.to_seqi line))
          |> Seq.concat
          |> Seq.filter (fun (_, chr) -> chr <> ' ') in
      let collect_cells symbol =
        CellSet.of_seq @@ Seq.filter_map (fun (p, chr) -> if chr = symbol then Some p else None) nonempty_cells in
      let sheep = collect_cells 'S' in
      let hideouts = collect_cells '#' in
      let dragon = match CellSet.to_list @@ collect_cells 'D' with
        | [p] -> p
        | _ -> failwith "No dragon was found on board." in
      ({nr_rows; nr_cols; sheep; hideouts}, dragon)


let get_next_positions {nr_rows; nr_cols; _} {x; y}  =
  let is_cell_on_board {x = a; y = b} =
    0 <= a && a < nr_rows && 0 <= b && b < nr_cols in
  let ps = [
    {x = x - 2; y = y + 1};
    {x = x - 1; y = y + 2};
    {x = x + 1; y = y + 2};
    {x = x + 2; y = y + 1};
    {x = x + 2; y = y - 1};
    {x = x + 1; y = y - 2};
    {x = x - 1; y = y - 2};
    {x = x - 2; y = y - 1};
    ] in
  List.filter is_cell_on_board ps


let move_sheep_on_board ({nr_rows; sheep; _} as board) =
  {board with sheep = CellSet.filter_map (fun {x; y} -> if x = nr_rows - 1 then None else Some {x = x + 1; y}) sheep}


let collect_sheep_victims {sheep; hideouts; _} dragons =
  CellSet.diff (CellSet.inter sheep dragons) hideouts


let get_nr_sheep_the_dragon_can_eat ({sheep; _} as board) initial_position steps =
  let queue = Queue.create () in
  Queue.add (initial_position, 0) queue;
  let rec aux visited =
    match Queue.take_opt queue with
      | None -> visited
      | Some (position, distance) ->
          if distance = steps then visited
          else
            let next_positions =
              position
                |> get_next_positions board
                |> List.to_seq
                |> Seq.filter (fun p -> not @@ CellSet.mem p visited) in
            Queue.add_seq queue (Seq.map (fun p -> (p, distance + 1)) next_positions);
          aux (CellSet.add_seq next_positions visited) in
  let possible_dragon_positions = aux (CellSet.singleton initial_position) in
  CellSet.cardinal @@ CellSet.inter sheep possible_dragon_positions


let get_nr_sheep_the_dragon_can_eat_pt_2 initial_board dragon steps =
  let rec aux acc ({sheep; _} as current_board) dragons k =
    if k = steps then acc
    else
      let dragons' =
        dragons
          |> CellSet.to_list
          |> List.concat_map (get_next_positions current_board)
          |> CellSet.of_list in
      let victims1 = collect_sheep_victims current_board dragons' in
      let updated_board = {current_board with sheep = CellSet.diff sheep victims1} in
      let ({sheep = sheep'; _} as next_board) = move_sheep_on_board updated_board in
      let victims2 = collect_sheep_victims next_board dragons' in
      let acc' = acc + CellSet.cardinal victims1 + CellSet.cardinal victims2 in
      aux acc' {next_board with sheep = CellSet.diff sheep' victims2} dragons' (k + 1) in
  aux 0 initial_board (CellSet.singleton dragon) 0
