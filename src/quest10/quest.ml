type cell = {x : int; y : int}

module CellSet = Set.Make(
  struct
    type t = cell
    let compare = compare
  end
)

type board = { nr_rows : int; nr_cols : int; sheep : CellSet.t }


let parse_input = function
  | [] -> failwith "Empty input."
  | ((row :: _) as lines) ->
      let nr_cols = String.length row in
      let nr_rows = List.length lines in
      let animals =
        lines
          |> List.to_seq
          |> Seq.mapi (fun x line -> Seq.map (fun (y, chr) -> ({x; y}, chr)) (String.to_seqi line))
          |> Seq.concat
          |> Seq.filter (fun (_, chr) -> chr <> ' ') in
      let sheep = CellSet.of_seq @@ Seq.filter_map (fun (p, chr) -> if chr = 'S' then Some p else None) animals in
      let dragon = match Seq.find_map (fun (p, chr) -> if chr = 'D' then Some p else None) animals with
        | None -> failwith "No dragon was found on board."
        | Some p -> p in
      ({nr_rows; nr_cols; sheep}, dragon)


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


let get_nr_sheeps_the_dragon_can_eat ({sheep; _} as board) initial_position steps =
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
