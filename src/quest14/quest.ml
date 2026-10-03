type tile = {x : int; y : int}

module TileSet = Set.Make(
  struct
    type t = tile
    let compare = compare
  end
)

type grid = {nr_rows : int; nr_cols : int; active_tiles : TileSet.t}


let parse_input = function
  | [] -> failwith "Empty input."
  | ((hd :: _) as lines) ->
      let nr_rows = List.length lines in
      let nr_cols = String.length hd in
      let active_tiles =
        lines
          |> List.to_seq
          |> Seq.mapi
              (fun x row -> Seq.filter_map (fun (y, c) -> if c = '#' then Some {x; y} else None) @@ String.to_seqi row)
          |> Seq.concat
          |> TileSet.of_seq in
      {nr_rows; nr_cols; active_tiles}


let perform_one_round ({nr_rows; nr_cols; active_tiles} as grid) =
  let is_valid {x; y} =
    0 <= x && x < nr_rows && 0 <= y && y < nr_cols in
  let get_nr_active_diagonal_neighbors {x; y} =
    let diagonals = [{x = x - 1; y = y - 1}; {x = x - 1; y = y + 1}; {x = x + 1; y = y + 1}; {x = x + 1; y = y - 1}] in
    List.(diagonals |> filter (fun cell -> is_valid cell && TileSet.mem cell active_tiles) |> length) in
  let is_active_in_next_round tile =
    let n = get_nr_active_diagonal_neighbors tile in
    let is_currently_active = TileSet.mem tile active_tiles in
    let is_odd = n mod 2 = 1 in
    (is_currently_active && is_odd) || (not is_currently_active && not is_odd) in
  let tiles = Seq.flat_map (fun x -> Seq.map (fun y -> {x; y}) (Seq.init nr_cols Fun.id)) (Seq.init nr_rows Fun.id) in
  let next_active_tiles = TileSet.of_seq @@ Seq.filter is_active_in_next_round tiles in
  {grid with active_tiles = next_active_tiles}


let play_game grid nr_rounds =
  let rec aux acc state k =
    if k = nr_rounds then acc
    else
      let ({active_tiles; _} as state') = perform_one_round state in
      aux (acc + TileSet.cardinal active_tiles) state' (k + 1) in
  aux 0 grid 0
