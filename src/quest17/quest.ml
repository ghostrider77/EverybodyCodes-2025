type coord = {x : int; y : int}

module CoordMap = Map.Make(
  struct
    type t = coord
    let compare = compare
  end
)

type grid = {nr_rows : int; nr_cols : int; cells : int CoordMap.t; volcano : coord}


let parse_input = function
  | [] -> failwith "Empty input."
  | (hd :: _) as lines ->
      let nr_rows = List.length lines in
      let nr_cols = String.length hd in
      let convert chr =
        int_of_string @@ String.make 1 (if chr = '@' then '0' else chr) in
      let cells =
        lines
          |> List.to_seq
          |> Seq.mapi (fun x row -> Seq.map (fun (y, chr) -> ({x; y}, convert chr)) @@ String.to_seqi row)
          |> Seq.concat in
      let volcano = Option.get @@ Seq.find_map (fun (cell, v) -> if v = 0 then Some cell else None) cells in
      {nr_rows; nr_cols; cells = CoordMap.of_seq cells; volcano}


let euclidean_distance {x = x1; y = y1} {x = x2; y = y2} =
  Float.hypot (float (x1 - x2)) (float (y1 - y2))


let calc_radius_that_reaches_grid_limits {nr_rows; nr_cols; volcano = {x; y}; _} =
  let x_limit = min x (nr_rows - x - 1) in
  let y_limit = min y (nr_cols - y - 1) in
  min x_limit y_limit


let calc_sum_of_cells_within_radius {cells; volcano; _} radius =
  CoordMap.fold (fun coord v acc -> if euclidean_distance coord volcano <= radius then acc + v else acc) cells 0


let calc_largest_destruction grid =
  let max_radius = calc_radius_that_reaches_grid_limits grid in
  let rec aux ((best_radius, best_destruction) as acc) previous_sum radius =
    if radius > max_radius then best_radius * best_destruction
    else
      let current_sum = calc_sum_of_cells_within_radius grid (float radius) in
      let destruction = current_sum - previous_sum in
      if destruction > best_destruction then aux (radius, destruction) current_sum (radius + 1)
      else aux acc current_sum (radius + 1) in
  aux (0, 0) 0 1
