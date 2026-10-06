type coord = {x : int; y : int}

module CoordMap = Map.Make(
  struct
    type t = coord
    let compare = compare
  end
)

type grid = {cells : int CoordMap.t; volcano : coord}


let parse_input lines =
  let convert chr =
    int_of_string @@ String.make 1 (if chr = '@' then '0' else chr) in
  let cells =
    lines
      |> List.to_seq
      |> Seq.mapi (fun x row -> Seq.map (fun (y, chr) -> ({x; y}, convert chr)) @@ String.to_seqi row)
      |> Seq.concat in
  let volcano = Option.get @@ Seq.find_map (fun (cell, v) -> if v = 0 then Some cell else None) cells in
  {cells = CoordMap.of_seq cells; volcano}


let euclidean_distance {x = x1; y = y1} {x = x2; y = y2} =
  Float.hypot (float (x1 - x2)) (float (y1 - y2))


let calc_sum_of_cells_within_radius {cells; volcano} radius =
  CoordMap.fold (fun coord v acc -> if euclidean_distance coord volcano <= radius then acc + v else acc) cells 0
