type coord = {x : int; y : int}
type grid = {nr_rows : int; nr_cols : int; barrels : int iarray iarray}

val parse_input : string list -> grid

val find_ignited_barrels : grid -> coord list -> int
