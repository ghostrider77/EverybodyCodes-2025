module CharMap : Map.S with type key = char
module StringSet : Set.S

val parse_input : string list -> string list * char list CharMap.t

val find_name_that_complies_to_the_rules : string list -> char list CharMap.t -> string

val get_index_sum_of_all_allowed_names : string list -> char list CharMap.t -> int

val calc_nr_of_unique_prefixes : string list -> char list CharMap.t -> int -> int -> int
