type range

val parse_input : string list -> int list

val parse_ranges : string list -> range list

val create_dial_from_numbers : int list -> int iarray

val create_dial_from_ranges : range list -> int iarray

val turn_dial : int iarray -> int -> int
