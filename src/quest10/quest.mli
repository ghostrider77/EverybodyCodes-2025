type cell
type board

val parse_input : string list -> board * cell

val get_nr_sheep_the_dragon_can_eat : board -> cell -> int -> int

val get_nr_sheep_the_dragon_can_eat_pt_2 : board -> cell -> int -> int
