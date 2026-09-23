type cell
type board

val parse_input : string list -> board * cell

val get_nr_sheeps_the_dragon_can_eat : board -> cell -> int -> int
