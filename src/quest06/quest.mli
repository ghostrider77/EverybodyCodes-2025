type profession = SwordFighter | Archer | Magician
type hero

val parse_input : string -> hero list

val count_mentor_novice_pairs : hero list -> profession -> int
