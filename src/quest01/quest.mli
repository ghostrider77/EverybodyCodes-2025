type instruction

val parse_input : string list -> string iarray * instruction list

val follow_instructions : string iarray -> instruction list -> string

val follow_circular_instructions : string iarray -> instruction list -> string

val follow_circular_instructions_with_swaps : string iarray -> instruction list -> string
