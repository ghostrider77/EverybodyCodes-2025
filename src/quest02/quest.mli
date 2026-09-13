module ComplexNumber : sig
  type t
  val init : int -> int -> t
  val add : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val of_string : string -> t
  val to_string : t -> string
  val to_pair : t -> int * int
end

val read_number : string -> ComplexNumber.t

val perform_cycle_k_times : ComplexNumber.t -> int -> ComplexNumber.t

val create_grid : ComplexNumber.t -> (int * int) Seq.t

val perform_conditional_cycles : ComplexNumber.t -> int -> ComplexNumber.t option
