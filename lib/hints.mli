val get_hint : int -> string option
(** Return a hint for a puzzle_id if one exists *)

val register_hint : puzzle_id:int -> string -> unit
(** Add a hint for a puzzle *)
