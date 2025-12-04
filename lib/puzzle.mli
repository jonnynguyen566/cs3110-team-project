(** Abstract puzzle status *)
type puzzle_status =
  | Locked
  | Unlocked
  | Solved

(** Supported puzzle kinds *)
type puzzle_type =
  | Riddle of string * string
  | Math of string * int
  | Trivia of string * string

type puzzle
(** Puzzle definition *)

val puzzle_id : puzzle -> int
val status : puzzle -> puzzle_status
val success_msg : puzzle -> string
val puzzle_type : puzzle -> puzzle_type

val make :
  id:int ->
  puzzle_type:puzzle_type ->
  deps:int list ->
  success_msg:string ->
  puzzle
(** Create a puzzle *)

val check_answer : puzzle -> string -> bool
(** Try to solve a puzzle by checking an answer *)

val mark_solved : puzzle -> unit
(** Mark a puzzle as solved *)

val try_unlock : int list -> puzzle -> unit
(** Unlock puzzle if dependencies are solved *)

val set_status : puzzle -> puzzle_status -> unit
(** Set the status of a puzzle *)
