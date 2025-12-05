(** 
  AF: The module represents an in-game puzzle in the escape room game:
    - Each [puzzle] value corresponds to one puzzle that the player can solve.
    - In each puzzle, it consists of
        - a unique integer ID
        - a puzzle type (riddle, math, trivia)
        - a list of dependency puzzle IDs that must be solved before this puzzle can be accessible to the player
        - a success message displayed upon solving the puzzle
        - a mutable status indicating whether the puzzle is Locked, Unlocked, or Solved

  RI:
  - [id] is a globally unique non-negative integer.
  - [status] is [Locked] initially, becomes [Unlocked] when all dependent puzzles are solved,
    and becomes [Solved] after [mark_solved] is called.
  - [status] can only transition: [Locked] to [Unlocked] to [Solved].
    It can never transition backwards.
  - [puzzle_type] contains the question and correct answer.
  - [success_msg] is a non-empty string displayed when the puzzle is solved. It is a hint that leads the player to the next clue
  - Dependencies in [deps] are valid puzzle IDs that must all be solved before this puzzle can unlock.
*)

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

(** Puzzle definition *)
type puzzle

(**[puzzle_id] is an accessor that returns the unique integer ID associated with [p]*)
val puzzle_id : puzzle -> int
(**[status] is an accessor that returns the current access status of [p]*)
val status : puzzle -> puzzle_status
(**[success_msg] is an accessor that returns the success message of [p]*)
val success_msg : puzzle -> string
(**[puzzle_type] is an accessor that returns the puzzle type of [p]*)
val puzzle_type : puzzle -> puzzle_type

(**[make] creates a new puzzle with the given parameters
    - [id]:          globally unique puzzle ID  
    - [puzzle_type]: the puzzle’s question and answer  
    - [deps]:        list of puzzle IDs that must be solved before this one can unlock  
    - [success_msg]: text displayed after solving the puzzle  *)
val make :
  id:int ->
  puzzle_type:puzzle_type ->
  deps:int list ->
  success_msg:string ->
  puzzle

(** [check_answer] returns [true] if the player's input is the correct answer that solves puzzle [p], or [false] otherwise. *)
val check_answer : puzzle -> string -> bool

(** [mark_solved] Mark a puzzle as solved. Mutates [p] to have status [Solved] and does not perform dependency checks*)
val mark_solved : puzzle -> unit

(** [try_unlock solved_ids p] unlocks puzzle [p] if all its dependency puzzle IDs
    are present in [solved_ids].
    - If all dependencies are satisfied, [p] becomes [Unlocked].
    - If [p] is already [Unlocked] or [Solved], it remains unchanged.
    - If dependencies are missing, [p] stays [Locked]. *)
val try_unlock : int list -> puzzle -> unit

(** [set_status p s] forcefully sets the status of puzzle [p] to [s].*)
val set_status : puzzle -> puzzle_status -> unit
(** Set the status of a puzzle *)
