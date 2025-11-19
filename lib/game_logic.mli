(* lib/game_logic.mli *)

(* Puzzle types and status *)
type puzzle_status =
  | Locked
  | Unlocked
  | Solved

type puzzle_type =
  | Math of string * int
  | Trivia of string * string

type puzzle = {
  puzzle_id : int;
  puzzle_type : puzzle_type;
  mutable status : puzzle_status;
  deps : int list;
}

(* Room types and status *)
type room_status =
  | Inaccessible
  | Accessible

type room = {
  room_id : int;
  mutable status : room_status;
  description : string;
  puzzles : puzzle list;
  room_deps : int list;
}

type answer_result = {
  is_correct : bool;
  message : string;
}

(* Game state *)
type game_state = {
  mutable current_room : room;
  rooms : room list;
}

(* Functions *)
val new_puzzle_id : unit -> int
(* Generate a fresh puzzle id. *)

val get_next_room : game_state -> room
(* Return the next accessible room (wraps around). *)

val find_puzzle : game_state -> int -> puzzle option
(* Find a puzzle by id in the game's rooms. *)

val deps_satisfied : game_state -> puzzle -> bool
(* True if all dependencies for the given puzzle are solved. *)

val check_puzzle_status : game_state -> puzzle -> puzzle_status
(* Update and return the puzzle's status (unlock if deps satisfied). *)

val get_puzzle_status : game_state -> int -> puzzle_status
(* Get the status of a puzzle by id. Helps reconvert the option none/some type
   to puzzle_status *)

val chest_puzzle : puzzle
val casket_puzzle : puzzle
val submit_answer : game_state -> int -> string -> bool
(* Submit an answer for puzzle id; return true if correct and marks solved. *)
