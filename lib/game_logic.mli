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
  success_message : string;
}

(* Room types and status *)
type room_status =
  | Inaccessible
  | Accessible

type answer_result = {
  is_correct : bool;
  message : string;
}

type room = {
  room_id : int;
  mutable status : room_status;
  description : string;
  puzzles : puzzle list;
  room_deps : int list;
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
(* Return the puzzle's current status without updating. *)

val chest_puzzle : puzzle
val casket_puzzle : puzzle
val submit_answer : game_state -> int -> string -> answer_result
(* Submit an answer to a puzzle by id. Returns whether the answer is correct
   along with a message. *)

val init_game : unit -> game_state
