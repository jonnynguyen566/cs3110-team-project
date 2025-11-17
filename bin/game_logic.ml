(*Instantiating types for puzzles*)
type puzzle_status =
  | Locked (*Puzzle hasn't been revealed yet/isn't visible*)
  | Unlocked (*Puzzle is unlocked and can be interacted with*)
  | Solved (*Puzzle marked as solved, can use for unlocking dependent puzzles*)

type puzzle_type =
  | Riddle of string * string  (*question, answer*)
  | Math of string * int       (*question, numeric answer*)
  | Trivia of string * string  (*question, answer*)

type puzzle = {
  puzzle_id : string;
  puzzle_type : puzzle_type;
  mutable status : puzzle_status;
}

(*Instantiating types for rooms*)
type room_status =
  | Inaccessible (*Room cannot be accessed yet*)
  | Accessible   (*Room can be accessed by the player*)

type room = {
  room_id : string;
  mutable status : room_status;
  description : string;
  puzzles : puzzle list; (* List of puzzles in the room *)
}

(*Game state determines which room the player is currently in which rooms have been unlocked (meaning they can access those rooms)*)
type game_state = {
  mutable current_room : room;
  mutable unlocked_rooms : room list;
}

(*Function to check if a given answer to a puzzle is correct*)