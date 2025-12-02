open Puzzle
open Room
open Game_state
open Hints

type game_state = Game_state.t

type answer_result = {
  is_correct : bool;
  message : string;
}

val init_game : unit -> game_state
val find_puzzle : game_state -> int -> Puzzle.puzzle option
val get_puzzle_status : game_state -> int -> Puzzle.puzzle_status
val check_puzzle_status : game_state -> Puzzle.puzzle -> Puzzle.puzzle_status
val submit_answer : game_state -> int -> string -> answer_result
