let global_puzzleid_counter = ref 0

(*Helper Function: Use this function every time when creating a new puzzle to
  ensure we are generating unique puzzle ids*)
let new_puzzle_id () =
  let pid = !global_puzzleid_counter in
  global_puzzleid_counter := pid + 1;
  pid

(*Instantiating types for puzzles*)
type puzzle_status =
  | Locked (*Puzzle hasn't been revealed yet/isn't visible*)
  | Unlocked (*Puzzle is unlocked and can be interacted with*)
  | Solved (*Puzzle marked as solved, can use for unlocking dependent puzzles*)

type puzzle_type =
  | Math of string * int (*question, numeric answer*)
  | Trivia of string * string (*question, answer*)

(*Will ensure that puzzle_id will start at 0*)
type puzzle = {
  puzzle_id : int;
  puzzle_type : puzzle_type;
  mutable status : puzzle_status;
  deps : int list;
      (*List of puzzle IDs that must be solved before this puzzle can be
        attempted*)
}

(*Instantiating types for rooms*)
type room_status =
  | Inaccessible (*Room cannot be accessed yet*)
  | Accessible (*Room can be accessed by the player*)

(*Will ensure that room_id will start at 0*)
type room = {
  room_id : int;
  mutable status : room_status;
  description : string;
  puzzles : puzzle list; (* List of puzzles in the room *)
  room_deps : int list (* puzzle IDs required to unlock room *);
}

type answer_result = {
  is_correct : bool;
  message : string;
}

(*Game state determines which room the player is currently in which rooms have
  been unlocked (meaning they can access those rooms)*)
type game_state = {
  mutable current_room : room;
  rooms : room list; (* List of all rooms in the game, static*)
}

(*Room related functions*)
let rec get_next_room game_state =
  let rooms = game_state.rooms in
  let n = List.length rooms in
  let current_idx = game_state.current_room.room_id in
  let rec loop i =
    let idx = (i + 1) mod n in
    let room = List.nth rooms idx in
    if room.status = Accessible then room else loop idx
  in
  loop current_idx

(*Puzzle related functions*)
let find_puzzle (game : game_state) (pid : int) : puzzle option =
  let rec search_rooms = function
    | [] -> None
    | r :: rs -> (
        match List.find_opt (fun p -> p.puzzle_id = pid) r.puzzles with
        | Some p -> Some p
        | None -> search_rooms rs)
  in
  search_rooms game.rooms

(* Helper function: returns true if all dependencies are solved, and false if
   not*)
let deps_satisfied game (p : puzzle) =
  List.for_all
    (fun dep_id ->
      match find_puzzle game dep_id with
      | Some dep_puzzle -> dep_puzzle.status = Solved
      | None -> false)
    p.deps

(* Check if puzzle is accessible. If locked, verify dependencies are satisfied
   and update status to unlocked if possible *)
let check_puzzle_status game (p : puzzle) =
  match p.status with
  | Unlocked | Solved -> p.status
  | Locked ->
      if deps_satisfied game p then (
        p.status <- Unlocked;
        (*Updates status to unlocked and returns unlocked*)
        p.status)
      else p.status (*Should still return Locked*)

let get_puzzle_status (game : game_state) (pid : int) : puzzle_status =
  match find_puzzle game pid with
  | None -> Locked (*If puzzle not found, treat as locked*)
  | Some p -> check_puzzle_status game p



(*Chest puzzle id should be 1*)
let chest_puzzle : puzzle =
  {
    puzzle_id = new_puzzle_id ();
    puzzle_type = Trivia ("What is a female camel called?", "cow");
    status = Unlocked;
    deps = [];
  }

(*Casket puzzle id should be 2*)
let casket_puzzle : puzzle =
  {
    puzzle_id = new_puzzle_id ();
    puzzle_type =
      Math ("Does the following definition type check: let x = 2 +. 3.0", 0);
    status = Unlocked;
    deps = [ chest_puzzle.puzzle_id ];
  }

let submit_answer game pid answer =
  match find_puzzle game pid with
  | None -> false
  | Some puzzle -> (
      match puzzle.puzzle_type with
      | Trivia (_, correct_answer) ->
          if
            String.lowercase_ascii answer
            = String.lowercase_ascii correct_answer
          then (
            puzzle.status <- Solved;
            true)
          else false
      | Math (_, correct_answer) -> (
          try
            let ans_int = int_of_string answer in
            if ans_int = correct_answer then (
              puzzle.status <- Solved;
              true)
            else false
          with Failure _ -> false))
