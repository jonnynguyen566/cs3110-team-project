open Puzzle
open Room
open Game_state
open Hints

type game_state = Game_state.t

type answer_result = {
  is_correct : bool;
  message : string;
}

(*Instantiating Puzzles*)
let chest_puzzle_0 =
  Puzzle.make ~id:0
    ~puzzle_type:(Trivia ("What is a female camel called?", "cow"))
    ~deps:[]

let casket_puzzle_1 =
  Puzzle.make ~id:1
    ~puzzle_type:(Math ("Does this type check: let x = 2 +. 3.0?", 0))
    ~deps:[ 0 ]

(* Hints *)
let () =
  register_hint ~puzzle_id:0 "Riches uncovered… now seek the sarcophagus.";
  register_hint ~puzzle_id:1 "You have awakened the mummy… find your escape!"

(*Instantiating Rooms*)
let room0 =
  Room.make ~id:0 ~description:"You are in the starting room."
    ~puzzles:[ chest_puzzle_0; casket_puzzle_1 ]
    ~room_deps:[]

(*Initiating Game State*)
let init_game () = Game_state.init ~rooms:[ room0 ] ~start:0

(*Game Logic Functions*)
let find_puzzle gs pid =
  List.find_map
    (fun room -> List.find_opt (fun p -> puzzle_id p = pid) (Room.puzzles room))
    (Game_state.all_rooms gs)

(*This one is READ ONLY -> directly returns current puzzle status*)
let get_puzzle_status gs pid =
  match find_puzzle gs pid with
  | None -> Locked
  | Some p -> Puzzle.status p

let check_puzzle_status gs p =
  Puzzle.try_unlock (Game_state.solved_puzzles gs) p;
  Puzzle.status p

let submit_answer gs pid answer =
  match find_puzzle gs pid with
  | None -> { is_correct = false; message = "Puzzle not found." }
  | Some p ->
      if check_answer p answer then (
        mark_solved p;
        Game_state.solve_puzzle gs ~puzzle_id:pid;

        let msg =
          match get_hint pid with
          | Some h -> h
          | None -> "Correct!"
        in

        { is_correct = true; message = msg })
      else { is_correct = false; message = "Incorrect answer." }

(*Game logic for changing rooms*)
let goto_next_room gs = Game_state.goto_next_room gs
let current_room_id gs = Game_state.get_current_room_id gs

let is_room_accessible gs id =
  match Game_state.get_room gs id with
  | None -> false
  | Some r -> Room.is_accessible r
