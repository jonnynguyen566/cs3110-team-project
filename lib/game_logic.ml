open Puzzle
open Room
open Game_state

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
  | Riddle of string * string (*question, answer*)
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
  success_msg : string;
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

(*Chest puzzle id should be 1*)
(* starting room puzzles *)
let chest_puzzle =
  Puzzle.make ~id:0
    ~puzzle_type:(Trivia ("What is a female camel called?", "cow"))
    ~deps:[]
    ~success_msg:
      "Riches uncovered, but danger remains. Seek the sarcophagus before it's \
       too late."

(*Casket puzzle id should be 2*)
let casket_puzzle =
  Puzzle.make ~id:1
    ~puzzle_type:
      (Trivia
         ("Does the following definition type check: let x = 2 +. 3.0", "no"))
    ~deps:[]
    ~success_msg:
      "You've awakened the mummy... now seize your chance to escape the tomb!"

(* corridor room puzzles *)
let h1_puzzle =
  Puzzle.make ~id:2
    ~puzzle_type:
      (Math
         ( "To decode this hieroglyphic, evaluate the result of this \
            expression: (18 / 5) + (18 mod 5)",
           6 ))
    ~deps:[]
    ~success_msg:
      "Seek the one who sits in silence, legs bent, watching from the side."

let h2_puzzle =
  Puzzle.make ~id:3
    ~puzzle_type:
      (Trivia
         ( "To decode this hieroglyphic, answer this question: How many humps \
            does a Bactrian camel have?",
           "2" ))
    ~deps:[]
    ~success_msg:
      "You must look for the one who stands tall with a bird’s gaze and a \
       serpent-shaped staff."

let h3_puzzle =
  Puzzle.make ~id:4
    ~puzzle_type:
      (Math
         ( "To decode this hieroglyphic, evaluate the result of this \
            expression: if 3 * 2 > 5 then 9 else 4",
           9 ))
    ~deps:[]
    ~success_msg:
      "To reveal the final clue, seek the twin currents that flow side by side."

let h4_puzzle =
  Puzzle.make ~id:5
    ~puzzle_type:
      (Trivia
         ( "To decode this hieroglyphic, answer this question: How many rows \
            of eyelashes does a camel have to protect them from the sand?",
           "2" ))
    ~deps:[]
    ~success_msg:
      "All clues are now in your hands. The challenge awaits you - find the \
       way to unlock your escape."

let lock_puzzle =
  Puzzle.make ~id:6
    ~puzzle_type:
      (Math
         ( "The lock will yield only to the four numbers drawn from the \
            corridor’s sacred glyphs. Enter them in a single four-digit \
            sequence: XXXX.",
           6292 ))
    ~deps:[]
    ~success_msg:
      "The code is correct. The corridor unlocks. Go, quickly, to the next \
       room!"

(* stairway room puzzles *)
let torch_puzzle =
  Puzzle.make ~id:7
    ~puzzle_type:
      (Trivia
         ( "What OCaml type represents a value that may or may not exist?",
           "option" ))
    ~deps:[]
    ~success_msg:
      "The lit torch helps you see more clearly. You can now see the spider \
       blocking your path."

let spider_puzzle =
  Puzzle.make ~id:8
    ~puzzle_type:
      (Trivia
         ( "Which OCaml concept ensures that once a value is created, it \
            cannot be changed?",
           "immutability" ))
    ~deps:[]
    ~success_msg:
      "The spider retreats, clearing your path forward. Now you must open the \
       door to escape this room!"

let doorknob_puzzle =
  Puzzle.make ~id:9
    ~puzzle_type:
      (Math ("Evaluate this expression: let x = 3 in let x = x + 4 in x", 7))
    ~deps:[] ~success_msg:"The door creaks open, revealing a way forward!"

let starting_room =
  Room.make ~id:0
    ~description:
      "The starting tomb with a chest and a sarcophagus that must be opened."
    ~puzzles:[ chest_puzzle; casket_puzzle ]
    ~room_deps:[]

let corridor_room =
  Room.make ~id:1
    ~description:
      "The next room, with a dark corridor and hieroglyphics that must be \
       decoded."
    ~puzzles:[ h1_puzzle; h2_puzzle; h3_puzzle; h4_puzzle; lock_puzzle ]
    ~room_deps:[]

let stairway_room =
  Room.make ~id:2
    ~description:"A cobwebbed staircase with a door that must be unlocked."
    ~puzzles:[ torch_puzzle; spider_puzzle; doorknob_puzzle ]
    ~room_deps:[]

let () =
  Puzzle.set_status chest_puzzle Puzzle.Unlocked;
  Puzzle.set_status casket_puzzle Puzzle.Unlocked;
  Puzzle.set_status h1_puzzle Puzzle.Unlocked;
  Puzzle.set_status h2_puzzle Puzzle.Unlocked;
  Puzzle.set_status h3_puzzle Puzzle.Unlocked;
  Puzzle.set_status h4_puzzle Puzzle.Unlocked;
  Puzzle.set_status lock_puzzle Puzzle.Unlocked;
  Puzzle.set_status torch_puzzle Puzzle.Unlocked;
  Puzzle.set_status spider_puzzle Puzzle.Unlocked;
  Puzzle.set_status doorknob_puzzle Puzzle.Unlocked

let init_game () =
  Game_state.init
    ~rooms:[ starting_room; corridor_room; stairway_room ]
    ~start:0
