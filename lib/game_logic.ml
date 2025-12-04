open Puzzle
open Room
open Game_state

let global_puzzleid_counter = ref 0
let global_roomid_counter = ref 0

(*Helper Function: Use this function every time when creating a new puzzle to
  ensure we are generating unique puzzle ids*)
let new_puzzle_id () =
  let pid = !global_puzzleid_counter in
  global_puzzleid_counter := pid + 1;
  pid

(*Helper Function: Use this function every time when creating a new room to
  ensure we are generating unique room ids*)
let new_room_id () =
  let rid = !global_roomid_counter in
  global_roomid_counter := rid + 1;
  rid

let chest_id = new_puzzle_id ()
let casket_id = new_puzzle_id ()
let h1_id = new_puzzle_id ()
let h2_id = new_puzzle_id ()
let h3_id = new_puzzle_id ()
let h4_id = new_puzzle_id ()
let lock_id = new_puzzle_id ()
let torch_id = new_puzzle_id ()
let spider_id = new_puzzle_id ()
let doorknob_id = new_puzzle_id ()
let scroll_id = new_puzzle_id ()
let pot1_id = new_puzzle_id ()
let pot2_id = new_puzzle_id ()
let pot3_id = new_puzzle_id ()
let lockedpot_id = new_puzzle_id ()
let map_id = new_puzzle_id ()
let oillamp_id = new_puzzle_id ()
let lockedchest_id = new_puzzle_id ()
let starting_room_id = new_room_id ()
let corridor_room_id = new_room_id ()
let stairway_room_id = new_room_id ()
let pottery_room_id = new_room_id ()

(*Chest puzzle id should be 1*)
(* starting room puzzles *)
let chest_puzzle =
  Puzzle.make ~id:chest_id
    ~puzzle_type:(Trivia ("What is a female camel called?", "cow"))
    ~deps:[]
    ~success_msg:
      "Riches uncovered, but danger remains. Seek the sarcophagus before it's \
       too late."

(*Casket puzzle id should be 2*)
let casket_puzzle =
  Puzzle.make ~id:casket_id
    ~puzzle_type:
      (Trivia
         ("Does the following definition type check: let x = 2 +. 3.0", "no"))
    ~deps:[ chest_id ]
    ~success_msg:
      "You've awakened the mummy... now seize your chance to escape the tomb!"

(* corridor room puzzles *)
let h1_puzzle =
  Puzzle.make ~id:h1_id
    ~puzzle_type:
      (Math
         ( "To decode this hieroglyphic, evaluate the result of this \
            expression: (18 / 5) + (18 mod 5)",
           6 ))
    ~deps:[]
    ~success_msg:
      "Seek the one who sits in silence, legs bent, watching from the side."

let h2_puzzle =
  Puzzle.make ~id:h2_id
    ~puzzle_type:
      (Trivia
         ( "To decode this hieroglyphic, answer this question: How many humps \
            does a Bactrian camel have?",
           "2" ))
    ~deps:[ h1_id ]
    ~success_msg:
      "You must look for the one who stands tall with a bird’s gaze and a \
       serpent-shaped staff."

let h3_puzzle =
  Puzzle.make ~id:h3_id
    ~puzzle_type:
      (Math
         ( "To decode this hieroglyphic, evaluate the result of this \
            expression: if 3 * 2 > 5 then 9 else 4",
           9 ))
    ~deps:[ h2_id ]
    ~success_msg:
      "To reveal the final clue, seek the twin currents that flow side by side."

let h4_puzzle =
  Puzzle.make ~id:h4_id
    ~puzzle_type:
      (Trivia
         ( "To decode this hieroglyphic, answer this question: How many rows \
            of eyelashes does a camel have to protect them from the sand?",
           "2" ))
    ~deps:[ h3_id ]
    ~success_msg:
      "All clues are now in your hands. The challenge awaits you - find the \
       way to unlock your escape."

let lock_puzzle =
  Puzzle.make ~id:lock_id
    ~puzzle_type:
      (Math
         ( "The lock will yield only to the four numbers drawn from the \
            corridor’s sacred glyphs. Enter them in a single four-digit \
            sequence: XXXX.",
           6292 ))
    ~deps:[ h4_id ]
    ~success_msg:
      "The code is correct. The corridor unlocks. Go, quickly, to the next \
       room!"

(* stairway room puzzles *)
let torch_puzzle =
  Puzzle.make ~id:torch_id
    ~puzzle_type:
      (Trivia
         ( "What OCaml type represents a value that may or may not exist?",
           "option" ))
    ~deps:[]
    ~success_msg:
      "The lit torch helps you see more clearly. You can now see the spider \
       blocking your path."

let spider_puzzle =
  Puzzle.make ~id:spider_id
    ~puzzle_type:
      (Trivia
         ( "Which OCaml concept ensures that once a value is created, it \
            cannot be changed?",
           "immutability" ))
    ~deps:[ torch_id ]
    ~success_msg:
      "The spider retreats, clearing your path forward. Now you must open the \
       door to escape this room!"

let doorknob_puzzle =
  Puzzle.make ~id:doorknob_id
    ~puzzle_type:
      (Math ("Evaluate this expression: let x = 3 in let x = x + 4 in x", 7))
    ~deps:[ spider_id ]
    ~success_msg:"The door creaks open, revealing a way forward!"

(* pottery room puzzles *)
let scroll_puzzle =
  Puzzle.make ~id:scroll_id
    ~puzzle_type:
      (Riddle
         ( "To find the puzzle which you seek, look at lists; they're not so \
            weak.\n\
            One piece in front, a trail behind,\n\
            with brackets keeping all aligned.\n\
            Split in two, I never fail:\n\
            my front is head, my back is ____.",
           "tail" ))
    ~deps:[]
    ~success_msg:
      "The head comes first, but the tails prevail! Now seek three pots to \
       lead the trail."

let pot1_puzzle =
  Puzzle.make ~id:pot1_id
    ~puzzle_type:
      (Trivia
         ( "What is the symbol for appending to a list in OCaml? (Type the \
            actual symbol)",
           "@" ))
    ~deps:[ scroll_id ]
    ~success_msg:"This is #1. Be sure to remember! Explore the next two pots."

let pot2_puzzle =
  Puzzle.make ~id:pot2_id
    ~puzzle_type:
      (Trivia
         ( "I know no one uses this anymore... but what's the pound key? (The \
            actual symbol)",
           "#" ))
    ~deps:[ pot1_id ] ~success_msg:"This is #2. Again, don't forget... "

let pot3_puzzle =
  Puzzle.make ~id:pot3_id
    ~puzzle_type:(Trivia ("What is the symbol for the modulo operator?", "%"))
    ~deps:[ pot2_id ]
    ~success_msg:
      "This is #3. Perhaps these symbols would be useful in opening a lock \
       somewhere."

let lockedpot_puzzle =
  Puzzle.make ~id:lockedpot_id
    ~puzzle_type:
      (Riddle ("Enter the three symbol code to unchain this pot.", "@#%"))
    ~deps:[ pot3_id ]
    ~success_msg:"This gave you a key to the next room. Nice. So long."

(* Treasure room puzzles *)
let map_puzzle =
  Puzzle.make ~id:map_id
    ~puzzle_type:
      (Trivia
         ( "Wow, a map! What OCaml higher order programming function lets you \
            select elements from a list based on a predicate function? (Hint: \
            it's not List.Map)",
           "List.Filter" ))
    ~deps:[] ~success_msg:"I think this map marked X on the oil lamps."

let oillamp_puzzle =
  Puzzle.make ~id:oillamp_id
    ~puzzle_type:
      (Trivia
         ( "I hope this bottle has a genie in it. What movie is known for \
            having a big blue genie that comes out of a magical lamp? ",
           "Aladdin" ))
    ~deps:[ map_id ]
    ~success_msg:
      "Perhaps it's time to look at the treasure chest in this treasure room."

let lockedchest_puzzle =
  Puzzle.make ~id:lockedchest_id
    ~puzzle_type:
      (Riddle
         ( "This lock requires three numbers. 1: How many places can I find a \
            genie in here? 2: How many treasure chests are there? 3: How many \
            things can I use as a light source?",
           "332" ))
    ~deps:[ oillamp_id ] ~success_msg:"Goodbye treasure room :("

let starting_room =
  Room.make ~id:starting_room_id
    ~description:
      "The starting tomb with a chest and a sarcophagus that must be opened."
    ~puzzles:[ chest_puzzle; casket_puzzle ]
    ~room_deps:[]

let corridor_room =
  Room.make ~id:corridor_room_id
    ~description:
      "The next room, with a dark corridor and hieroglyphics that must be \
       decoded."
    ~puzzles:[ h1_puzzle; h2_puzzle; h3_puzzle; h4_puzzle; lock_puzzle ]
    ~room_deps:[]

let stairway_room =
  Room.make ~id:stairway_room_id
    ~description:"A cobwebbed staircase with a door that must be unlocked."
    ~puzzles:[ torch_puzzle; spider_puzzle; doorknob_puzzle ]
    ~room_deps:[]

let pottery_room =
  Room.make ~id:pottery_room_id
    ~description:
      "A room of scrolls and pots with a final pot waiting to be opened."
    ~puzzles:
      [ scroll_puzzle; pot1_puzzle; pot2_puzzle; pot3_puzzle; lockedpot_puzzle ]
    ~room_deps:[]

let treasure_room =
  Room.make ~id:3
    ~description:
      "A with treasures and a map with a final chest that must be unlocked."
    ~puzzles:[ map_puzzle; oillamp_puzzle; lockedchest_puzzle ]
    ~room_deps:[]

let () =
  Puzzle.set_status chest_puzzle Puzzle.Unlocked;
  Puzzle.set_status h1_puzzle Puzzle.Unlocked;
  Puzzle.set_status torch_puzzle Puzzle.Unlocked;
  Puzzle.set_status spider_puzzle Puzzle.Unlocked;
  Puzzle.set_status doorknob_puzzle Puzzle.Unlocked;
  Puzzle.set_status scroll_puzzle Puzzle.Unlocked;
  Puzzle.set_status pot1_puzzle Puzzle.Unlocked;
  Puzzle.set_status pot2_puzzle Puzzle.Unlocked;
  Puzzle.set_status pot3_puzzle Puzzle.Unlocked;
  Puzzle.set_status lockedpot_puzzle Puzzle.Unlocked;
  Puzzle.set_status map_puzzle Puzzle.Unlocked;
  Puzzle.set_status oillamp_puzzle Puzzle.Unlocked;
  Puzzle.set_status lockedchest_puzzle Puzzle.Unlocked;
  Puzzle.set_status scroll_puzzle Puzzle.Unlocked

let init_game () =
  Game_state.init
    ~rooms:
      [
        starting_room; corridor_room; stairway_room; pottery_room; treasure_room;
      ]
    ~start:0
