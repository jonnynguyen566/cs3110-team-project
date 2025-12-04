val chest_puzzle : Puzzle.puzzle
(** Starting room puzzles *)

val casket_puzzle : Puzzle.puzzle

val h1_puzzle : Puzzle.puzzle
(** Corridor room puzzles *)

val h2_puzzle : Puzzle.puzzle
val h3_puzzle : Puzzle.puzzle
val h4_puzzle : Puzzle.puzzle
val lock_puzzle : Puzzle.puzzle

val torch_puzzle : Puzzle.puzzle
(** Stairway room puzzles *)

val spider_puzzle : Puzzle.puzzle
val doorknob_puzzle : Puzzle.puzzle

val scroll_puzzle : Puzzle.puzzle
(** Pottery room puzzles *)

val pot1_puzzle : Puzzle.puzzle
val pot2_puzzle : Puzzle.puzzle
val pot3_puzzle : Puzzle.puzzle
val lockedpot_puzzle : Puzzle.puzzle

val map_puzzle : Puzzle.puzzle
(** Treasure room puzzles *)

val oillamp_puzzle : Puzzle.puzzle
val lockedchest_puzzle : Puzzle.puzzle

val plant_puzzle : Puzzle.puzzle
(** Throne room puzzles *)

val statue_puzzle : Puzzle.puzzle
val throne_puzzle : Puzzle.puzzle

val hourglass_puzzle : Puzzle.puzzle
(** Ending room puzzles *)

val horus_puzzle : Puzzle.puzzle
val sphinx_puzzle : Puzzle.puzzle

val starting_room : Room.room
(** Room definitions *)

val corridor_room : Room.room
val stairway_room : Room.room
val pottery_room : Room.room
val treasure_room : Room.room
val throne_room : Room.room
val ending_room : Room.room

val init_game : unit -> Game_state.t
(** Initialize the game state *)
