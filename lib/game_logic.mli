(** Starting room puzzles *)
val chest_puzzle : Puzzle.puzzle
val casket_puzzle : Puzzle.puzzle

(** Corridor room puzzles *)
val h1_puzzle : Puzzle.puzzle
val h2_puzzle : Puzzle.puzzle
val h3_puzzle : Puzzle.puzzle
val h4_puzzle : Puzzle.puzzle
val lock_puzzle : Puzzle.puzzle

(** Stairway room puzzles *)
val torch_puzzle : Puzzle.puzzle
val spider_puzzle : Puzzle.puzzle
val doorknob_puzzle : Puzzle.puzzle

(** Pottery room puzzles *)
val scroll_puzzle : Puzzle.puzzle
val pot1_puzzle : Puzzle.puzzle
val pot2_puzzle : Puzzle.puzzle
val pot3_puzzle : Puzzle.puzzle
val lockedpot_puzzle : Puzzle.puzzle

(** Treasure room puzzles *)
val map_puzzle : Puzzle.puzzle
val oillamp_puzzle : Puzzle.puzzle
val lockedchest_puzzle : Puzzle.puzzle

(** Throne room puzzles *)
val plant_puzzle : Puzzle.puzzle
val statue_puzzle : Puzzle.puzzle
val throne_puzzle : Puzzle.puzzle

(** Room definitions *)
val starting_room : Room.room
val corridor_room : Room.room
val stairway_room : Room.room
val pottery_room : Room.room
val treasure_room : Room.room
val throne_room : Room.room

(** Initialize the game state *)
val init_game : unit -> Game_state.t