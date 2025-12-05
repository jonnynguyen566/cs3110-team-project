(** AF: The module’s values represent the static content of the escape room:
    - Each [Puzzle.puzzle] value corresponds to one in-game puzzle.
    - Each [Room.room] value corresponds to one in-game room containing puzzles.

    RI:
    - Puzzle IDs must be globally unique. The module ensures this by
      incrementing a global counter through [new_puzzle_id].
    - Room IDs must be globally unique via [new_room_id].
    - Dependencies for each puzzle must reference earlier puzzle IDs only.
    - All puzzle values remain mutable but must be used within their intended
      room and dependency structure.
    - Room definitions must contain only puzzles that belong to that room.
    - Initialization sets appropriate puzzles to [Unlocked] status before the
      game begins. *)

val chest_puzzle : Puzzle.puzzle
(** Puzzle found in the starting room. *)

val casket_puzzle : Puzzle.puzzle
(** A second puzzle in the starting room, unlocked after the chest puzzle. *)

val h1_puzzle : Puzzle.puzzle
(** Puzzles found in the corridor room. *)

val h2_puzzle : Puzzle.puzzle
val h3_puzzle : Puzzle.puzzle
val h4_puzzle : Puzzle.puzzle
val lock_puzzle : Puzzle.puzzle

val torch_puzzle : Puzzle.puzzle
(** Puzzles found in the stairway room. *)

val spider_puzzle : Puzzle.puzzle
val doorknob_puzzle : Puzzle.puzzle

val scroll_puzzle : Puzzle.puzzle
(** Puzzles found in the pottery room. *)

val pot1_puzzle : Puzzle.puzzle
val pot2_puzzle : Puzzle.puzzle
val pot3_puzzle : Puzzle.puzzle
val lockedpot_puzzle : Puzzle.puzzle

val map_puzzle : Puzzle.puzzle
(** Puzzles found in the treasure room. *)

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
(** The first room the player sees. *)

val corridor_room : Room.room
(** Room containing hieroglyphic puzzles. *)

val stairway_room : Room.room
(** Staircase with a locked door. *)

val pottery_room : Room.room
(** Room of pots and scroll puzzles. *)

val treasure_room : Room.room
(** Treasure chamber with three puzzles. *)

val throne_room : Room.room
(** The pharaoh’s throne chamber. *)

val ending_room : Room.room
(** The final room. *)

val init_game : unit -> Game_state.t
(** [init_game ()] constructs the full game, initializing all rooms and puzzles,
    setting puzzles to [Unlocked], and returning a fresh game state. *)
