type t
(** Abstract type [t] represents the complete full state of game AF:
    - the room the player currently occupies
    - the list of rooms in the game and their accessibility
    - the list of solved puzzle IDs
    - the start time of the game
    - an end time shown after a finished game

    RI:
    - [current_room] is an element of [rooms].
    - Every puzzle ID in [solved_puzzles t] corresponds to a puzzle that exists.
    - A room is [Accessible] if all puzzle IDs in its [room_deps] appear in
      [solved_puzzles t].
    - No puzzle may change from [Solved] back to [Unlocked] or [Locked].
    - The timer satisfies:
    - [start_time] is set exactly once during [init].
    - [end_time] is [None] until the game is completed.
    - If [end_time] is [Some _], then [is_finished t = true]. *)

val init : rooms:Room.room list -> start:int -> t
(** [init ~rooms ~start] initializes a new game state.
    - [rooms] is the complete list of rooms in the game.
    - [start] is the ID of the starting room. The starting room becomes
      [Accessible], all others remain unchanged. The game timer begins
      immediately. *)

val start_timer : t -> unit
(** [start_timer t] resets and starts the internal timer for [t]. Typically used
    only for restarting or testing. *)

val current_room : t -> Room.room
(** [current_room t] returns the room the player is currently in. *)

val goto_next_room : t -> unit
(** [goto_next_room t] moves the player to the next accessible room, cycling in
    room-ID order. No effect if no rooms are accessible. *)

val solve_puzzle : t -> puzzle_id:int -> unit
(** [solve_puzzle t ~puzzle_id] records puzzle [puzzle_id] as solved and updates
    room and puzzle unlock status accordingly.
    - Idempotent: solving the same puzzle twice has no additional effect.
    - If all puzzles in the game become solved, the game timer stops. *)

val solved_puzzles : t -> int list
(** [solved_puzzles t] returns the list of puzzle IDs solved so far. *)

val is_finished : t -> bool
(** [is_finished t] returns [true] if all puzzles in all rooms are solved. *)

val all_rooms : t -> Room.room list
(** [all_rooms t] returns the list of all rooms in the game. *)

val elapsed_time : t -> float
(** [elapsed_time t] returns the number of seconds elapsed:
    - from the start of the game until now, if unfinished
    - from the start to [end_time], if finished *)

val format_time : float -> string
(** [format_time seconds] returns a human-readable string such as ["05:13"] for
    five minutes, thirteen seconds, or ["01:02:45"] for one hour, two minutes,
    forty-five seconds. *)

val ending_message : string
(** Message shown after winning the game. *)

val intro_message : string
(** Intro message shown at the start of the game. *)
