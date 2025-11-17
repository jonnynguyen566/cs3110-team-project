type t

val init : rooms:Room.room list -> start:int -> t
val current_room : t -> Room.room
val goto_next_room : t -> unit
val solve_puzzle : t -> puzzle_id:int -> unit
val solved_puzzles : t -> int list
<<<<<<< HEAD
val is_finished : t -> bool
=======
(** Return list of solved puzzle ids *)

val is_finished : t -> bool
(** Is the game finished? *)

>>>>>>> 1a5e316 (implemented each interface)
val ending_message : string
val intro_message : string
