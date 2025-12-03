type t

val init : rooms:Room.room list -> start:int -> t
val current_room : t -> Room.room
val get_current_room_id : t -> int
val all_rooms : t -> Room.room list
val accessible_rooms : t -> Room.room list
val get_room : t -> int -> Room.room option
val goto_next_room : t -> unit
val solve_puzzle : t -> puzzle_id:int -> unit
val solved_puzzles : t -> int list
val is_finished : t -> bool
val all_rooms : t -> Room.room list
val ending_message : string
val intro_message : string
