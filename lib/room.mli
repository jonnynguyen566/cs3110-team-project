(** Abstract room status, accessibility of the room *)
type room_status =
  | Inaccessible
  | Accessible

(** Abstract type representing a room in the escape game. *)
type room = {
  id : int;
  description : string;
  puzzles : Puzzle.puzzle list;
  room_deps : int list;
  mutable status : room_status;
  intro_msg : string
}

(** [room_id] is an accessor that returns the unique ID of room [r]. *)
val room_id : room -> int
(** [status] is an accessor that returns the current status of room [r]. *)
val status : room -> room_status
(** [puzzles] is an accessor that returns the list of puzzles in room [r]. *)
val puzzles : room -> Puzzle.puzzle list
(** [intro_message] is an accessor that returns the introductory message of room [r]. *)
val intro_message : room -> string

(** [make] creates a new room with the given parameters
    - [id]:          globally unique room ID  
    - [description]: textual description of the room  
    - [puzzles]:     list of puzzles contained in the room  
    - [room_deps]:   list of room IDs that must be fulfilled before this one can unlock  
    - [intro_msg]:   text displayed upon entering the room for the first time  *)
val make :
  id:int ->
  description:string ->
  puzzles:Puzzle.puzzle list ->
  room_deps:int list ->
  intro_msg:string ->
  room

(** [room_fulfilled r] returns [true] if all puzzles in room [r] are solved. *)
val room_fulfilled : room -> bool
(** [try_unlock r ~solved_puzzles] unlocks room [r] by setting its status to
    [Accessible] if all of its puzzle dependencies appear in [solved_puzzles]. *)
val try_unlock : room -> solved_puzzles:int list -> unit
(** [is_accessible r] returns [true] if room [r] is currently in the [Accessible] state. *)
val is_accessible : room -> bool
(** [attempt_enter r ~solved_puzzles] attempts to enter room [r].
    Returns [true] if the room becomes accessible (either already accessible
    or successfully unlocked by checking dependencies), and [false] otherwise. *)
val attempt_enter : room -> solved_puzzles:int list -> bool