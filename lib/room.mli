type room_status =
  | Inaccessible
  | Accessible

type room = {
  id : int;
  description : string;
  puzzles : Puzzle.puzzle list;
  room_deps : int list;
  mutable status : room_status;
  intro_msg : string
}

val room_id : room -> int
val status : room -> room_status
val puzzles : room -> Puzzle.puzzle list
val intro_message : room -> string

val make :
  id:int ->
  description:string ->
  puzzles:Puzzle.puzzle list ->
  room_deps:int list ->
  intro_msg:string ->
  room
(** Create a new room *)
val room_fulfilled : room -> bool
val try_unlock : room -> solved_puzzles:int list -> unit
(** Unlock room if dependencies are solved *)
val is_accessible : room -> bool
val attempt_enter : room -> solved_puzzles:int list -> bool