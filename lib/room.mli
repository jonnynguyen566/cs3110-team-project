type room_status =
  | Inaccessible
  | Accessible

type room = {
  id : int;
  description : string;
  puzzles : Puzzle.puzzle list;
  room_deps : int list;
  mutable status : room_status;
}

val room_id : room -> int
val status : room -> room_status
val puzzles : room -> Puzzle.puzzle list

val make :
  id:int ->
  description:string ->
  puzzles:Puzzle.puzzle list ->
  room_deps:int list ->
  room
(** Create a new room *)

val try_unlock : room -> solved_puzzles:int list -> unit
(** Unlock room if dependencies are solved *)
