type room_status =
  | Inaccessible
  | Accessible

type room = {
  id : int;
  description : string;
  puzzles : Puzzle.puzzle list;
  room_deps : int list; (*Rooms DEPEND ON PUZZLES, NOT OTHER ROOMS; this is an int list of puzzle ids that must be solved to unlock this room*)
  mutable status : room_status;
}

let room_id r = r.id
let status r = r.status
let puzzles r = r.puzzles

let make ~id ~description ~puzzles ~room_deps =
  { id; description; puzzles; room_deps; status = Inaccessible }


(*Helper function used to check if all puzzles in a room have been solved*)
let room_fulfilled room =
  List.for_all (fun p -> Puzzle.status p = Puzzle.Solved) room.puzzles

let try_unlock room ~solved_puzzles =
  match room.status with
<<<<<<< HEAD
  | Accessible -> ()
=======
  | Accessible -> () (* already open *)
>>>>>>> 1a5e316 (implemented each interface)
  | Inaccessible ->
      if List.for_all (fun id -> List.mem id solved_puzzles) room.room_deps then
        room.status <- Accessible

(*Is the room passed in the argument accessible? Returns bool*)
let is_accessible r =
  match r.status with
  | Accessible -> true
  | Inaccessible -> false


(*This is the function to attempt to enter a room whenever the
  arrow for the next room is clicked*)
let attempt_enter room ~solved_puzzles =
  (*First try to unlock the room*)
  try_unlock room ~solved_puzzles;
  (*Return whether the room is accessible after the attempt without mutating*)
  is_accessible room
