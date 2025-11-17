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

let room_id r = r.id
let status r = r.status
let puzzles r = r.puzzles

let make ~id ~description ~puzzles ~room_deps =
  { id; description; puzzles; room_deps; status = Inaccessible }

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
