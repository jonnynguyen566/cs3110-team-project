open Room
open Puzzle
open Hints

type t = {
  mutable current_room : Room.room;
  rooms : Room.room list;
  mutable solved : int list; (*Global list of solved puzzle ids*)
}

let current_room gs = gs.current_room
let solved_puzzles gs = gs.solved
let all_rooms gs = gs.rooms

(*Helper getter for GUI to access to room_id only, prevents exposing the whole
  room object and giving frontend access to mutable field*)
let get_current_room_id gs = Room.room_id gs.current_room

let accessible_rooms gs =
  List.filter (fun r -> Room.status r = Accessible) gs.rooms

let init ~rooms ~start =
  let start_room = List.find (fun r -> Room.room_id r = start) rooms in
  start_room.status <- Accessible;
  { current_room = start_room; rooms; solved = [] }

let get_room gs id = List.find_opt (fun r -> Room.room_id r = id) gs.rooms
let set_current_room gs room = gs.current_room <- room

let goto_next_room gs =
  let accessible = accessible_rooms gs in
  match accessible with
  | [] -> ()
  | rs ->
      let idx =
        List.find_index
          (fun r -> Room.room_id r = Room.room_id gs.current_room)
          rs
      in
      let next =
        match idx with
        | None -> List.hd rs
        | Some i -> List.nth rs ((i + 1) mod List.length rs)
      in
      gs.current_room <- next

let solve_puzzle gs ~puzzle_id =
  if not (List.mem puzzle_id gs.solved) then gs.solved <- puzzle_id :: gs.solved;

<<<<<<< HEAD
=======
  (* Unlock puzzles inside rooms *)
>>>>>>> 1a5e316 (implemented each interface)
  List.iter
    (fun room ->
      List.iter (fun p -> Puzzle.try_unlock gs.solved p) (Room.puzzles room))
    gs.rooms;

<<<<<<< HEAD
=======
  (* Unlock rooms dependent on puzzles *)
>>>>>>> 1a5e316 (implemented each interface)
  List.iter
    (fun room -> Room.try_unlock room ~solved_puzzles:gs.solved)
    gs.rooms

let is_finished gs =
<<<<<<< HEAD
=======
  (* Game is finished when ALL puzzles in ALL rooms are solved *)
>>>>>>> 1a5e316 (implemented each interface)
  let all_puzzles = List.concat (List.map Room.puzzles gs.rooms) in
  List.for_all (fun p -> Puzzle.status p = Puzzle.Solved) all_puzzles

let ending_message =
  "Congratulations! You've solved all the puzzles and escaped!"

let intro_message =
  "Welcome to the escape room! Explore the rooms and solve puzzles to progress."
