open Room
open Puzzle
open Hints

type t = {
  mutable current_room : Room.room;
  rooms : Room.room list;
  mutable solved : int list;
  mutable start_time : float option;
  mutable end_time : float option;
}

let current_room gs = gs.current_room
let solved_puzzles gs = gs.solved

let init ~rooms ~start =
  let start_room = List.find (fun r -> Room.room_id r = start) rooms in
  start_room.status <- Accessible;
  {
    current_room = start_room;
    rooms;
    solved = [];
    start_time = None;
    (* Timer not started yet *)
    end_time = None;
  }

let start_timer gs =
  if gs.start_time = None then gs.start_time <- Some (Unix.time ())

let all_rooms gs = gs.rooms

let goto_next_room gs =
  let accessible = List.filter (fun r -> Room.status r = Accessible) gs.rooms in
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

let is_finished gs =
  let all_puzzles = List.concat (List.map Room.puzzles gs.rooms) in
  List.for_all (fun p -> Puzzle.status p = Puzzle.Solved) all_puzzles

let solve_puzzle gs ~puzzle_id =
  if not (List.mem puzzle_id gs.solved) then gs.solved <- puzzle_id :: gs.solved;

  List.iter
    (fun room ->
      List.iter (fun p -> Puzzle.try_unlock gs.solved p) (Room.puzzles room))
    gs.rooms;

  List.iter
    (fun room -> Room.try_unlock room ~solved_puzzles:gs.solved)
    gs.rooms;
  if is_finished gs && gs.end_time = None then
    gs.end_time <- Some (Unix.time ())

let elapsed_time gs =
  match gs.start_time with
  | None -> 0.0
  | Some t_start -> (
      match gs.end_time with
      | Some t_end -> t_end -. t_start
      | None -> Unix.time () -. t_start)

let format_time seconds =
  let s = int_of_float seconds in
  let h = s / 3600 in
  let m = s mod 3600 / 60 in
  let s = s mod 60 in
  if h > 0 then Printf.sprintf "%02d:%02d:%02d" h m s
  else Printf.sprintf "%02d:%02d" m s

let ending_message =
  "You've left the shadows \n\
  \ and the troubles behind. \n\
  \ The Pharoah's riddle is solved, \n\
  \ and the open sands await. \n\
  \ Well played, explorer."

let intro_message =
  "Welcome to the escape room! Explore the rooms and solve puzzles to progress."
