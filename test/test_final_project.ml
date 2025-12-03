open OUnit2
open Cs3110teamproject.Hints
open Cs3110teamproject.Puzzle
open Cs3110teamproject.Room
open Cs3110teamproject.Game_state

(*Helper Functions for creating puzzles to test with*)
let mk_riddle ?(deps = []) id q a =
  Cs3110teamproject.Puzzle.make ~id ~puzzle_type:(Riddle (q, a)) ~deps

let mk_unlocked_riddle id q a =
  let r = mk_riddle id q a in
  mark_solved r;
  r

let mk_trivia ?(deps = []) id q a =
  Cs3110teamproject.Puzzle.make ~id ~puzzle_type:(Trivia (q, a)) ~deps

let mk_math ?(deps = []) id q n =
  Cs3110teamproject.Puzzle.make ~id ~puzzle_type:(Math (q, n)) ~deps

(*Helper Functions for creating rooms to test with*)
let mk_room ?(room_deps = []) id desc puzzles =
  Cs3110teamproject.Room.make ~id ~description:desc ~puzzles ~room_deps

(* Helper function to initialize game state *)
let mk_gamestate rooms start = Cs3110teamproject.Game_state.init ~rooms ~start

(* ------------------------------------------------------------- *)
(* HINT MODULE TESTS                                             *)
(* ------------------------------------------------------------- *)

(* Basic test to make sure that registering a hint changes the hash table and is
   accessible with get *)
let test_register_and_get_hint _ =
  register_hint ~puzzle_id:1 "hint";
  assert_equal (Some "hint") (get_hint 1)

(* A puzzle that doesn't exist should not have a hint in the hash table *)
let test_get_invalid_hint _ = assert_equal None (get_hint 999)

(* Registering a different hint for the same puzzle should overwrite the old
   hint *)
let test_change_hint _ =
  register_hint ~puzzle_id:5 "hint1";
  register_hint ~puzzle_id:5 "hint2";
  assert_equal (Some "hint2") (get_hint 5)

(* Registering two hints *)
let test_two_hints _ =
  register_hint ~puzzle_id:1 "hint1";
  register_hint ~puzzle_id:2 "hint2";
  assert_equal (Some "hint1") (get_hint 1);
  assert_equal (Some "hint2") (get_hint 2)

(* Registering three hints *)
let test_three_hints _ =
  register_hint ~puzzle_id:1 "hint1";
  register_hint ~puzzle_id:2 "hint2";
  register_hint ~puzzle_id:3 "hint3";
  assert_equal (Some "hint1") (get_hint 1);
  assert_equal (Some "hint2") (get_hint 2);
  assert_equal (Some "hint3") (get_hint 3)

(* Registering three hints and overwriting all of them *)
let test_change_three_hints _ =
  register_hint ~puzzle_id:1 "hint1";
  register_hint ~puzzle_id:2 "hint2";
  register_hint ~puzzle_id:3 "hint3";
  register_hint ~puzzle_id:1 "newhint1";
  register_hint ~puzzle_id:2 "newhint2";
  register_hint ~puzzle_id:3 "newhint3";
  assert_equal (Some "newhint1") (get_hint 1);
  assert_equal (Some "newhint2") (get_hint 2);
  assert_equal (Some "newhint3") (get_hint 3)

(* Combining puzzle creation and hint registration*)
let test_hint_with_puzzle _ =
  let p = mk_riddle 67 "What is the best CS class" "3110" in
  register_hint ~puzzle_id:(puzzle_id p) "Add 3000 and 110";
  assert_equal (Some "Add 3000 and 110") (get_hint (puzzle_id p))

(* ------------------------------------------------------------- *)
(* GAME STATE MODULE TESTS                                       *)
(* ------------------------------------------------------------- *)

(* Tests making just two rooms with the starting room being accessible
   immidietly*)
let test_start_room_accessible _ =
  let p = mk_unlocked_riddle 1 "riddle" "answer" in
  let r1 = mk_room 1 "Init room" [ p ] in
  let r2 = mk_room 2 "Next room" [] in
  let gs = mk_gamestate [ r1; r2 ] 1 in
  assert_equal Cs3110teamproject.Room.Accessible
    (Cs3110teamproject.Room.status (current_room gs));
  assert_equal 1 (room_id (current_room gs))

(* Tests functionality of all rooms to return a list of all the rooms in the
   current game state*)
let test_all_rooms _ =
  let r1 = mk_room 1 "Init room" [] in
  let r2 = mk_room 2 "Next room" [] in
  let gs = mk_gamestate [ r1; r2 ] 1 in
  assert_equal [ r1; r2 ] (all_rooms gs)

(* Tests if solved puzzles are being tracked *)
let test_solve_puzzle_adds_to_solved _ =
  let p = mk_unlocked_riddle 3 "Am I awesome?" "yes" in
  let r = mk_room 1 "Room" [ p ] in
  let gs = mk_gamestate [ r ] 1 in
  Cs3110teamproject.Game_state.solve_puzzle gs ~puzzle_id:(puzzle_id p);
  assert_bool "Puzzle ID should be in solved list"
    (List.mem (puzzle_id p) (solved_puzzles gs))

(* Solved puzzles should unlock the puzzles that are dependent on them *)
let test_solve_puzzle_unlocks_dependent_puzzles _ =
  let p1 = mk_unlocked_riddle 1 "True or False" "True" in
  let p2 = mk_riddle ~deps:[ 1 ] 2 "Is Clarkson the goat" "yes ofc" in
  let r = mk_room 1 "Room" [ p1; p2 ] in
  let gs = mk_gamestate [ r ] 1 in

  (* p2 is Locked before solving p1 *)
  assert_equal Cs3110teamproject.Puzzle.Locked
    (Cs3110teamproject.Puzzle.status p2);

  Cs3110teamproject.Game_state.solve_puzzle gs ~puzzle_id:1;

  (* Now p2 should be unlocked *)
  assert_equal Cs3110teamproject.Puzzle.Unlocked
    (Cs3110teamproject.Puzzle.status p2)

(* Solving all of the puzzles that a room depends on should unlock the room *)
let test_solve_puzzle_unlocks_rooms _ =
  let p1 = mk_unlocked_riddle 1 "Q1" "A1" in
  let r1 = mk_room 1 "Room1" [ p1 ] in
  let r2 = mk_room ~room_deps:[ 1 ] 2 "Room2" [] in
  let gs = mk_gamestate [ r1; r2 ] 1 in

  (* Room2 is inaccessible initially *)
  assert_equal Cs3110teamproject.Room.Inaccessible
    (Cs3110teamproject.Room.status r2);

  Cs3110teamproject.Game_state.solve_puzzle gs ~puzzle_id:1;

  (* Room2 should unlock *)
  assert_equal Cs3110teamproject.Room.Accessible
    (Cs3110teamproject.Room.status r2)

(* Test ability to move to an accessible room *)
let test_goto_next_room_basic _ =
  let r1 = mk_room 1 "Starting room" [] in
  let r2 = mk_room 2 "Accessible room" [] in
  r1.status <- Cs3110teamproject.Room.Accessible;
  r2.status <- Cs3110teamproject.Room.Accessible;

  let gs = mk_gamestate [ r1; r2 ] 1 in

  (* Starting in r1 → next accessible is r2 *)
  Cs3110teamproject.Game_state.goto_next_room gs;
  assert_equal 2 (room_id (current_room gs))

(* Moving to the next room from the last room should wrap around back to the
   first room *)

let test_goto_next_room_wraps_around _ =
  let r1 = mk_room 1 "First Room" [] in
  let r2 = mk_room 2 "Last Room" [] in
  r1.status <- Cs3110teamproject.Room.Accessible;
  r2.status <- Cs3110teamproject.Room.Accessible;

  let gs = mk_gamestate [ r1; r2 ] 2 in

  (* Starting in r2 → next accessible wraps to r1 *)
  Cs3110teamproject.Game_state.goto_next_room gs;
  assert_equal 1 (room_id (current_room gs))

(* Game state should be returned as true when all puzzlesare solved*)
let test_is_finished_true_when_all_solved _ =
  let p1 = mk_unlocked_riddle 1 "Easy" "Peezy" in
  let p2 = mk_unlocked_riddle 2 "Lemon" "Squeezy" in
  let r = mk_room 1 "Room" [ p1; p2 ] in
  let gs = mk_gamestate [ r ] 1 in

  (* Mark all puzzles solved *)
  Cs3110teamproject.Puzzle.mark_solved p1;
  Cs3110teamproject.Puzzle.mark_solved p2;

  assert_bool "Game should be finished"
    (Cs3110teamproject.Game_state.is_finished gs)

(* Game state should be returned as false when all puzzles are unsolved*)
let test_is_finished_false_when_unfinished _ =
  let p1 = mk_riddle 1 "No so easy" "or peezy" in
  let p2 = mk_riddle 2 "Lime" "crushy" in
  let r = mk_room 1 "Room" [ p1; p2 ] in
  let gs = mk_gamestate [ r ] 1 in

  Cs3110teamproject.Puzzle.mark_solved p1;

  (* p2 is still Unlocked, not Solved *)
  assert_bool "Game should NOT be finished"
    (not (Cs3110teamproject.Game_state.is_finished gs))

let tests =
  "test suite"
  >::: [
         (* Hint Tests *)
         "Basic register and get hint" >:: test_register_and_get_hint;
         "An invalid hint should return none" >:: test_get_invalid_hint;
         "Changes a hint in the table" >:: test_change_hint;
         "Two hints" >:: test_two_hints;
         "Three hints" >:: test_three_hints;
         "Changes three hints" >:: test_change_three_hints;
         "Basic hint and puzzle interaction" >:: test_hint_with_puzzle;
         (* Game State Tests *)
         "First room should be accessible" >:: test_start_room_accessible;
         "Access list of all the rooms in the current game state"
         >:: test_all_rooms;
         "Solved puzzles should be kept track of in a list"
         >:: test_solve_puzzle_adds_to_solved;
         "Solving a puzzles dependencies should unlock the puzzle "
         >:: test_solve_puzzle_unlocks_dependent_puzzles;
         "Room dependencies being solved should unlock the room"
         >:: test_solve_puzzle_unlocks_rooms;
         "Going to the next room should change the room"
         >:: test_goto_next_room_basic;
         "Going to the next room when the current room is the last room loops \
          back to the first room" >:: test_goto_next_room_wraps_around;
         "Returns true when all puzzles are solved"
         >:: test_is_finished_true_when_all_solved;
         "Returns false when there are some unsolved puzzles"
         >:: test_is_finished_false_when_unfinished;
       ]

let _ = run_test_tt_main tests
