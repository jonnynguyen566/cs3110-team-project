open OUnit2
open Cs3110teamproject.Hints
open Cs3110teamproject.Puzzle
open Cs3110teamproject.Room
open Cs3110teamproject.Game_state

(*Helper Functions for creating puzzles to test with*)
let mk_riddle ?(deps = []) ?(success_msg = "Solved!") id q a =
  Cs3110teamproject.Puzzle.make ~id
    ~puzzle_type:(Riddle (q, a))
    ~deps ~success_msg

let mk_unlocked_riddle id q a =
  let r = mk_riddle id q a in
  mark_solved r;
  r

let mk_trivia ?(deps = []) ?(success_msg = "Solved!") id q a =
  Cs3110teamproject.Puzzle.make ~id
    ~puzzle_type:(Trivia (q, a))
    ~deps ~success_msg

let mk_math ?(deps = []) ?(success_msg = "Solved!") id q n =
  Cs3110teamproject.Puzzle.make ~id
    ~puzzle_type:(Math (q, n))
    ~deps ~success_msg

(*Helper Functions for creating rooms to test with*)
let mk_room ?(room_deps = []) id desc puzzles =
  Cs3110teamproject.Room.make ~id ~description:desc ~puzzles ~room_deps

(* Helper function to initialize game state *)
let mk_gamestate rooms start = Cs3110teamproject.Game_state.init ~rooms ~start

(* ------------------------------------------------------------- *)
(* PUZZLE MODULE TESTS                                           *)
(* ------------------------------------------------------------- *)

(*puzzle creation tests------------------------------------------*)
let test_make_riddle_basic _ =
  let p =
    mk_riddle 42
      " I have cities, but no houses. I have mountains, but no trees. I have \
       water, but no fish. What am I?"
      "map"
  in
  (* Check ID *)
  assert_equal 42 (Cs3110teamproject.Puzzle.puzzle_id p);
  (* New puzzles should be Locked *)
  assert_equal Cs3110teamproject.Puzzle.Locked
    (Cs3110teamproject.Puzzle.status p)

(*check answer tests---------------------------------------------*)
let test_check_answer_riddle _ =
  let p =
    mk_riddle 1
      " I have cities, but no houses. I have mountains, but no trees. I have \
       water, but no fish. What am I?"
      "map"
  in
  assert_equal true (check_answer p "map");
  assert_equal true (check_answer p "MAP");
  (*Testing caps*)
  assert_equal true (check_answer p "  map  ");
  (*Testing whitespace*)
  assert_equal false (check_answer p "donut")

let test_check_answer_math _ =
  let p = mk_math 2 "What is 2 + 2?" 4 in
  assert_equal true (check_answer p "4");
  assert_equal true (check_answer p " 4 ");
  (*Test whitespace*)
  assert_equal true (check_answer p "four");
  (*Test string form*)
  assert_equal true (check_answer p " four");
  (*Test string with whitespace*)
  assert_equal true (check_answer p "Four");
  (*Test word form with caps*)
  assert_equal false (check_answer p "3");
  assert_equal false (check_answer p "false")

let test_check_answer_trivia _ =
  let p = mk_trivia 3 "What is the capital of Egypt?" "Cairo" in
  assert_equal true (check_answer p "Cairo");
  assert_equal true (check_answer p "cairo");
  (*Testing lowercase*)
  assert_equal true (check_answer p "  Cairo  ");
  (*Testing whitespace*)
  assert_equal false (check_answer p "London")

(*try unlock tests----------------------------------------------*)
let test_try_unlock_no_deps _ =
  let p =
    mk_riddle 1
      " I have cities, but no houses. I have mountains, but no trees. I have \
       water, but no fish. What am I?"
      "map"
  in
  Cs3110teamproject.Puzzle.try_unlock [] p;
  assert_equal Cs3110teamproject.Puzzle.Unlocked
    (Cs3110teamproject.Puzzle.status p)

let test_try_unlock_with_unsolved_deps _ =
  let _p_dep =
    mk_riddle 1
      "I have cities, but no houses. I have mountains, but no trees. I have \
       water, but no fish. What am I?"
      "map"
  in
  let p =
    mk_riddle ~deps:[ 1 ] 2
      "What begins with the letter 'P,' ends with 'E,' and has thousands of \
       letters inside?"
      "post office"
  in
  (*The bracket is the passed in list of "global solved puzzles"*)
  Cs3110teamproject.Puzzle.try_unlock [] p;
  assert_equal Cs3110teamproject.Puzzle.Locked
    (Cs3110teamproject.Puzzle.status p)

let test_try_unlock_with_solved_deps _ =
  let p_dep =
    mk_riddle 1
      "I have cities, but no houses. I have mountains, but no trees. I have \
       water, but no fish. What am I?"
      "map"
  in
  let p =
    mk_riddle ~deps:[ 1 ] 2
      "What begins with the letter 'P,' ends with 'E,' and has thousands of \
       letters inside?"
      "post office"
  in
  (*The bracket is the passed in list of "global solved puzzles"*)
  Cs3110teamproject.Puzzle.try_unlock
    [ Cs3110teamproject.Puzzle.puzzle_id p_dep ]
    p;
  assert_equal Cs3110teamproject.Puzzle.Unlocked
    (Cs3110teamproject.Puzzle.status p)

let test_try_unlock_with_multiple_deps _ =
  let p_dep1 =
    mk_riddle 1
      "I have cities, but no houses. I have mountains, but no trees. I have \
       water, but no fish. What am I?"
      "map"
  in
  let p_dep2 = mk_math 2 "What is 2 + 2?" 4 in
  let p =
    mk_riddle ~deps:[ 1; 2 ] 3
      "What begins with the letter 'P,' ends with 'E,' and has thousands of \
       letters inside?"
      "post office"
  in
  (*The bracket is the passed in list of "global solved puzzles"*)
  Cs3110teamproject.Puzzle.try_unlock
    [
      Cs3110teamproject.Puzzle.puzzle_id p_dep1;
      Cs3110teamproject.Puzzle.puzzle_id p_dep2;
    ]
    p;
  assert_equal Cs3110teamproject.Puzzle.Unlocked
    (Cs3110teamproject.Puzzle.status p)

let test_try_unlock_already_unlocked _ =
  (* deps = [] means puzzle unlocks immediately *)
  let p =
    mk_riddle 1
      "I have cities, but no houses. I have mountains, but no trees. I have \
       water, but no fish. What am I?"
      "map"
  in

  (* Unlock it through the real API *)
  Cs3110teamproject.Puzzle.try_unlock [] p;
  assert_equal Cs3110teamproject.Puzzle.Unlocked
    (Cs3110teamproject.Puzzle.status p);

  (* Calling try_unlock again should leave it Unlocked *)
  Cs3110teamproject.Puzzle.try_unlock [] p;
  assert_equal Cs3110teamproject.Puzzle.Unlocked
    (Cs3110teamproject.Puzzle.status p)

let test_try_unlock_already_solved _ =
  let p =
    mk_riddle 1
      "I have cities, but no houses. I have mountains, but no trees. I have \
       water, but no fish. What am I?"
      "map"
  in

  (* First, unlock it properly *)
  Cs3110teamproject.Puzzle.try_unlock [] p;
  assert_equal Cs3110teamproject.Puzzle.Unlocked
    (Cs3110teamproject.Puzzle.status p);

  (* Now mark it solved *)
  Cs3110teamproject.Puzzle.mark_solved p;
  assert_equal Cs3110teamproject.Puzzle.Solved
    (Cs3110teamproject.Puzzle.status p);

  (* Calling try_unlock again should NOT revert or change status *)
  Cs3110teamproject.Puzzle.try_unlock [] p;
  assert_equal Cs3110teamproject.Puzzle.Solved
    (Cs3110teamproject.Puzzle.status p)

(*try unlock tests----------------------------------------------*)
let test_mark_solved _ =
  (* deps=[] so this unlocks immediately *)
  let p =
    mk_riddle 1
      "I have cities, but no houses. I have mountains, but no trees. I have \
       water, but no fish. What am I?"
      "map"
  in

  Cs3110teamproject.Puzzle.try_unlock [] p;
  assert_equal Cs3110teamproject.Puzzle.Unlocked
    (Cs3110teamproject.Puzzle.status p);

  (* Now mark solved *)
  Cs3110teamproject.Puzzle.mark_solved p;
  assert_equal Cs3110teamproject.Puzzle.Solved
    (Cs3110teamproject.Puzzle.status p)

(* ------------------------------------------------------------- *)
(* ROOM MODULE TEST                                              *)
(* ------------------------------------------------------------- *)
(*Room Creation Tests--------------------------------------------*)
let test_make_room_basic _ =
  let p1 =
    mk_riddle 1
      "I have cities, but no houses. I have mountains, but no trees. I have \
       water, but no fish. What am I?"
      "map"
  in
  let p2 = mk_math 2 "What is 2 + 2?" 4 in
  let room = mk_room 1 "Puzzle Room" [ p1; p2 ] in
  (* Check ID *)
  assert_equal 1 (Cs3110teamproject.Room.room_id room);
  (* New rooms should be Inaccessible *)
  assert_equal Cs3110teamproject.Room.Inaccessible
    (Cs3110teamproject.Room.status room)

(*room_fulfilled tests-------------------------------------------*)
let room_fulfilled_no_puzzles _ =
  let room = mk_room 1 "Empty Room" [] in
  assert_equal true (Cs3110teamproject.Room.room_fulfilled room)

let room_fulfilled_not_fulfilled _ =
  let p1 =
    mk_riddle 1
      "I have cities, but no houses. I have mountains, but no trees. I have \
       water, but no fish. What am I?"
      "map"
  in
  let p2 = mk_math 2 "What is 2 + 2?" 4 in
  let room = mk_room 2 "Puzzle Room" [ p1; p2 ] in
  (* Neither puzzle is solved yet *)
  assert_equal false (Cs3110teamproject.Room.room_fulfilled room)

let room_fullfilled_is_fulfilled _ =
  let p1 =
    mk_riddle 1
      "I have cities, but no houses. I have mountains, but no trees. I have \
       water, but no fish. What am I?"
      "map"
  in
  let p2 = mk_math 2 "What is 2 + 2?" 4 in
  let room = mk_room 3 "Puzzle Room" [ p1; p2 ] in
  (* Unlock and solve both puzzles *)
  Cs3110teamproject.Puzzle.try_unlock [] p1;
  Cs3110teamproject.Puzzle.mark_solved p1;
  Cs3110teamproject.Puzzle.try_unlock [] p2;
  Cs3110teamproject.Puzzle.mark_solved p2;
  assert_equal true (Cs3110teamproject.Room.room_fulfilled room)

(*try_unlock tests-----------------------------------------------*)
let test_try_unlock_room_no_deps _ =
  let room = mk_room 1 "No Deps Room" [] in
  Cs3110teamproject.Room.try_unlock room ~solved_puzzles:[];
  assert_equal Cs3110teamproject.Room.Accessible
    (Cs3110teamproject.Room.status room)

let test_try_unlock_accessible_room _ =
  let p1 =
    mk_riddle 1
      "I have cities, but no houses. I have mountains, but no trees. I have \
       water, but no fish. What am I?"
      "map"
  in
  let p2 = mk_math 2 "What is 2 + 2?" 4 in
  let room = mk_room 1 "Already Accessible Room" [ p1; p2 ] in
  Cs3110teamproject.Puzzle.try_unlock [] p1;
  Cs3110teamproject.Puzzle.mark_solved p1;
  room.status <- Cs3110teamproject.Room.Accessible;
  Cs3110teamproject.Room.try_unlock room ~solved_puzzles:[];
  assert_equal Cs3110teamproject.Room.Accessible
    (Cs3110teamproject.Room.status room)

let test_try_unlock_inaccessible_deps_solved _ =
  let p1 =
    mk_riddle 1
      "I have cities, but no houses. I have mountains, but no trees. I have \
       water, but no fish. What am I?"
      "map"
  in
  let p2 = mk_math 2 "What is 2 + 2?" 4 in
  let room = mk_room ~room_deps:[ 1; 2 ] 2 "Dependent Room" [ p1; p2 ] in
  Cs3110teamproject.Puzzle.try_unlock [] p1;
  Cs3110teamproject.Puzzle.mark_solved p1;
  Cs3110teamproject.Puzzle.try_unlock [] p2;
  Cs3110teamproject.Puzzle.mark_solved p2;
  Cs3110teamproject.Room.try_unlock room ~solved_puzzles:[ 1; 2 ];
  assert_equal Cs3110teamproject.Room.Accessible
    (Cs3110teamproject.Room.status room)

let test_try_unlock_inaccessible_deps_unsolved _ =
  let p1 =
    mk_riddle 1
      "I have cities, but no houses. I have mountains, but no trees. I have \
       water, but no fish. What am I?"
      "map"
  in
  let p2 = mk_math 2 "What is 2 + 2?" 4 in
  let room = mk_room ~room_deps:[ 1; 2 ] 2 "Dependent Room" [ p1; p2 ] in
  Cs3110teamproject.Puzzle.try_unlock [] p1;
  Cs3110teamproject.Puzzle.mark_solved p1;
  (* p2 remains unsolved *)
  Cs3110teamproject.Room.try_unlock room ~solved_puzzles:[ 1 ];
  assert_equal Cs3110teamproject.Room.Inaccessible
    (Cs3110teamproject.Room.status room)

(*is_accessible tests--------------------------------------------*)
let test_is_accessible_false _ =
  let room = mk_room 1 "Inaccessible Room" [] in
  assert_equal false (Cs3110teamproject.Room.is_accessible room)

let test_is_accessible_true _ =
  let room = mk_room 1 "Accessible Room" [] in
  room.status <- Cs3110teamproject.Room.Accessible;
  assert_equal true (Cs3110teamproject.Room.is_accessible room)

(*attempt_enter tests--------------------------------------------*)
let test_attempt_enter_inaccessible_deps_unsolved _ =
  let p1 =
    mk_riddle 1
      "I have cities, but no houses. I have mountains, but no trees. I have \
       water, but no fish. What am I?"
      "map"
  in
  let p2 = mk_math 2 "What is 2 + 2?" 4 in
  let room = mk_room ~room_deps:[ 1; 2 ] 2 "Dependent Room" [ p1; p2 ] in
  Cs3110teamproject.Puzzle.try_unlock [] p1;
  Cs3110teamproject.Puzzle.mark_solved p1;
  (* p2 remains unsolved *)
  let can_enter =
    Cs3110teamproject.Room.attempt_enter room
      ~solved_puzzles:[ Cs3110teamproject.Puzzle.puzzle_id p1 ]
  in
  assert_equal false can_enter;
  assert_equal Cs3110teamproject.Room.Inaccessible
    (Cs3110teamproject.Room.status room)

let test_attempt_enter_inaccessible_deps_solved _ =
  let p1 =
    mk_riddle 1
      "I have cities, but no houses. I have mountains, but no trees. I have \
       water, but no fish. What am I?"
      "map"
  in
  let p2 = mk_math 2 "What is 2 + 2?" 4 in
  let room = mk_room ~room_deps:[ 1; 2 ] 2 "Dependent Room" [ p1; p2 ] in
  Cs3110teamproject.Puzzle.try_unlock [] p1;
  Cs3110teamproject.Puzzle.mark_solved p1;
  Cs3110teamproject.Puzzle.try_unlock [] p2;
  Cs3110teamproject.Puzzle.mark_solved p2;
  let can_enter =
    Cs3110teamproject.Room.attempt_enter room
      ~solved_puzzles:
        [
          Cs3110teamproject.Puzzle.puzzle_id p1;
          Cs3110teamproject.Puzzle.puzzle_id p2;
        ]
  in
  assert_equal true can_enter;
  assert_equal Cs3110teamproject.Room.Accessible
    (Cs3110teamproject.Room.status room)

let test_attempt_enter_accessible _ =
  let p1 =
    mk_riddle 1
      "I have cities, but no houses. I have mountains, but no trees. I have \
       water, but no fish. What am I?"
      "map"
  in
  let p2 = mk_math 2 "What is 2 + 2?" 4 in
  let room = mk_room 1 "Already Accessible Room" [ p1; p2 ] in
  Cs3110teamproject.Puzzle.try_unlock [] p1;
  Cs3110teamproject.Puzzle.mark_solved p1;
  room.status <- Cs3110teamproject.Room.Accessible;
  let can_enter =
    Cs3110teamproject.Room.attempt_enter room ~solved_puzzles:[]
  in
  assert_equal true can_enter;
  assert_equal Cs3110teamproject.Room.Accessible
    (Cs3110teamproject.Room.status room)

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
         (*Puzzle Tests*)
         "make riddle basic" >:: test_make_riddle_basic;
         "check answer riddle" >:: test_check_answer_riddle;
         "check answer math" >:: test_check_answer_math;
         "check answer trivia" >:: test_check_answer_trivia;
         "try unlock no deps" >:: test_try_unlock_no_deps;
         "try unlock with unsolved deps" >:: test_try_unlock_with_unsolved_deps;
         "try unlock with solved deps" >:: test_try_unlock_with_solved_deps;
         "try unlock with multiple deps" >:: test_try_unlock_with_multiple_deps;
         "try unlock already unlocked" >:: test_try_unlock_already_unlocked;
         "try unlock already solved" >:: test_try_unlock_already_solved;
         "mark solved" >:: test_mark_solved;
         (*Room Tests*)
         "make room basic" >:: test_make_room_basic;
         "room fulfilled no puzzles" >:: room_fulfilled_no_puzzles;
         "room fulfilled not fulfilled" >:: room_fulfilled_not_fulfilled;
         "room fulfilled is fulfilled" >:: room_fullfilled_is_fulfilled;
         "try unlock room no deps" >:: test_try_unlock_room_no_deps;
         "try unlock accessible room" >:: test_try_unlock_accessible_room;
         "try unlock inaccessible deps solved"
         >:: test_try_unlock_inaccessible_deps_solved;
         "try unlock inaccessible deps unsolved"
         >:: test_try_unlock_inaccessible_deps_unsolved;
         "is accessible false" >:: test_is_accessible_false;
         "is accessible true" >:: test_is_accessible_true;
         "attempt enter inaccessible deps unsolved"
         >:: test_attempt_enter_inaccessible_deps_unsolved;
         "attempt enter inaccessible deps solved"
         >:: test_attempt_enter_inaccessible_deps_solved;
         "attempt enter accessible" >:: test_attempt_enter_accessible;
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
