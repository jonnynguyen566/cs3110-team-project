open OUnit2
open Escape_room.Puzzle
open Escape_room.Room

(*Helper Functions for creating puzzles to test with*)
let mk_riddle ?(deps = []) id q a =
  Escape_room.Puzzle.make ~id ~puzzle_type:(Riddle (q, a)) ~deps

let mk_trivia ?(deps = []) id q a =
  Escape_room.Puzzle.make ~id ~puzzle_type:(Trivia (q, a)) ~deps

let mk_math ?(deps = []) id q n =
  Escape_room.Puzzle.make ~id ~puzzle_type:(Math (q, n)) ~deps

(*Helper Functions for creating rooms to test with*)
let mk_room ?(room_deps = []) id desc puzzles =
  Escape_room.Room.make ~id ~description:desc ~puzzles ~room_deps

(* ------------------------------------------------------------- *)
(* PUZZLE MODULE TESTS                                           *)
(* ------------------------------------------------------------- *)

(*puzzle creation tests------------------------------------------*)
let test_make_riddle_basic _ =
  let p = mk_riddle 42 "What has keys?" "piano" in
  (* Check ID *)
  assert_equal 42 (Escape_room.Puzzle.puzzle_id p);
  (* New puzzles should be Locked *)
  assert_equal Escape_room.Puzzle.Locked (Escape_room.Puzzle.status p)

(*check answer tests---------------------------------------------*)
let test_check_answer_riddle _ =
  let p = mk_riddle 1 "What has keys but can't open locks?" "piano" in
  assert_equal true (check_answer p "piano");
  assert_equal true (check_answer p "PIANO");
  (*Testing caps*)
  assert_equal true (check_answer p "  piano  ");
  (*Testing whitespace*)
  assert_equal false (check_answer p "keyboard")

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
  let p = mk_riddle 1 "What has keys but can't open locks?" "piano" in
  Escape_room.Puzzle.try_unlock [] p;
  assert_equal Escape_room.Puzzle.Unlocked (Escape_room.Puzzle.status p)

let test_try_unlock_with_unsolved_deps _ =
  let _p_dep = mk_riddle 1 "What has keys but can't open locks?" "piano" in
  let p = mk_riddle ~deps:[ 1 ] 2 "What has hands but can't clap?" "clock" in
  (*The bracket is the passed in list of "global solved puzzles"*)
  Escape_room.Puzzle.try_unlock [] p;
  assert_equal Escape_room.Puzzle.Locked (Escape_room.Puzzle.status p)

let test_try_unlock_with_solved_deps _ =
  let p_dep = mk_riddle 1 "What has keys but can't open locks?" "piano" in
  let p = mk_riddle ~deps:[ 1 ] 2 "What has hands but can't clap?" "clock" in
  (*The bracket is the passed in list of "global solved puzzles"*)
  Escape_room.Puzzle.try_unlock [ Escape_room.Puzzle.puzzle_id p_dep ] p;
  assert_equal Escape_room.Puzzle.Unlocked (Escape_room.Puzzle.status p)

let test_try_unlock_with_multiple_deps _ =
  let p_dep1 = mk_riddle 1 "What has keys but can't open locks?" "piano" in
  let p_dep2 = mk_math 2 "What is 2 + 2?" 4 in
  let p = mk_riddle ~deps:[ 1; 2 ] 3 "What has hands but can't clap?" "clock" in
  (*The bracket is the passed in list of "global solved puzzles"*)
  Escape_room.Puzzle.try_unlock
    [ Escape_room.Puzzle.puzzle_id p_dep1; Escape_room.Puzzle.puzzle_id p_dep2 ]
    p;
  assert_equal Escape_room.Puzzle.Unlocked (Escape_room.Puzzle.status p)

let test_try_unlock_already_unlocked _ =
  (* deps = [] means puzzle unlocks immediately *)
  let p = mk_riddle 1 "What has keys but can't open locks?" "piano" in

  (* Unlock it through the real API *)
  Escape_room.Puzzle.try_unlock [] p;
  assert_equal Escape_room.Puzzle.Unlocked (Escape_room.Puzzle.status p);

  (* Calling try_unlock again should leave it Unlocked *)
  Escape_room.Puzzle.try_unlock [] p;
  assert_equal Escape_room.Puzzle.Unlocked (Escape_room.Puzzle.status p)

let test_try_unlock_already_solved _ =
  let p = mk_riddle 1 "What has keys but can't open locks?" "piano" in

  (* First, unlock it properly *)
  Escape_room.Puzzle.try_unlock [] p;
  assert_equal Escape_room.Puzzle.Unlocked (Escape_room.Puzzle.status p);

  (* Now mark it solved *)
  Escape_room.Puzzle.mark_solved p;
  assert_equal Escape_room.Puzzle.Solved (Escape_room.Puzzle.status p);

  (* Calling try_unlock again should NOT revert or change status *)
  Escape_room.Puzzle.try_unlock [] p;
  assert_equal Escape_room.Puzzle.Solved (Escape_room.Puzzle.status p)

(*try unlock tests----------------------------------------------*)
let test_mark_solved _ =
  (* deps=[] so this unlocks immediately *)
  let p = mk_riddle 1 "What has keys but can't open locks?" "piano" in

  Escape_room.Puzzle.try_unlock [] p;
  assert_equal Escape_room.Puzzle.Unlocked (Escape_room.Puzzle.status p);

  (* Now mark solved *)
  Escape_room.Puzzle.mark_solved p;
  assert_equal Escape_room.Puzzle.Solved (Escape_room.Puzzle.status p)

(* ------------------------------------------------------------- *)
(* ROOM MODULE TEST                                              *)
(* ------------------------------------------------------------- *)
(*Room Creation Tests--------------------------------------------*)

(*room_fulfilled tests-------------------------------------------*)
let room_fulfilled_no_puzzles _ =
  let room = mk_room 1 "Empty Room" [] in
  assert_equal true (Escape_room.Room.room_fulfilled room)

let room_fulfilled_not_fulfilled _ =
  let p1 = mk_riddle 1 "What has keys but can't open locks?" "piano" in
  let p2 = mk_math 2 "What is 2 + 2?" 4 in
  let room = mk_room 2 "Puzzle Room" [ p1; p2 ] in
  (* Neither puzzle is solved yet *)
  assert_equal false (Escape_room.Room.room_fulfilled room)

let room_fullfilled_is_fulfilled _ =
  let p1 = mk_riddle 1 "What has keys but can't open locks?" "piano" in
  let p2 = mk_math 2 "What is 2 + 2?" 4 in
  let room = mk_room 3 "Puzzle Room" [ p1; p2 ] in
  (* Unlock and solve both puzzles *)
  Escape_room.Puzzle.try_unlock [] p1;
  Escape_room.Puzzle.mark_solved p1;
  Escape_room.Puzzle.try_unlock [] p2;
  Escape_room.Puzzle.mark_solved p2;
  assert_equal true (Escape_room.Room.room_fulfilled room)

(*try_unlock tests-----------------------------------------------*)
let test_try_unlock_room_no_deps _ =
  let room = mk_room 1 "No Deps Room" [] in
  Escape_room.Room.try_unlock room ~solved_puzzles:[];
  assert_equal Escape_room.Room.Accessible (Escape_room.Room.status room)

let test_try_unlock_accessible_room _ =
  let p1 = mk_riddle 1 "What has keys but can't open locks?" "piano" in
  let p2 = mk_math 2 "What is 2 + 2?" 4 in
  let room = mk_room 1 "Already Accessible Room" [ p1; p2 ] in
  Escape_room.Puzzle.try_unlock [] p1;
  Escape_room.Puzzle.mark_solved p1;
  room.status <- Escape_room.Room.Accessible;
  Escape_room.Room.try_unlock room ~solved_puzzles:[];
  assert_equal Escape_room.Room.Accessible (Escape_room.Room.status room)

let test_try_unlock_inaccessible_deps_solved _ =
  let p1 = mk_riddle 1 "What has keys but can't open locks?" "piano" in
  let p2 = mk_math 2 "What is 2 + 2?" 4 in
  let room = mk_room ~room_deps:[ 1; 2 ] 2 "Dependent Room" [ p1; p2 ] in
  Escape_room.Puzzle.try_unlock [] p1;
  Escape_room.Puzzle.mark_solved p1;
  Escape_room.Puzzle.try_unlock [] p2;
  Escape_room.Puzzle.mark_solved p2;
  Escape_room.Room.try_unlock room ~solved_puzzles:[ 1; 2 ];
  assert_equal Escape_room.Room.Accessible (Escape_room.Room.status room)

let test_try_unlock_inaccessible_deps_unsolved _ =
  let p1 = mk_riddle 1 "What has keys but can't open locks?" "piano" in
  let p2 = mk_math 2 "What is 2 + 2?" 4 in
  let room = mk_room ~room_deps:[ 1; 2 ] 2 "Dependent Room" [ p1; p2 ] in
  Escape_room.Puzzle.try_unlock [] p1;
  Escape_room.Puzzle.mark_solved p1;
  (* p2 remains unsolved *)
  Escape_room.Room.try_unlock room ~solved_puzzles:[ 1 ];
  assert_equal Escape_room.Room.Inaccessible (Escape_room.Room.status room)

(*is_accessible tests--------------------------------------------*)
let test_is_accessible_false _ =
  let room = mk_room 1 "Inaccessible Room" [] in
  assert_equal false (Escape_room.Room.is_accessible room)

let test_is_accessible_true _ =
  let room = mk_room 1 "Accessible Room" [] in
  room.status <- Escape_room.Room.Accessible;
  assert_equal true (Escape_room.Room.is_accessible room)

(*attempt_enter tests--------------------------------------------*)
let test_attempt_enter_inaccessible_deps_unsolved _ =
  let p1 = mk_riddle 1 "What has keys but can't open locks?" "piano" in
  let p2 = mk_math 2 "What is 2 + 2?" 4 in
  let room = mk_room ~room_deps:[ 1; 2 ] 2 "Dependent Room" [ p1; p2 ] in
  Escape_room.Puzzle.try_unlock [] p1;
  Escape_room.Puzzle.mark_solved p1;
  (* p2 remains unsolved *)
  let can_enter =
    Escape_room.Room.attempt_enter room ~solved_puzzles:[ Escape_room.Puzzle.puzzle_id p1 ]
  in
  assert_equal false can_enter;
  assert_equal Escape_room.Room.Inaccessible (Escape_room.Room.status room)

let test_attempt_enter_inaccessible_deps_solved _ =
  let p1 = mk_riddle 1 "What has keys but can't open locks?" "piano" in
  let p2 = mk_math 2 "What is 2 + 2?" 4 in
  let room = mk_room ~room_deps:[ 1; 2 ] 2 "Dependent Room" [ p1; p2 ] in
  Escape_room.Puzzle.try_unlock [] p1;
  Escape_room.Puzzle.mark_solved p1;
  Escape_room.Puzzle.try_unlock [] p2; 
  Escape_room.Puzzle.mark_solved p2;
  let can_enter =
    Escape_room.Room.attempt_enter room ~solved_puzzles:[ Escape_room.Puzzle.puzzle_id p1; Escape_room.Puzzle.puzzle_id p2 ]
  in
  assert_equal true can_enter;
  assert_equal Escape_room.Room.Accessible (Escape_room.Room.status room)

(* ------------------------------------------------------------- *)
(* TEST SUITE                                                    *)
(* ------------------------------------------------------------- *)

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
         "room fulfilled no puzzles" >:: room_fulfilled_no_puzzles;
         "room fulfilled not fulfilled" >:: room_fulfilled_not_fulfilled;
         "room fulfilled is fulfilled" >:: room_fullfilled_is_fulfilled;
         "try unlock room no deps" >:: test_try_unlock_room_no_deps;
         "try unlock accessible room" >:: test_try_unlock_accessible_room;
          "try unlock inaccessible deps solved" >:: test_try_unlock_inaccessible_deps_solved;
          "try unlock inaccessible deps unsolved" >:: test_try_unlock_inaccessible_deps_unsolved;
          "is accessible false" >:: test_is_accessible_false;
          "is accessible true" >:: test_is_accessible_true;
          "attempt enter inaccessible deps unsolved" >:: test_attempt_enter_inaccessible_deps_unsolved;
          "attempt enter inaccessible deps solved" >:: test_attempt_enter_inaccessible_deps_solved;

       ]

let _ = run_test_tt_main tests
