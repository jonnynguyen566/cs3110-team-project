open OUnit2
open Escape_room.Puzzle

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

(* ------------------------------------------------------------- *)
(* TEST SUITE                                                    *)
(* ------------------------------------------------------------- *)

let tests =
  "test suite"
  >::: [
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
       ]

let _ = run_test_tt_main tests
