(*Instantiating types for puzzles*)
type puzzle_status =
  | Locked (*Puzzle hasn't been revealed yet/isn't visible*)
  | Unlocked (*Puzzle is unlocked and can be interacted with*)
  | Solved (*Puzzle marked as solved, can use for unlocking dependent puzzles*)

type puzzle_type =
  | Riddle of string * string
  | Math of string * int
  | Trivia of string * string

type puzzle = {
  id : int;
  ptype : puzzle_type;
  deps : int list;
  mutable status : puzzle_status;
}

let puzzle_id p = p.id
let status p = p.status

let make ~id ~puzzle_type ~deps =
  { id; ptype = puzzle_type; deps; status = Locked }

let normalize s = String.lowercase_ascii (String.trim s)

(*Helper function that maps simplle one digit numbers in string/word form to a
  int for math riddles*)
let word_to_int =
  let table =
    [
      ("zero", 0);
      ("one", 1);
      ("two", 2);
      ("three", 3);
      ("four", 4);
      ("five", 5);
      ("six", 6);
      ("seven", 7);
      ("eight", 8);
      ("nine", 9);
      ("ten", 10);
    ]
  in
  fun s ->
    let s = String.lowercase_ascii (String.trim s) in
    List.assoc_opt s table

let check_answer p answer =
  let ans = normalize answer in     (* trim + lowercase ONCE *)
  match p.ptype with
  | Riddle (_, correct) | Trivia (_, correct) ->
      ans = normalize correct

  | Math (_, n) -> (
      match (int_of_string_opt ans, word_to_int ans) with
      | Some x, _ -> x = n
      | None, Some x -> x = n
      | _ -> false
    )

let mark_solved p = p.status <- Solved

(*Checks status of a puzzle if it should be accessible to the user yet*)
let try_unlock solved_ids p =
  match p.status with
  | Locked ->
      if List.for_all (fun dep -> List.mem dep solved_ids) p.deps then
        p.status <- Unlocked
  | _ -> ()
