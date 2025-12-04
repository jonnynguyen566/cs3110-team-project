type puzzle_status =
  | Locked
  | Unlocked
  | Solved

type puzzle_type =
  | Riddle of string * string
  | Math of string * int
  | Trivia of string * string

type puzzle = {
  id : int;
  ptype : puzzle_type;
  deps : int list;
  mutable status : puzzle_status;
  success_msg : string;
}

let puzzle_id p = p.id
let status p = p.status
let success_msg p = p.success_msg
let puzzle_type p = p.ptype

let make ~id ~puzzle_type ~deps ~success_msg =
  { id; ptype = puzzle_type; deps; status = Locked; success_msg }

let normalize s = String.lowercase_ascii (String.trim s)

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
  let ans = normalize answer in
  (* trim + lowercase ONCE *)
  match p.ptype with
  | Riddle (_, correct) | Trivia (_, correct) -> ans = normalize correct
  | Math (_, n) -> (
      match (int_of_string_opt ans, word_to_int ans) with
      | Some x, _ -> x = n
      | None, Some x -> x = n
      | _ -> false)

let mark_solved p = p.status <- Solved
let set_status p new_status = p.status <- new_status

let try_unlock solved_ids p =
  match p.status with
  | Locked ->
      if List.for_all (fun dep -> List.mem dep solved_ids) p.deps then
        p.status <- Unlocked
  | _ -> ()
