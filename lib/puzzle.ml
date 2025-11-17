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
}

let puzzle_id p = p.id
let status p = p.status

let make ~id ~puzzle_type ~deps =
  { id; ptype = puzzle_type; deps; status = Locked }

let normalize s = String.lowercase_ascii (String.trim s)

let check_answer p answer =
  match p.ptype with
  | Riddle (_, ans) | Trivia (_, ans) -> normalize answer = normalize ans
  | Math (_, n) -> (
      match int_of_string_opt answer with
      | Some x -> x = n
      | None -> false)

let mark_solved p = p.status <- Solved

let try_unlock solved_ids p =
  match p.status with
  | Locked ->
      if List.for_all (fun dep -> List.mem dep solved_ids) p.deps then
        p.status <- Unlocked
  | _ -> ()
