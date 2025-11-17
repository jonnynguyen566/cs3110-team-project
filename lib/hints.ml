let hint_table : (int, string) Hashtbl.t = Hashtbl.create 10
let register_hint ~puzzle_id hint = Hashtbl.replace hint_table puzzle_id hint
let get_hint puzzle_id = Hashtbl.find_opt hint_table puzzle_id
