open Bogue
module Game_logic = Cs3110teamproject.Game_logic
module Game_state = Cs3110teamproject.Game_state
module Puzzle = Cs3110teamproject.Puzzle
module Room = Cs3110teamproject.Room
module W = Widget
module L = Layout

(** type for state for objects that can be toggled *)
type toggle_state =
  | Closed
  | Open

(** type for beginning screen states in UI flow *)
type screen_state =
  | Intro1
  | Intro2
  | StartingRoom

(*actual pop up for when the user toggles an image that has a hint*)
let puzzle_popup (game_state : Game_state.t) (puzzle : Puzzle.puzzle) on_correct
    parent_layout () =
  let question_text =
    match Puzzle.puzzle_type puzzle with
    | Puzzle.Riddle (q, _) | Puzzle.Math (q, _) | Puzzle.Trivia (q, _) -> q
  in
  let question = W.text_display ~w:250 question_text |> L.resident in
  let input = W.text_input ~max_size:100 ~prompt:"Your answer:" () in
  let input_layout = L.resident input in

  (* text display for feedback messages *)
  let feedback = W.text_display ~w:250 "" in
  let feedback_layout = L.resident feedback in

  let submit_button =
    W.button "Submit" ~action:(fun _ ->
        let answer = W.get_text input in
        if Puzzle.check_answer puzzle answer then begin
          Puzzle.mark_solved puzzle;
          Game_state.solve_puzzle game_state
            ~puzzle_id:(Puzzle.puzzle_id puzzle);
          W.set_text feedback (Puzzle.success_msg puzzle);
          on_correct ()
        end
        else begin
          W.set_text feedback ("Wrong answer: " ^ answer)
        end)
  in
  let button_layout = L.resident submit_button in

  let content =
    L.tower [ question; input_layout; button_layout; feedback_layout ]
  in

  Popup.one_button ~button:"Close" ~dst:parent_layout content

(*general function for when a image triggers a hint/popup (default image and
  then clicked image)*)
let toggle_image ?w ?h ?x ?y ?(noscale = false) ~closed_image ~open_image
    ~game_state ~puzzle room_layout () =
  let img = W.image ?w ?h ~noscale closed_image in
  let state = ref Closed in
  let on_click _ _ _ =
    let puzzle_status = Puzzle.status puzzle in
    match puzzle_status with
    | Puzzle.Locked ->
        let popup_msg = "This puzzle is locked. Solve other puzzles first!" in
        let msg_widget = W.text_display ~w:150 ~h:70 popup_msg |> L.resident in
        Popup.one_button ~button:"OK" ~dst:room_layout msg_widget |> ignore
    | Puzzle.Unlocked -> (
        match !state with
        | Closed ->
            puzzle_popup game_state puzzle
              (fun () ->
                Image.set_file (W.get_image img) open_image;
                state := Open;
                W.update img)
              room_layout ()
            |> ignore
        | Open ->
            Image.set_file (W.get_image img) closed_image;
            state := Closed;
            W.update img)
    | Puzzle.Solved ->
        let popup_msg = "This puzzle has already been solved!" in
        let msg_widget = W.text_display ~w:150 ~h:70 popup_msg |> L.resident in
        Popup.one_button ~button:"OK" ~dst:room_layout msg_widget |> ignore
  in
  W.connect_main img img on_click Trigger.buttons_up |> W.add_connection img;
  (L.resident ?x ?y img, state)

(** another image toggling, except this time it changes the background, not just
    the image itself*)
let toggle_image_with_bg_change ?w ?h ?x ?y ?(noscale = false) ~closed_image
    ~open_image ~bg_widget ~new_bg_image ~game_state ~puzzle room_layout () =
  let img = W.image ?w ?h ~noscale closed_image in
  let state = ref Closed in
  let on_click _ _ _ =
    let puzzle_status = Puzzle.status puzzle in
    match puzzle_status with
    | Puzzle.Locked ->
        let popup_msg = "This puzzle is locked. Solve other puzzles first!" in
        let msg_widget = W.text_display ~w:150 ~h:70 popup_msg |> L.resident in
        Popup.one_button ~button:"OK" ~dst:room_layout msg_widget |> ignore
    | Puzzle.Unlocked -> (
        match !state with
        | Closed ->
            puzzle_popup game_state puzzle
              (fun () ->
                Image.set_file (W.get_image img) open_image;
                Image.set_file (W.get_image bg_widget) new_bg_image;
                state := Open;
                W.update img;
                W.update bg_widget)
              room_layout ()
            |> ignore
        | Open ->
            Image.set_file (W.get_image img) closed_image;
            state := Closed;
            W.update img)
    | Puzzle.Solved ->
        let popup_msg = "This puzzle has already been solved!" in
        let msg_widget = W.text_display ~w:150 ~h:70 popup_msg |> L.resident in
        Popup.one_button ~button:"OK" ~dst:room_layout msg_widget |> ignore
  in
  W.connect_main img img on_click Trigger.buttons_up |> W.add_connection img;
  (L.resident ?x ?y img, state)

(* actual display logic *)
let () =
  let game_state = Game_logic.init_game () in
  let chest_puzzle = Game_logic.chest_puzzle in
  let casket_puzzle = Game_logic.casket_puzzle in
  let h1_puzzle = Game_logic.h1_puzzle in
  let h2_puzzle = Game_logic.h2_puzzle in
  let h3_puzzle = Game_logic.h3_puzzle in
  let h4_puzzle = Game_logic.h4_puzzle in
  let lock_puzzle = Game_logic.lock_puzzle in
  let torch_puzzle = Game_logic.torch_puzzle in
  let spider_puzzle = Game_logic.spider_puzzle in
  let doorknob_puzzle = Game_logic.doorknob_puzzle in
  let scroll_puzzle = Game_logic.scroll_puzzle in
  let pot1_puzzle = Game_logic.pot1_puzzle in
  let pot2_puzzle = Game_logic.pot2_puzzle in
  let pot3_puzzle = Game_logic.pot3_puzzle in
  let lockedpot_puzzle = Game_logic.lockedpot_puzzle in

  let bg_w, bg_h = (1280, 720) in
  let current_screen = ref Intro1 in
  let intro_bg = W.image ~noscale:true "images/starting_room_blurry.png" in
  let intro_bg_layout = L.resident ~w:bg_w ~h:bg_h intro_bg in

  (* scale intro screens to the window so Windows does not crop to the
     top-left *)
  let beginning = W.image ~w:bg_w ~h:bg_h "images/click_to_begin.png" in
  let beginning_layout = L.resident beginning in
  let screen1 =
    L.superpose ~w:bg_w ~h:bg_h [ intro_bg_layout; beginning_layout ]
  in

  let instructions = W.image ~w:bg_w ~h:bg_h "images/instructions.png" in
  let instr_layout = L.resident instructions in
  let screen2 = L.superpose ~w:bg_w ~h:bg_h [ intro_bg_layout; instr_layout ] in

  (* starting room *)
  let main_bg =
    W.image ~w:bg_w ~h:bg_h ~noscale:true "images/starting_room.jpg"
  in
  let main_bg_layout = L.resident ~w:bg_w ~h:bg_h main_bg in
  let screen3 = L.superpose ~w:bg_w ~h:bg_h [ main_bg_layout ] in

  (* corridor room *)
  let corridor_bg =
    W.image ~w:bg_w ~h:bg_h ~noscale:true "images/corridor_cobwebs_door.png"
  in
  let corridor_bg_layout = L.resident ~w:bg_w ~h:bg_h corridor_bg in
  let screen4 = L.superpose ~w:bg_w ~h:bg_h [ corridor_bg_layout ] in

  (* stairway room *)
  let stairway_bg =
    W.image ~w:bg_w ~h:bg_h ~noscale:true "images/cobweb_staircase_closed.png"
  in
  let stairway_bg_layout = L.resident ~w:bg_w ~h:bg_h stairway_bg in
  let screen5 = L.superpose ~w:bg_w ~h:bg_h [ stairway_bg_layout ] in

  (* pottery room *)
  let pottery_bg =
    W.image ~w:bg_w ~h:bg_h ~noscale:true "images/potteryroom.png"
  in
  let potteryroom_bg_layout = L.resident ~w:bg_w ~h:bg_h pottery_bg in
  let screen6 = L.superpose ~w:bg_w ~h:bg_h [ potteryroom_bg_layout ] in

  let treasure_room, treasure_state =
    toggle_image ~x:800 ~y:470 ~w:325 ~h:163
      ~closed_image:"images/chest_closed.png"
      ~open_image:"images/chest_open.png" ~game_state ~puzzle:chest_puzzle
      screen3 ()
  in

  let casket_room, casket_state =
    toggle_image ~x:370 ~y:240 ~w:570 ~h:300
      ~closed_image:"images/casket_closed.png"
      ~open_image:"images/casket_open.png" ~game_state ~puzzle:casket_puzzle
      screen3 ()
  in

  (* Corridor Room *)
  let lock_room, lock_state =
    toggle_image ~x:640 ~y:370 ~w:30 ~h:45
      ~closed_image:"images/lock_closed.png" ~open_image:"images/lock_open.png"
      ~game_state ~puzzle:lock_puzzle screen4 ()
  in
  let h1_room, h1_state =
    toggle_image ~x:1030 ~y:400 ~w:125 ~h:125 ~closed_image:"images/h_1.png"
      ~open_image:"images/h1_dark.png" ~game_state ~puzzle:h1_puzzle screen4 ()
  in

  let h2_room, h2_state =
    toggle_image ~x:240 ~y:180 ~w:150 ~h:150 ~closed_image:"images/h_2.png"
      ~open_image:"images/h2_dark.png" ~game_state ~puzzle:h2_puzzle screen4 ()
  in

  let h3_room, h3_state =
    toggle_image ~x:160 ~y:385 ~w:140 ~h:167 ~closed_image:"images/h_3.png"
      ~open_image:"images/h3_dark.png" ~game_state ~puzzle:h3_puzzle screen4 ()
  in

  let h4_room, h4_state =
    toggle_image ~x:900 ~y:120 ~w:140 ~h:167 ~closed_image:"images/h_4.png"
      ~open_image:"images/h4_dark.png" ~game_state ~puzzle:h4_puzzle screen4 ()
  in

  (* stairway room *)
  let doorknob_room, doorknob_state =
    toggle_image_with_bg_change ~x:910 ~y:280 ~w:40 ~h:80
      ~closed_image:"images/doorknob.png" ~open_image:"images/transparent.png"
      ~bg_widget:stairway_bg ~new_bg_image:"images/stairway.jpg" ~game_state
      ~puzzle:doorknob_puzzle screen5 ()
  in

  let torch_room, torch_state =
    toggle_image ~x:100 ~y:200 ~w:140 ~h:167
      ~closed_image:"images/unlit_torch.png" ~open_image:"images/lit_torch.png"
      ~game_state ~puzzle:torch_puzzle screen5 ()
  in

  let spider_room, spider_state =
    toggle_image ~x:1000 ~y:100 ~w:167 ~h:167
      ~closed_image:"images/hanging_spider.png"
      ~open_image:"images/transparent.png" ~game_state ~puzzle:spider_puzzle
      screen5 ()
  in

  (* Pottery room *)
  let scroll_room, scroll_state =
    toggle_image ~x:610 ~y:380 ~w:167 ~h:167
      ~closed_image:"images/scroll_closed.png"
      ~open_image:"images/scroll_open.png" ~game_state ~puzzle:scroll_puzzle
      screen6 ()
  in

  let pot1_room, pot1_state =
    toggle_image ~x:500 ~y:160 ~w:100 ~h:130 ~closed_image:"images/pot1.png"
      ~open_image:"images/pot1.png" ~game_state ~puzzle:pot1_puzzle screen6 ()
  in

  let pot2_room, pot2_state =
    toggle_image ~x:600 ~y:160 ~w:100 ~h:120 ~closed_image:"images/pot2.png"
      ~open_image:"images/pot2.png" ~game_state ~puzzle:pot2_puzzle screen6 ()
  in

  let pot3_room, pot3_state =
    toggle_image ~x:700 ~y:160 ~w:100 ~h:120 ~closed_image:"images/pot3.png"
      ~open_image:"images/pot3.png" ~game_state ~puzzle:pot3_puzzle screen6 ()
  in

  let lockedpot_room, lockedpot_state =
    toggle_image ~x:1000 ~y:340 ~w:140 ~h:120
      ~closed_image:"images/lockedpot.png" ~open_image:"images/unlockedpot.png"
      ~game_state ~puzzle:lockedpot_puzzle screen6 ()
  in

  let main_layout = L.superpose ~w:bg_w ~h:bg_h [ screen1 ] in
  L.auto_scale main_layout;
  L.disable_resize main_layout;

  let navigation_arrow ~x ~y ~image ~target_screen ~current_room ~target_room ~main_layout () =
    let arrow = W.image ~noscale:true image in
    let arrow_layout = L.resident ~x ~y arrow in

    let on_click _ _ _ = 
      if Room.room_fulfilled current_room then
        (
          L.set_rooms main_layout [ target_screen ];
          let msg_widget = W.text_display ~w:300 ~h:85 (Room.intro_message target_room) |> L.resident in
          Popup.one_button ~button:"OK" ~dst:target_screen msg_widget |> ignore
        )
      else 
        let popup_msg = "You must solve all the puzzles in this room first to continue!" in
        let msg_widget = W.text_display ~w:150 ~h:70 popup_msg |> L.resident in
        Popup.one_button ~button:"OK" ~dst:main_layout msg_widget |> ignore
    in

    W.connect_main arrow arrow on_click Trigger.buttons_up
    |> W.add_connection arrow;
    arrow_layout
  in

  let arrow_to_corridor =
    navigation_arrow ~x:1100 ~y:350 ~image:"images/Arrow.png"
      ~target_screen:screen4 
      ~current_room:Game_logic.starting_room
      ~target_room:Game_logic.corridor_room
      ~main_layout ()
  in

  let arrow_to_stairway =
    navigation_arrow ~x:1100 ~y:350 ~image:"images/Arrow.png"
      ~target_screen:screen5 
      ~current_room:Game_logic.corridor_room
      ~target_room:Game_logic.stairway_room
      ~main_layout ()
  in

  let arrow_to_pottery =
    navigation_arrow ~x:1100 ~y:350 ~image:"images/Arrow.png"
      ~target_screen:screen6 
      ~current_room:Game_logic.stairway_room
      ~target_room:Game_logic.pottery_room
      ~main_layout ()
  in

  L.set_rooms screen3
    [ main_bg_layout; treasure_room; casket_room; arrow_to_corridor ];
  L.set_rooms screen4
    [
      corridor_bg_layout;
      lock_room;
      h1_room;
      h2_room;
      h3_room;
      h4_room;
      arrow_to_stairway;
    ];
  L.set_rooms screen5
    [
      stairway_bg_layout;
      doorknob_room;
      spider_room;
      torch_room;
      arrow_to_pottery;
    ];
  L.set_rooms screen6
    [
      potteryroom_bg_layout;
      scroll_room;
      pot1_room;
      pot2_room;
      pot3_room;
      lockedpot_room;
    ];

  let transition_to_intro2 _ _ _ =
    current_screen := Intro2;
    L.set_rooms main_layout [ screen2 ]
  in
  let transition_to_starting_room _ _ _ =
    current_screen := StartingRoom;
    L.set_rooms main_layout [ screen3 ];
    let msg_widget = W.text_display ~w:300 ~h:85 (Room.intro_message Game_logic.starting_room) |> L.resident in
    Popup.one_button ~button:"OK" ~dst:screen3 msg_widget |> ignore
  in

  (* Connect click handlers to screens *)
  W.connect_main beginning beginning
    (fun ev x y -> if !current_screen = Intro1 then transition_to_intro2 ev x y)
    Trigger.buttons_up
  |> W.add_connection beginning;

  W.connect_main instructions instructions
    (fun ev x y ->
      if !current_screen = Intro2 then transition_to_starting_room ev x y)
    Trigger.buttons_up
  |> W.add_connection instructions;

  main_layout |> Bogue.of_layout |> Bogue.run
