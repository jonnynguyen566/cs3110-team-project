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
  let map_puzzle = Game_logic.map_puzzle in
  let oillamp_puzzle = Game_logic.oillamp_puzzle in
  let lockedchest_puzzle = Game_logic.lockedchest_puzzle in
  let plant_puzzle = Game_logic.plant_puzzle in
  let statue_puzzle = Game_logic.statue_puzzle in
  let throne_puzzle = Game_logic.throne_puzzle in
  let hourglass_puzzle = Game_logic.hourglass_puzzle in
  let horus_puzzle = Game_logic.horus_puzzle in
  let sphinx_puzzle = Game_logic.sphinx_puzzle in

  let bg_w, bg_h = (1280, 720) in
  (* Arrow positions relative to window dimensions *)
  let back_arrow_x = 20 in
  let forward_arrow_x = bg_w - 142 in  (* 142 = arrow width + 20px margin *)
  let arrow_y = 382 in
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

  (* treasure room *)
  let treasure_bg =
    W.image ~w:bg_w ~h:bg_h ~noscale:true "images/treasureroom.jpg"
  in
  let treasure_bg_layout = L.resident ~w:bg_w ~h:bg_h treasure_bg in
  let screen7 = L.superpose ~w:bg_w ~h:bg_h [ treasure_bg_layout ] in

  (* throne room *)
  let throne_bg =
    W.image ~w:bg_w ~h:bg_h ~noscale:true "images/throne_room.jpg"
  in
  let throne_bg_layout = L.resident ~w:bg_w ~h:bg_h throne_bg in
  let screen8 = L.superpose ~w:bg_w ~h:bg_h [ throne_bg_layout ] in

  (* ending room *)
  let ending_bg =
    W.image ~w:bg_w ~h:bg_h ~noscale:true "images/ending_room_closed.png"
  in
  let ending_bg_layout = L.resident ~w:bg_w ~h:bg_h ending_bg in
  let screen9 = L.superpose ~w:bg_w ~h:bg_h [ ending_bg_layout ] in

  (* closing screen *)
  let final_bg =
    W.image ~w:bg_w ~h:bg_h ~noscale:true "images/finishedscene.jpg"
  in
  let final_bg_layout = L.resident ~w:bg_w ~h:bg_h final_bg in
  let screen10 = L.superpose ~w:bg_w ~h:bg_h [ final_bg_layout ] in

  (* Starting room items*)
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

  (* Corridor Room items *)
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

  (* Stairway room items *)
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

  (* Pottery room items *)
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

  (* Treasure room items *)
  let map_room, map_state =
    toggle_image ~x:420 ~y:425 ~w:300 ~h:120 ~closed_image:"images/map.png"
      ~open_image:"images/map.png" ~game_state ~puzzle:map_puzzle screen7 ()
  in

  let oillamp_room, oillamp_state =
    toggle_image ~x:500 ~y:600 ~w:140 ~h:120 ~closed_image:"images/oillamp.png"
      ~open_image:"images/oillamp.png" ~game_state ~puzzle:oillamp_puzzle
      screen7 ()
  in

  let lockedchest_room, lockedchest_state =
    toggle_image ~x:820 ~y:370 ~w:280 ~h:280
      ~closed_image:"images/lockedchest.png"
      ~open_image:"images/lockedchestopen.png" ~game_state
      ~puzzle:lockedchest_puzzle screen7 ()
  in

  (* Throne room items *)
  let plant_room, plant_state =
    toggle_image ~x:1060 ~y:300 ~w:236 ~h:450 ~closed_image:"images/plant.png"
      ~open_image:"images/plant.png" ~game_state ~puzzle:plant_puzzle screen8 ()
  in

  let statue_room, statue_state =
    toggle_image ~x:830 ~y:240 ~w:300 ~h:500
      ~closed_image:"images/statueclosed.png"
      ~open_image:"images/statueopen.png" ~game_state ~puzzle:statue_puzzle
      screen8 ()
  in
  let throne_room, throne_state =
    toggle_image_with_bg_change ~x:370 ~y:90 ~w:500 ~h:510
      ~closed_image:"images/throne.png" ~open_image:"images/transparent.png"
      ~bg_widget:throne_bg ~new_bg_image:"images/throne_room_exit.jpg"
      ~game_state ~puzzle:throne_puzzle screen8 ()
  in

  (* Ending room items*)
  let hourglass_room, hourglass_state =
    toggle_image ~x:970 ~y:500 ~w:333 ~h:200
      ~closed_image:"images/hourglass.png"
      ~open_image:"images/hourglass_broken.png" ~game_state
      ~puzzle:hourglass_puzzle screen9 ()
  in
  let horus_room, horus_state =
    toggle_image ~x:390 ~y:210 ~w:150 ~h:75 ~closed_image:"images/horus.png"
      ~open_image:"images/horus_dark.png" ~game_state ~puzzle:horus_puzzle
      screen9 ()
  in
  let sphinx_room, sphinx_state =
    toggle_image_with_bg_change ~x:500 ~y:250 ~w:500 ~h:250
      ~closed_image:"images/sphinx.png" ~open_image:"images/transparent.png"
      ~bg_widget:ending_bg ~new_bg_image:"images/ending_room.png" ~game_state
      ~puzzle:sphinx_puzzle screen9 ()
  in

  (* Timer display *)
  let timer_display =
    W.label ~fg:(Draw.opaque Draw.black) ~size:22 "Time: 00:00"
  in
  let timer_bg = W.image ~w:220 ~h:70 "images/scroll_open.png" in
  let timer_layout =
    L.superpose
      [
        L.resident ~x:(bg_w - 240) ~y:15 timer_bg;
        L.resident ~x:(bg_w - 190) ~y:35 timer_display;
      ]
  in

  (* Timer update function - refreshes every second *)
  let rec update_timer () =
    let elapsed = Game_state.elapsed_time game_state in
    let formatted = Game_state.format_time elapsed in
    Label.set (W.get_label timer_display) ("Time: " ^ formatted);
    W.update timer_display;
    (* Force widget redraw *)
    (* Schedule next update in 1 second (1000ms) *)
    ignore (Timeout.add 1000 update_timer)
  in

  let main_layout = L.superpose ~w:bg_w ~h:bg_h [ screen1; timer_layout ] in
  L.auto_scale main_layout;
  L.disable_resize main_layout;

  let show_screen room = L.set_rooms main_layout [ room; timer_layout ] in

  (* Navigation arrow logic. *)
  let navigation_arrow ~x ~y ~image ~target_screen ~current_room ~target_room
      ~main_layout ?(optional = false) () =
    let arrow = W.image ~noscale:true image in
    let arrow_layout = L.resident ~x ~y arrow in

    let on_click _ _ _ =
      if optional then show_screen target_screen
      else if Room.room_fulfilled current_room then (
        show_screen target_screen;
        let msg_widget =
          W.text_display ~w:300 ~h:85 (Room.intro_message target_room)
          |> L.resident
        in
        Popup.one_button ~button:"OK" ~dst:target_screen msg_widget |> ignore)
      else
        let popup_msg =
          "You must solve all the puzzles in this room first to continue!"
        in
        let msg_widget = W.text_display ~w:150 ~h:70 popup_msg |> L.resident in
        Popup.one_button ~button:"OK" ~dst:main_layout msg_widget |> ignore
    in

    W.connect_main arrow arrow on_click Trigger.buttons_up
    |> W.add_connection arrow;
    arrow_layout
  in

  (* Arrows between rooms. *)
  let arrow_to_corridor =
    navigation_arrow ~x:forward_arrow_x ~y:arrow_y ~image:"images/Arrowcopy.png"
      ~target_screen:screen4 ~current_room:Game_logic.starting_room
      ~target_room:Game_logic.corridor_room ~main_layout ()
  in

  let arrow_to_stairway =
    navigation_arrow ~x:forward_arrow_x ~y:arrow_y ~image:"images/Arrowcopy.png"
      ~target_screen:screen5 ~current_room:Game_logic.corridor_room
      ~target_room:Game_logic.stairway_room ~main_layout ()
  in

  let arrow_to_pottery =
    navigation_arrow ~x:forward_arrow_x ~y:arrow_y ~image:"images/Arrowcopy.png"
      ~target_screen:screen6 ~current_room:Game_logic.stairway_room
      ~target_room:Game_logic.pottery_room ~main_layout ()
  in

  let arrow_to_treasure =
    navigation_arrow ~x:forward_arrow_x ~y:arrow_y ~image:"images/Arrowcopy.png"
      ~target_screen:screen7 ~current_room:Game_logic.pottery_room
      ~target_room:Game_logic.treasure_room ~main_layout ()
  in

  let arrow_to_throneroom =
    navigation_arrow ~x:forward_arrow_x ~y:arrow_y ~image:"images/Arrowcopy.png"
      ~target_screen:screen8 ~current_room:Game_logic.treasure_room
      ~target_room:Game_logic.throne_room ~main_layout ()
  in

  let arrow_to_ending =
    navigation_arrow ~x:forward_arrow_x ~y:arrow_y ~image:"images/Arrowcopy.png"
      ~target_screen:screen9 ~current_room:Game_logic.throne_room
      ~target_room:Game_logic.ending_room ~main_layout ()
  in

  (* Back arrows. Optional is true. *)
  let arrow_corridor_to_start =
    navigation_arrow ~x:back_arrow_x ~y:arrow_y ~image:"images/backArrow.png"
      ~target_screen:screen3 ~current_room:Game_logic.corridor_room
      ~target_room:Game_logic.starting_room ~main_layout ~optional:true ()
  in

  let arrow_stairway_to_corridor =
    navigation_arrow ~x:back_arrow_x ~y:arrow_y ~image:"images/backArrow.png"
      ~target_screen:screen4 ~current_room:Game_logic.stairway_room
      ~target_room:Game_logic.corridor_room ~main_layout ~optional:true ()
  in

  let arrow_pottery_to_stairway =
    navigation_arrow ~x:back_arrow_x ~y:arrow_y ~image:"images/backArrow.png"
      ~target_screen:screen5 ~current_room:Game_logic.pottery_room
      ~target_room:Game_logic.stairway_room ~main_layout ~optional:true ()
  in

  let arrow_treasure_to_pottery =
    navigation_arrow ~x:back_arrow_x ~y:arrow_y ~image:"images/backArrow.png"
      ~target_screen:screen6 ~current_room:Game_logic.treasure_room
      ~target_room:Game_logic.pottery_room ~main_layout ~optional:true ()
  in

  let arrow_throne_to_treasure =
    navigation_arrow ~x:back_arrow_x ~y:arrow_y ~image:"images/backArrow.png"
      ~target_screen:screen7 ~current_room:Game_logic.throne_room
      ~target_room:Game_logic.treasure_room ~main_layout ~optional:true ()
  in

  let arrow_ending_to_throne =
    navigation_arrow ~x:back_arrow_x ~y:arrow_y ~image:"images/backArrow.png"
      ~target_screen:screen8 ~current_room:Game_logic.ending_room
      ~target_room:Game_logic.throne_room ~main_layout ~optional:true ()
  in

  let show_ending_popup game_state =
    let final_time = Game_state.elapsed_time game_state in
    let time_str = Game_state.format_time final_time in

    (* Create scroll background *)
    let scroll_bg = W.image ~w:450 ~h:350 "images/scrollvert.png" in
    let scroll_layout = L.resident ~x:420 ~y:150 scroll_bg in

    (* Create ending message text *)
    let message_text = W.text_display ~w:400 ~h:150 Game_state.ending_message in
    let message_layout = L.resident ~x:530 ~y:260 message_text in

    (* Create time display *)
    let time_text = W.label ~size:20 ("Final Time: " ^ time_str) in
    let time_layout = L.resident ~x:540 ~y:380 time_text in

    (* Update screen10 to include the overlay *)
    L.set_rooms screen10
      [ final_bg_layout; scroll_layout; message_layout; time_layout ]
  in

  let arrow_ending_to_final =
    let arrow = W.image ~noscale:true "images/Arrowcopy.png" in
    let arrow_layout = L.resident ~x:forward_arrow_x ~y:arrow_y arrow in

    let on_click _ _ _ =
      if Game_state.is_finished game_state then begin
        show_screen screen10;
        (* Show popup after brief delay *)
        ignore (Timeout.add 500 (fun () -> show_ending_popup game_state))
      end
      else
        let popup_msg =
          "You must solve all the puzzles in this room first to continue!"
        in
        let msg_widget = W.text_display ~w:150 ~h:70 popup_msg |> L.resident in
        Popup.one_button ~button:"OK" ~dst:main_layout msg_widget |> ignore
    in

    W.connect_main arrow arrow on_click Trigger.buttons_up
    |> W.add_connection arrow;
    arrow_layout
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
      arrow_corridor_to_start;
    ];
  L.set_rooms screen5
    [
      stairway_bg_layout;
      doorknob_room;
      spider_room;
      torch_room;
      arrow_to_pottery;
      arrow_stairway_to_corridor;
    ];
  L.set_rooms screen6
    [
      potteryroom_bg_layout;
      scroll_room;
      pot1_room;
      pot2_room;
      pot3_room;
      lockedpot_room;
      arrow_to_treasure;
      arrow_pottery_to_stairway;
    ];
  L.set_rooms screen7
    [
      treasure_bg_layout;
      oillamp_room;
      map_room;
      lockedchest_room;
      arrow_treasure_to_pottery;
      arrow_to_throneroom;
    ];

  L.set_rooms screen8
    [
      throne_bg_layout;
      plant_room;
      statue_room;
      throne_room;
      arrow_throne_to_treasure;
      arrow_to_ending;
    ];

  L.set_rooms screen9
    [
      ending_bg_layout;
      hourglass_room;
      horus_room;
      sphinx_room;
      arrow_ending_to_throne;
      arrow_ending_to_final;
    ];

  let transition_to_intro2 _ _ _ =
    current_screen := Intro2;
    show_screen screen2
  in
  let transition_to_starting_room _ _ _ =
    current_screen := StartingRoom;
    show_screen screen3;
    (* Start the timer when player enters the starting room *)
    Game_state.start_timer game_state;
    let msg_widget =
      W.text_display ~w:300 ~h:85 (Room.intro_message Game_logic.starting_room)
      |> L.resident
    in
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

  (* Start the timer update loop *)
  update_timer ();

  let board = main_layout |> Bogue.of_layout in
  Bogue.run board
