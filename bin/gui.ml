open Bogue
module W = Widget
module L = Layout

(** type for state for objects that can be toggled *)
type toggle_state =
  | Closed
  | Open

type answer_result = {
  is_correct : bool;
  message : string;
}
(** type for whether user inputted right or wrong answer *)

(** type for room state*)
type game_state =
type game_state =
  | Intro1
  | Intro2
  | StartingRoom
  | CorridorRoom
  | StairwayRoom

(*actual pop up for when the user toggles an image that has a hint*)
let puzzle_popup message on_submit parent_layout () =
  let question = W.text_display ~w:250 message |> L.resident in
  let input = W.text_input ~max_size:100 ~prompt:"Your answer:" () in
  let input_layout = L.resident input in

  (* text display for feedback messages *)
  let feedback = W.text_display ~w:250 "" in
  let feedback_layout = L.resident feedback in

  let submit_button =
    W.button "Submit" ~action:(fun _ ->
        let answer = W.get_text input in
        let result = on_submit answer in
        (* Display the result message in the feedback widget *)
        W.set_text feedback result)
  in
  let submit_button =
    W.button "Submit" ~action:(fun _ ->
        let answer = W.get_text input in
        let result = on_submit answer in
        (* Display the result message in the feedback widget *)
        W.set_text feedback result)
  in
  let button_layout = L.resident submit_button in

  let content =
    L.tower [ question; input_layout; button_layout; feedback_layout ]
  in


  let content =
    L.tower [ question; input_layout; button_layout; feedback_layout ]
  in

  Popup.one_button ~button:"Close" ~dst:parent_layout content

(*general function for when a image triggers a hint/popup (default image and
  then clicked image)*)
let toggle_image ?w ?h ?x ?y ?(noscale = false) ~closed_image ~open_image
    ~puzzle_message ~on_answer room_layout () =
  let img = W.image ?w ?h ~noscale closed_image in
  let state = ref Closed in
  let on_click _ _ _ =
    match !state with
    | Closed ->
        puzzle_popup puzzle_message
          (fun answer ->
            let result = on_answer answer in
            (* Only open if answer is correct *)
            if result.is_correct then begin
              Image.set_file (W.get_image img) open_image;
              state := Open;
              W.update img
            end;
            result.message)
          room_layout ()
        |> ignore
        puzzle_popup puzzle_message
          (fun answer ->
            let result = on_answer answer in
            (* Only open if answer is correct *)
            if result.is_correct then begin
              Image.set_file (W.get_image img) open_image;
              state := Open;
              W.update img
            end;
            result.message)
          room_layout ()
        |> ignore
    | Open ->
        Image.set_file (W.get_image img) closed_image;
        state := Closed;
        W.update img
  in
  W.connect_main img img on_click Trigger.buttons_up |> W.add_connection img;
  (L.resident ?x ?y img, state) *)

(** another image toggling, except this time it changes the background, not just
    the image itself*)
let toggle_image_with_bg_change ?w ?h ?x ?y ?(noscale = false) ~closed_image
    ~open_image ~bg_widget ~new_bg_image ~puzzle_message ~on_answer room_layout
    () =
  let img = W.image ?w ?h ~noscale closed_image in
  let state = ref Closed in
  let on_click _ _ _ =
    match !state with
    | Closed ->
        puzzle_popup puzzle_message
          (fun answer ->
            let result = on_answer answer in
            if result.is_correct then begin
              (* Change the doorknob image *)
              Image.set_file (W.get_image img) open_image;
              (* Change the background image *)
              Image.set_file (W.get_image bg_widget) new_bg_image;
              state := Open;
              W.update img;
              W.update bg_widget
            end;
            result.message)
          room_layout ()
        |> ignore
    | Open ->
        Image.set_file (W.get_image img) closed_image;
        state := Closed;
        W.update img
  in
  W.connect_main img img on_click Trigger.buttons_up |> W.add_connection img;
  (L.resident ?x ?y img, state)

(* actual display logic *)
(* actual display logic *)
let () =
  let bg_w, bg_h = (1280, 720) in
  let current_state = ref Intro1 in
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

  let navigation_arrow ~x ~y ~image ~target_room ~target_screen ~main_layout ()
      =
    let arrow = W.image ~noscale:true image in
    let arrow_layout = L.resident ~x ~y arrow in

    let on_click _ _ _ =
      current_state := target_room;
      L.set_rooms main_layout [ target_screen ]
    in

    W.connect_main arrow arrow on_click Trigger.buttons_up
    |> W.add_connection arrow;
    arrow_layout
  in

  let treasure_room, treasure_state =
    toggle_image ~x:800 ~y:470 ~w:325 ~h:163
      ~closed_image:"images/chest_closed.png"
      ~open_image:"images/chest_open.png"
      ~puzzle_message:
        "To unlock the chest, answer this question: What is a female camel \
         called?"
      ~on_answer:(fun answer ->
        if String.lowercase_ascii answer = "cow" then
          {
            is_correct = true;
            message =
              "Riches uncovered, but danger remains. Seek the sarcophagus \
               before it's too late.";
          }
        else { is_correct = false; message = "Wrong answer: " ^ answer })
      screen3 ()
  in
  in

  let casket_room, casket_state =
    toggle_image ~x:370 ~y:240 ~w:570 ~h:300
      ~closed_image:"images/casket_closed.png"
      ~open_image:"images/casket_open.png"
      ~puzzle_message:
        "To open the sarcophagus, determine whether this definition type \
         checks: let x = 2 +. 3.0"
      ~puzzle_message:
        "To open the sarcophagus, determine whether this definition type \
         checks: let x = 2 +. 3.0"
      ~on_answer:(fun answer ->
        if
          String.lowercase_ascii answer = "no"
          || String.lowercase_ascii answer = "false"
        then
          {
            is_correct = true;
            message =
              "You've awakened the mummy... now seize your chance to escape \
               the tomb!";
          }
        else { is_correct = false; message = "Wrong answer: " ^ answer })
        if
          String.lowercase_ascii answer = "no"
          || String.lowercase_ascii answer = "false"
        then
          {
            is_correct = true;
            message =
              "You've awakened the mummy... now seize your chance to escape \
               the tomb!";
          }
        else { is_correct = false; message = "Wrong answer: " ^ answer })
      screen3 ()
  in
  let lock_room, lock_state =
    toggle_image ~x:640 ~y:370 ~w:30 ~h:45
      ~closed_image:"images/lock_closed.png" ~open_image:"images/lock_open.png"
      ~puzzle_message:
        "To open the lock, enter the numbers you obtained from the \
         heiroglyphics in the corridor. Use the format XXXX where X is a \
         single digit."
      ~on_answer:(fun answer ->
        if String.lowercase_ascii answer = "6229" then
          {
            is_correct = true;
            message =
              "You've entered the correct code... now escape the corridor and \
               go quickly to the next room!!";
          }
        else { is_correct = false; message = "Wrong answer: " ^ answer })
      screen4 ()
  in
  let h1_room, h1_state =
    toggle_image ~x:1030 ~y:400 ~w:125 ~h:125 ~closed_image:"images/h_1.png"
      ~open_image:"images/h1_dark.png"
      ~puzzle_message:
        "To decode this hieroglyphic, evaluate the result of this expression: \
         (18 / 5) + (18 mod 5)"
      ~on_answer:(fun answer ->
        if String.lowercase_ascii answer = "6" then
          {
            is_correct = true;
            message =
              "You're one step closer to escaping this room...but you still \
               need to decode the other heiroglyphics! Remember your answer to \
               this riddle.";
          }
        else { is_correct = false; message = "Wrong answer: " ^ answer })
      screen4 ()
  in

  let h2_room, h2_state =
    toggle_image ~x:240 ~y:180 ~w:150 ~h:150 ~closed_image:"images/h_2.png"
      ~open_image:"images/h2_dark.png"
      ~puzzle_message:
        "To decode this hieroglyphic, answer this question: How many humps \
         does a Bactrian camel have?"
      ~on_answer:(fun answer ->
        if String.lowercase_ascii answer = "2" then
          {
            is_correct = true;
            message =
              "Correct! Now you must decode the final heiroglyphic! Remember \
               your answer to this riddle.";
          }
        else { is_correct = false; message = "Wrong answer: " ^ answer })
      screen4 ()
  in

  let h3_room, h3_state =
    toggle_image ~x:160 ~y:385 ~w:140 ~h:167 ~closed_image:"images/h_3.png"
      ~open_image:"images/h3_dark.png"
      ~puzzle_message:
        "To decode this hieroglyphic, evaluate the result of this expression: \
         if 3 * 2 > 5 then 9 else 4"
      ~on_answer:(fun answer ->
        if String.lowercase_ascii answer = "9" then
          {
            is_correct = true;
            message =
              "You've decoded all the heiroglyphics! Now use your answers to \
               the heiroglyphics to open the lock and escape the corridor!";
          }
        else { is_correct = false; message = "Wrong answer: " ^ answer })
      screen4 ()
  in

  let h4_room, h4_state =
    toggle_image ~x:900 ~y:120 ~w:140 ~h:167 ~closed_image:"images/h_4.png"
      ~open_image:"images/h4_dark.png"
      ~puzzle_message:
        "To decode this hieroglyphic, answer this question: How many rows of \
         eyelashes does a camel have to protect them from the sand?"
      ~on_answer:(fun answer ->
        if String.lowercase_ascii answer = "2" then
          {
            is_correct = true;
            message =
              "You're one step closer to escaping this room...but you still \
               need to decode the other heiroglyphics! Remember your answer to \
               this riddle.";
          }
        else { is_correct = false; message = "Wrong answer: " ^ answer })
      screen4 ()
  in

  let doorknob_room, doorknob_state =
    toggle_image_with_bg_change ~x:910 ~y:280 ~w:40 ~h:80
      ~closed_image:"images/doorknob.png" ~open_image:"images/transparent.png"
      ~bg_widget:stairway_bg ~new_bg_image:"images/stairway.jpg"
      ~puzzle_message:
        "To open the door, evaluate this expression: let x = 3 in let x = x + \
         4 in x"
      ~on_answer:(fun answer ->
        if String.lowercase_ascii answer = "7" then
          {
            is_correct = true;
            message = "The door creaks open, revealing a way forward!";
          }
        else { is_correct = false; message = "Wrong answer: " ^ answer })
      screen5 ()
  in

  let torch_room, torch_state =
    toggle_image ~x:100 ~y:200 ~w:140 ~h:167
      ~closed_image:"images/unlit_torch.png" ~open_image:"images/lit_torch.png"
      ~puzzle_message:
        "To light this torch, answer this question: What OCaml type represents \
         a value that may or may not exist?"
      ~on_answer:(fun answer ->
        if
          String.lowercase_ascii answer = "option"
          || String.lowercase_ascii answer = "option type"
          || String.lowercase_ascii answer = "options"
        then
          {
            is_correct = true;
            message =
              "The lit torch helps you see more clearly. You can now see the \
               spider blocking your path!";
          }
        else { is_correct = false; message = "Wrong answer: " ^ answer })
      screen5 ()
  in

  let spider_room, spider_state =
    toggle_image ~x:1000 ~y:100 ~w:167 ~h:167
      ~closed_image:"images/hanging_spider.png"
      ~open_image:"images/transparent.png"
      ~puzzle_message:
        "To remove the spider, answer this question: Which OCaml concept \
         ensures that once a value is created, it cannot be changed?"
      ~on_answer:(fun answer ->
        if
          String.lowercase_ascii answer = "immutability"
          || String.lowercase_ascii answer = "immutable"
        then
          {
            is_correct = true;
            message =
              "Correct! The spider retreats, clearing your path forward. Now \
               you must open the door to escape this room!";
          }
        else { is_correct = false; message = "Wrong answer: " ^ answer })
      screen5 ()
  in

  let main_layout = L.superpose ~w:bg_w ~h:bg_h [ screen1 ] in
  L.auto_scale main_layout;
  L.disable_resize main_layout;

  let arrow_to_corridor =
    navigation_arrow ~x:1100 ~y:350 ~image:"images/Arrow.png"
      ~target_room:CorridorRoom ~target_screen:screen4 ~main_layout ()
  in

  let arrow_to_stairway =
    navigation_arrow ~x:1100 ~y:350 ~image:"images/Arrow.png"
      ~target_room:StairwayRoom ~target_screen:screen5 ~main_layout ()
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
    [ stairway_bg_layout; doorknob_room; spider_room; torch_room ];

  let transition_to_intro2 _ _ _ =
    current_state := Intro2;
    L.set_rooms main_layout [ screen2 ]
  in
  let transition_to_starting_room _ _ _ =
    current_state := StartingRoom;
    L.set_rooms main_layout [ screen3 ]
  in


  (* Connect click handlers to screens *)
  W.connect_main beginning beginning
    (fun ev x y -> if !current_state = Intro1 then transition_to_intro2 ev x y)
    Trigger.buttons_up
  W.connect_main beginning beginning
    (fun ev x y -> if !current_state = Intro1 then transition_to_intro2 ev x y)
    Trigger.buttons_up
  |> W.add_connection beginning;

  W.connect_main instructions instructions
    (fun ev x y ->
      if !current_state = Intro2 then transition_to_starting_room ev x y)
    Trigger.buttons_up
  W.connect_main instructions instructions
    (fun ev x y ->
      if !current_state = Intro2 then transition_to_starting_room ev x y)
    Trigger.buttons_up
  |> W.add_connection instructions;
  main_layout |> Bogue.of_layout |> Bogue.run
