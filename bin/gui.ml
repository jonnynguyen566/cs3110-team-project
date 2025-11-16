open Bogue
module W = Widget
module L = Layout

type toggle_state =
  | Closed
  | Open

type answer_result = {
  is_correct : bool;
  message : string;
}

(*actual pop up for when the user toggles an image that has a hint*)
let puzzle_popup message on_submit parent_layout () =
  let question = W.text_display ~w:250 message |> L.resident in
  let input = W.text_input ~max_size:100 ~prompt:"Your answer:" () in
  let input_layout = L.resident input in

  (* text display for feedback messages *)
  let feedback = W.text_display ~w:250 "" in
  let feedback_layout = L.resident feedback in

  let submit_button = W.button "Submit" ~action:(fun _ ->
    let answer = W.get_text input in
    let result = on_submit answer in
    (* Display the result message in the feedback widget *)
    W.set_text feedback result
  ) in
  let button_layout = L.resident submit_button in
  
  let content = L.tower [ question; input_layout; button_layout; feedback_layout ] in
  
  Popup.one_button ~button:"Close" ~dst:parent_layout content

(*general function for when a image triggers a hint/popup (default image and
  then clicked image)*)
let toggle_image ?w ?h ?x ?y ?(noscale = false) ~closed_image ~open_image ~puzzle_message ~on_answer
    room_layout () =
  let img = W.image ?w ?h ~noscale closed_image in
  let state = ref Closed in
  let on_click _ _ _ =
    match !state with
    | Closed ->
        puzzle_popup puzzle_message (fun answer ->
          let result = on_answer answer in
          (* Only open if answer is correct *)
          if result.is_correct then begin
            Image.set_file (W.get_image img) open_image;
            state := Open;
            W.update img;
          end;
          result.message) room_layout () |> ignore
    | Open ->
        Image.set_file (W.get_image img) closed_image;
        state := Closed;
        W.update img
  in
  W.connect_main img img on_click Trigger.buttons_up |> W.add_connection img;
  (L.resident ?x ?y img, state)

(* actual display logic  *)
let () =
  let bg_w, bg_h = (1280, 720) in
  let background =
    L.resident ~w:bg_w ~h:bg_h
      (W.image ~w:bg_w ~h:bg_h ~noscale:true "images/starting_room.jpg")
  in

  let main_layout = L.superpose ~w:bg_w ~h:bg_h [ background ] in
  L.auto_scale main_layout;
  L.disable_resize main_layout;
  let treasure_room, treasure_state =
    toggle_image ~x:800 ~y:470 ~w:325 ~h:163
      ~closed_image:"images/chest_closed.png"
      ~open_image:"images/chest_open.png" 
      ~puzzle_message:"To unlock the chest, answer this question: What is a female camel called?"
      ~on_answer:(fun answer ->
        if String.lowercase_ascii answer = "cow" then
          { is_correct = true; message = "Riches uncovered, but danger remains. Seek the sarcophagus before it's too late." }
        else
          { is_correct = false; message = "Wrong answer: " ^ answer }
      )
      main_layout ()
  in

  let casket_room, casket_state =
    toggle_image ~x:370 ~y:240 ~w:570 ~h:300
      ~closed_image:"images/casket_closed.png"
      ~open_image:"images/casket_open.png"
      ~puzzle_message:"To open the sarcophagus, determine whether this definition type checks: let x = 2 +. 3.0"
      ~on_answer:(fun answer ->
        if String.lowercase_ascii answer = "no" || String.lowercase_ascii answer = "false" then
          { is_correct = true; message = "You've awakened the mummy... now seize your chance to escape the tomb!" }
        else
          { is_correct = false; message = "Wrong answer: " ^ answer }
      )
      main_layout ()
  in

  L.set_rooms main_layout [ background; treasure_room; casket_room ];
  main_layout |> Bogue.of_layout |> Bogue.run 
