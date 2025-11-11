open Bogue
module W = Widget

let () =
  let label = W.label "Cookies:" in
  let count = W.label "0" in
  let button = W.button "Click for more" in
  Layout.flat_of_w ~name:"Counter tutorial"
    ~align:Draw.Center [label; count; button]
  |> Bogue.of_layout
  |> Bogue.run