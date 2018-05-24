
open Js_of_ocaml;;
open Dom_html;;
open Learnocaml_common;;

let setInnerHtml elt s =    
  elt##innerHTML <- Js.string s;;

let save = getElementById "save"
and identifier = getElementById_coerce "identifier" CoerceTo.input in
let id = match identifier with
  | None -> failwith "incorrect_input"
  | Some input -> Js.to_string input##value
and title = getElementById_coerce "title" CoerceTo.input in
let titre = match title with
  | None -> failwith "incorrect_input"
  | Some input -> Js.to_string input##value
and descr = getElementById_coerce "description" CoerceTo.input in
let description = match descr with
  | None -> None
  | Some input -> Some (Js.to_string input##value)
and difficulty = getElementById_coerce "difficulty" CoerceTo.select in
let diff = match difficulty with
  | None -> None
  | Some input -> float_of_string_opt (Js.to_string input##value)
and report = None
and solution = ""
and question = ""
and template = ""
and test = "" in
    save##onclick <- handler (fun _ ->
      Learnocaml_local_storage.(store (editor_state id))
        { Learnocaml_exercise_state.report ; id ; solution ; titre ; question ; template ; diff ; test ; description ;
          mtime = gettimeofday () };
      Dom_html.window##location##assign (Js.string ("editor.html#id="^id^"&action=open"));
      Js._true);;
