open Js_of_ocaml
open Dom_html
open Js_utils
open Learnocaml_common
open Editor_lib
open Learnocaml_data
open Learnocaml_data.Editor   
open Learnocaml_data.Exercise.Meta 

module StringMap = Map.Make (String)
(*
(* Internationalization *)
let () = Translate.set_lang ()
let () =
  let translations = [
    "txt_new_exo", [%i"New exercise"];
    "txt_id", [%i"Unique identifier:<br>"];
    "txt_title", [%i"Title (unique too):<br>"];
    "txt_descr", [%i"Description of the exercise:<br>"];
    "txt_diff", [%i"Difficulty level:<br>"];
    "cancel", [%i"Cancel"];
    "save", [%i"Save"];
  ] in
  Translate.set_string_translations translations
 *)


let getString = function
  | None -> failwith "incorrect_input"
  | Some input -> Js.to_string input##.value
let getStringOpt = function
  | None -> None
  | Some input -> Some (Js.to_string input##.value)
let getFloatOpt = function
  | None -> None
  | Some input -> float_of_string_opt (Js.to_string input##.value)

module H = Tyxml_js.Html
open Lwt.Infix

let previous_id = match (arg "id") with
  | exception Not_found -> ""
  | s -> s
let save = getElementById "save"
let identifier = getElementById_coerce "identifier" CoerceTo.input
let title = getElementById_coerce "title" CoerceTo.input
let description = getElementById_coerce "description" CoerceTo.textarea
let difficulty = getElementById_coerce "difficulty" CoerceTo.select
let solution, question, template, test,
    previous_title, previous_diff, prelude, prepare =
  match get_editor_state previous_id with
  | exception Not_found ->
     "", "", "",  "", "", 0., "", ""
  | {metadata; exercise; _ } ->
     exercise.solution, exercise.descr, exercise.template, exercise.test,
     metadata.title, metadata.stars, exercise.prelude, exercise.prepare
 

let () = match identifier with
  | None -> ()
  | Some input -> input##.value := Js.string previous_id
let () = match title with
  | None -> ()
  | Some input -> input##.value := Js.string previous_title
let previous_descr =
  match get_description previous_id with
  | exception Not_found -> None
  |  d -> Some d
let () = match previous_descr with
  | Some d -> setInnerHtml (getElementById "description") d
  | None -> ()
let () = match difficulty with
  | None -> ()
  | Some select -> select##.value := Js.string (string_of_float previous_diff)
 

let resultOptionToBool = function
  | None -> false
  | Some _ -> true
let isIdCorrect s =
  resultOptionToBool (Regexp.string_match (Regexp.regexp "^[a-z0-9_-]+$") s 0)
let isTitleCorrect s =
  (resultOptionToBool (Regexp.string_match (Regexp.regexp "^[^ \t]") s 0)) &&
    (resultOptionToBool (Regexp.string_match (Regexp.regexp ".*[^ \t]$") s 0))


let store id title description diff =
  let metadata= {id=Some id;title ;
     short_description= Some description;
     stars=diff; kind=Exercise;
     author=[];focus=[];requirements=[];
     forward=[];backward=[]}
  in
  let state =
    match get_editor_state previous_id with
    | exception Not_found -> new_state metadata
    | e ->
       {exercise=e.exercise ;metadata}
  in
  update_index state


let id_error = getElementById "id_error"
let title_error = getElementById "title_error"

let () = save##.onclick := handler (fun _ ->
  let id = getString identifier
  and titre = getString title
  and description = getString description
  and diff = match getFloatOpt difficulty with
    | None -> 0.
    | Some x -> x in
  let id_correct = isIdCorrect id
  and id_unique = idUnique id
  and title_correct = isTitleCorrect titre
  and title_unique = titleUnique titre in
  (if not id_correct then
    setInnerHtml id_error [%i"Incorrect identifier: an identifier \
                              can't be empty, \
                              and only lower case letters, numerals, dashes \
                              and underscores are allowed"]
  else if not id_unique then
    setInnerHtml id_error [%i"This identifier is already used, \
                              please choose another one"]
  else
    setInnerHtml id_error "");
  (if not title_correct then
    setInnerHtml title_error [%i"Incorrect title: a title can't be empty, \
                                 or begin or end with a space or a tab"]
  else if not title_unique then
     setInnerHtml title_error
       [%i"This title is already used, please choose another one"]
  else
    setInnerHtml title_error "");
  if id_correct && title_correct && id_unique && title_unique then
    begin
      
      store id titre description diff;
      Dom_html.window##.location##assign
        (Js.string ("editor.html#id=" ^ id));
 
    end;
  Js._true
) 
