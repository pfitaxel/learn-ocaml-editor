open Js_of_ocaml
open Str
open Js_of_ocaml
open Dom_html
open Learnocaml_common

module StringMap = Map.Make (String)

let setInnerHtml elt s =    
  elt##innerHTML <- Js.string s

(* test de validité de l'id et du titre *)
let transResultOption= function
  |None -> false
  |Some s-> true;;
let idOk s =transResultOption (Regexp.string_match (Regexp.regexp "[a-z0-9_-]+$") s 0);;
let titreOk s =(transResultOption (Regexp.string_match (Regexp.regexp "^[^ \t]") s 0))  &&  (transResultOption (Regexp.string_match (Regexp.regexp ".+[^ \t]$") s 0));;

(* conversion *)
let toString = function
  |None -> failwith "incorrect_input"
  |Some input -> Js.to_string input##value
let toStringOpt = function
  | None -> None
  | Some input -> Some (Js.to_string input##value)
let toFloatOpt = function
  | None -> None
  | Some input -> float_of_string_opt (Js.to_string input##value)

(* Élements à récupérer *)
let save = getElementById "save" in
let identifier = getElementById_coerce "identifier" CoerceTo.input in
let title = getElementById_coerce "title" CoerceTo.input in
let descr = getElementById_coerce "description" CoerceTo.textarea in
let difficulty = getElementById_coerce "difficulty" CoerceTo.select in
let report = None in
let solution = "" in
let question = "" in
let template = "" in
let test = "" in
let id_error= getElementById "id_error" in
save##onclick <- handler (fun _ ->
  (* récupération des informations *)
  let id = toString identifier in
  let titre = toString title in
  let description = toStringOpt descr in
  let diff = toFloatOpt difficulty in
  let store () = Learnocaml_local_storage.(store (editor_state id))
      { Learnocaml_exercise_state.report ; id ; solution ; titre ; question ; template ; diff ; test ; description ;
        mtime = gettimeofday () } in
  let titre_unique () =
    match Learnocaml_local_storage.(retrieve (editor_state id)) with
    exception Not_found->true
    |_->false in
  let store2 () =
    let exercise_title = titre in
    let stars = match diff with None -> failwith "" | Some f -> f in
    let exercise_stars = stars in
    let open Learnocaml_index in
    let exercise_kind = Learnocaml_exercise in
    let exercise_short_description = description in
    let exo = {exercise_kind; exercise_stars; exercise_title; exercise_short_description} in
    match Learnocaml_local_storage.(retrieve (index_state "index")) with
    | {Learnocaml_exercise_state.exos;mtime} ->
        let anciensexos = exos in
        let exos = StringMap.add id exo anciensexos in
        let index = {Learnocaml_exercise_state.exos; mtime = gettimeofday ()} in
        Learnocaml_local_storage.(store (index_state "index")) index
    | exception Not_found ->
        let exos = StringMap.singleton id exo in
        let index = {Learnocaml_exercise_state.exos;mtime = gettimeofday ()} in
        Learnocaml_local_storage.(store (index_state "index")) index in

  if titre_unique () then let ()=store (); store2 () in ();
    Dom_html.window##location##assign (Js.string ("editor.html#id="^id^"&action=open"));
  else  setInnerHtml id_error "id pas unique" ; Js._true);;

