open Js_utils
open Js_of_ocaml
open Str
open Js_of_ocaml
open Dom_html
open Learnocaml_common
open Learnocaml_exercise_state
module StringMap = Map.Make (String)
    
let setInnerHtml elt s =    
  elt##.innerHTML := Js.string s

let set_string_translations () =
  let translations = [
  "txt_new_exo", [%i"New exercise"];
  "txt_id", [%i"Unique identifier:<br>"];
  "txt_title", [%i"Title (unique too):<br>"];
  "txt_descr", [%i"Description of the exercise:<br>"];
  "txt_diff", [%i"Difficulty level:<br>"];
  "cancel", [%i"Cancel"];
  "save", [%i"Save"];
  ] in
  List.iter
  (fun (id, text) -> Manip.setInnerHtml (find_component id) text)
  translations

let set_lang () =
  match Js.Optdef.to_option (Dom_html.window##.navigator##.language) with
  | Some l -> Ocplib_i18n.set_lang (Js.to_string l)
  | None ->
    match Js.Optdef.to_option (Dom_html.window##.navigator##.userLanguage) with
    | Some l -> Ocplib_i18n.set_lang (Js.to_string l)
    | None -> ()

let transResultOption = function
  |None -> false
  |Some s-> true;;
let idOk s = transResultOption (Regexp.string_match (Regexp.regexp "^[a-z0-9_-]+$") s 0);;
let titreOk s = (transResultOption (Regexp.string_match (Regexp.regexp "^[^ \t]") s 0)) &&
                (transResultOption (Regexp.string_match (Regexp.regexp ".*[^ \t]$") s 0));;

let toString = function
  |None -> failwith "incorrect_input"
  |Some input -> Js.to_string input##.value
let toStringOpt = function
  | None -> None
  | Some input -> Some (Js.to_string input##.value)
let toFloatOpt = function
  | None -> None
  | Some input -> float_of_string_opt (Js.to_string input##.value)
let previousId = match (arg "id") with
  |exception Not_found -> ""
  |s -> s

let save = getElementById "save"
let identifier = getElementById_coerce "identifier" CoerceTo.input
let title = getElementById_coerce "title" CoerceTo.input
let descr = getElementById_coerce "description" CoerceTo.textarea
let difficulty = getElementById_coerce "difficulty" CoerceTo.select
let incipit=""
let  solution, question, template, test, previousTitre, previousDiff, prelude, prepare =
  match Learnocaml_local_storage.(retrieve (editor_state previousId)) with
  | exception Not_found ->  "", "", "", {testml="";testhaut=StringMap.empty}, "",0.0,"",""
  | {Learnocaml_exercise_state.metadata ; solution  ; question ; template ; test ; 
     mtime;prelude;prepare } ->  solution, question, template, test, metadata.titre, metadata.diff, prelude, prepare
                                 
let id_error = getElementById "id_error"
let title_error = getElementById "title_error"

let previousDescr=
  let open Learnocaml_exercise_state in
  let exos=Learnocaml_local_storage.(retrieve (index_state "index")).exos in
  let open Learnocaml_index in
  let exo =
    match (StringMap.find_opt previousId exos) with
          |None -> {exercise_kind=Learnocaml_exercise;exercise_title="";exercise_short_description=None;exercise_stars=1.5}
          |Some s->s
  in  exo.exercise_short_description

let _ = match previousDescr with
  | Some d -> setInnerHtml (getElementById "description") d
  | None -> setInnerHtml (getElementById "description") ""

let _= match identifier with
    None ->()
  | Some input->input##.value:=Js.string previousId

let _= match title with
    None ->()
  | Some input->input##.value:=Js.string previousTitre
          
let d= previousDiff
      
let _ =match difficulty with
  |None-> ()
  |Some select->select##.value:=Js.string (string_of_float d)

let _ = save##.onclick:= handler (fun _ ->
  let id = toString identifier in
  let titre = toString title in
  let description = toString descr in
  let diff =match toFloatOpt difficulty with
      None -> 0.0
    |Some f-> f
  in
  let metadata ={id;titre;description;diff} in
  let store () =if (previousId!="") then Learnocaml_local_storage.(delete (editor_state previousId));
    Learnocaml_local_storage.(store (editor_state id))
      { Learnocaml_exercise_state.metadata ; solution;incipit  ; question ; template  ; test ;  prelude;prepare;
        mtime = gettimeofday () } in
  
  let idUnique () =if id = previousId then true else
    match Learnocaml_local_storage.(retrieve (editor_state id)) with
    | exception Not_found -> true
    | _ -> false in
  
  let titleUnique () =
    let exos=
    match Learnocaml_local_storage.(retrieve (index_state "index")) with
    |{Learnocaml_exercise_state.exos ;mtime}-> exos
    in
    let open Learnocaml_index in
    if  previousTitre=titre then true else
    match StringMap.find_first_opt (fun key->(StringMap.find key exos).exercise_title=titre) exos with
      None->true
    | _ -> false 
  in
  let store2 () =
    let exercise_title = titre in
    let exercise_stars = diff in
    let open Learnocaml_index in
    let exercise_kind = Learnocaml_exercise in
    let exercise_short_description =Some description in
    let exo = {exercise_kind; exercise_stars; exercise_title; exercise_short_description} in
    match Learnocaml_local_storage.(retrieve (index_state "index")) with
    | {Learnocaml_exercise_state.exos; mtime} ->
        let anciensexos = if (previousId!="") then StringMap.remove previousId exos else exos in
        let exos = StringMap.add id exo anciensexos in
        let index = {Learnocaml_exercise_state.exos; mtime = gettimeofday ()} in
        Learnocaml_local_storage.(store (index_state "index")) index;
  in
  let id_correct = idOk id in
  let id_unique = idUnique () in
  let title_correct = titreOk titre in
  let title_unique = titleUnique () in
  if not id_correct && not title_correct then
    begin
      setInnerHtml id_error [%i"Incorrect identifier: an identifier can't be empty, \
                             and only lower case letters, numerals, dashes \
                             and underscores are allowed"];
      setInnerHtml title_error [%i"Incorrect title: a title can't be empty, \
                             or begin or end with a space or a tab"]
    end
  else if not id_correct && title_correct && not title_unique then
    begin
      setInnerHtml id_error [%i"Incorrect identifier: an identifier can't be empty, \
                             and only lower case letters, numerals, dashes \
                             and underscores are allowed"];
      setInnerHtml title_error [%i"This title is already used, please choose another one"]
    end
  else if not id_correct && title_correct && title_unique then
    begin
      setInnerHtml id_error [%i"Incorrect identifier: an identifier can't be empty, \
                             and only lower case letters, numerals, dashes \
                             and underscores are allowed"];
      setInnerHtml title_error ""
    end
  else if id_correct && not id_unique && not title_correct then
    begin
      setInnerHtml id_error [%i"This identifier is already used, please choose another one"];
      setInnerHtml title_error [%i"Incorrect title: a title can't be empty, \
                                or begin or end with a space or a tab"]
    end
  else if id_correct && not id_unique && title_correct && not title_unique then
    begin
      setInnerHtml id_error [%i"This identifier is already used, please choose another one"];
      setInnerHtml title_error [%i"This title is already used, please choose another one"]
    end
  else if id_correct && not id_unique && title_correct && title_unique then
    begin
      setInnerHtml id_error [%i"This identifier is already used, please choose another one"];
      setInnerHtml title_error ""
    end
  else if id_correct && id_unique && not title_correct then
    begin
      setInnerHtml id_error "";
      setInnerHtml title_error [%i"Incorrect title: a title can't be empty, \
                                or begin or end with a space or a tab"]
    end
  else if id_correct && id_unique && title_correct && not title_unique then
    begin
      setInnerHtml id_error "";
      setInnerHtml title_error [%i"This title is already used, please choose another one"]
    end
  else
    begin
      setInnerHtml id_error "";
      setInnerHtml title_error "";
      store ();
      store2 ();
      Dom_html.window##.location##assign
        (Js.string ("editor.html#id=" ^ id ^ "&action=open"));
    end
  ; Js._true
)

let () = set_lang ()
let () = set_string_translations ()
