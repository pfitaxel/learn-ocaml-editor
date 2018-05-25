open Js_of_ocaml;;
open Str
open Js_of_ocaml;;
open Dom_html;;
open Learnocaml_common;;
module StringMap = Map.Make (String)


let setInnerHtml elt s =    
  elt##innerHTML <- Js.string s;;

(* test si titre et id bon*)
(*
let idOk a = string_match (regexp "[a-z0-9_-]+$") a 0;;
let titreOk a= (string_match (regexp "^[^ \t]") a 0)  && (string_match (regexp ".*[^ \t]$") a 0) && (string_match  (regexp "^$") a 0) ;;

*)
(*conversion*)
let transToString = function
  |None -> failwith "incorrect_input"
  |Some input -> Js.to_string input##value in
let transToStringOpt= function
  | None -> None
  | Some input -> Some (Js.to_string input##value) in
let transToFloatOpt=function
  | None -> None
  | Some input ->Some ( float_of_string (Js.to_string input##value)) in
(*element a récupéré*)
let save = getElementById "save" in
let identifier = getElementById_coerce "identifier" CoerceTo.input in
let title = getElementById_coerce "title" CoerceTo.input in
let descr = getElementById_coerce "description" CoerceTo.textarea in
let difficulty = getElementById_coerce "difficulty" CoerceTo.select in
let para=getElementById "para" in
let report = None in
let solution = "" in
let question = "" in
let template = "" in
let test = "" in
save##onclick <- handler (fun _ ->
                     (* recuperation des info *)
  let id =transToString identifier in
  let titre= transToString title in
  let description=transToStringOpt descr in
  let diff= transToFloatOpt difficulty in
  let setPara =setInnerHtml para  in 
  let store () =Learnocaml_local_storage.(store (editor_state id))
           { Learnocaml_exercise_state.report ; id ; solution ; titre ; question ; template ; diff ; test ; description ;
             mtime = gettimeofday () }
  in
  
  (*let test a b=(if ( idOk a && titreOk b)
                then store ()
                else ()  in *)
   let store2 () =
               
                  let exercise_title=titre in
                  let stars=match diff with None-> failwith " " | Some f-> f in
                  let exercise_stars= stars in
                  let open Learnocaml_index in
                  let exercise_kind=Learnocaml_exercise in
                  let exercise_short_description=  description in
                  let exo={exercise_kind;exercise_stars;exercise_title;exercise_short_description} in
                     
                  
                    match Learnocaml_local_storage.(retrieve (index_state "index")) with
                        {Learnocaml_exercise_state.exos;mtime}->
                          let anciensexos=exos in let exos=StringMap.add id exo anciensexos in
                          let index= {Learnocaml_exercise_state.exos;mtime = gettimeofday ()} in                         
                          Learnocaml_local_storage.(store (index_state "index")) index
                            
                      | exception Not_found ->
                          let exos =StringMap.singleton id exo in
                          let index= {Learnocaml_exercise_state.exos;mtime = gettimeofday ()} in
                          Learnocaml_local_storage.(store (index_state "index")) index
                          
                      
                               
               
  
  in

              Learnocaml_local_storage.init ();   
              store () ;
              store2 ();


              Dom_html.window##location##assign (Js.string ("editor.html#id="^id^"&action=open"));Js._true);;
