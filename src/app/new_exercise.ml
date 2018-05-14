open Js_utils
open Lwt
open Learnocaml_index
open Learnocaml_common
module StringMap = Map.Make (String)
let a =ref [];;

let test () =
 Server_caller.fetch_exercise_index () >>= fun index ->
 let format_exercise_list all_exercise_states =
   let rec format_contents lvl acc contents =
     match contents with
     | Learnocaml_exercises exercises ->
         StringMap.fold
           (fun exercise_id { exercise_kind ;
                              exercise_title ;
                              exercise_short_description ;
                              exercise_stars } acc ->((exercise_id, exercise_title) :: acc)) exercises acc
     | Groups groups ->
         StringMap.fold
           (fun _ { group_title ; group_contents } acc ->
              format_contents (succ lvl) acc group_contents)
           groups acc in
 format_contents 1 [] index in
 let list_div =
   (format_exercise_list Learnocaml_local_storage.(retrieve all_exercise_states)) in
 a:= list_div;
 Lwt.return list_div
;;

let f =function
    []->failwith
  |(a,b)::l-> b
;;


















open Js_of_ocaml;;
open Dom_html;;

let setInnerHtml elt s =    
  elt##innerHTML<-Js.string s ;;

(*
let checkbox txt checked action =
  let b = Dom_html.createInput ~_type:(Js.string "checkbox") doc in
  b##.checked := Js.bool checked;
  b##.onclick :=
    Dom_html.handler (fun _ -> action (Js.to_bool b##.checked); Js._true);
  let lab = Dom_html.createLabel doc in
  Dom.appendChild lab b;
  Dom.appendChild lab (doc##createTextNode (Js.string txt));
  lab 
 *)




let saveb=getElementById "save";;
(*let title =getElementById "title" in
let identity=getElementById "identity" in
let description=getElementById "description" in*)
let para =getElementById ("para") in
(*let dificulty=getElementById_coerce "dificulty" CoerceTo.input in
match dificulty with
| None ->
    failwith "fail"
|Some input -> 
 saveb##.onclick:=handler (fun _ ->  setInnerHtml para (Js.to_string ( input##.value) )  ;Js._true);; 
*)
saveb##onclick<-handler ( fun _ ->  setInnerHtml para (f (!a)) ) ;Js._true);; 

