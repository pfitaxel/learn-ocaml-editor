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
let dificulty=getElementById_coerce "title" CoerceTo.input in
match dificulty with
| None ->
    failwith "fail"
|Some input -> 
 saveb##onclick<-handler (fun _ ->  setInnerHtml para (Js.to_string ( input##value) )  ;Js._true);; 

