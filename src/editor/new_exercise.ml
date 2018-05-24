open Js_of_ocaml;;
open Dom_html;;

let setInnerHtml elt s =    
  elt##innerHTML<-Js.string s ;;


let save=getElementById "save";;
let ident=getElementById_coerce "identifier" CoerceTo.input in
match ident with
| None -> failwith "fail"
|Some input ->
 save##onclick<-handler (fun _ ->Dom_html.window##location##assign (Js.string ("editor.html#id="^(Js.to_string input##value)^"&action=open")) ;Js._true);; 

