open Js_of_ocaml
open Js_utils
open Str
open Lwt.Infix
open Dom_html
open Learnocaml_common

module StringMap = Map.Make (String)


let init_tabs, select_tab =
  let names = [ "solution"; "spec"; "suite" ] in
  let current = ref "suite" in
  let select_tab name =
    set_arg "tab" name ;
    Manip.removeClass
      (find_component ("learnocaml-tab-" ^ !current))
      "front-tab" ;
    Manip.addClass
      (find_component ("learnocaml-tab-" ^ name))
      "front-tab" ;
    current := name in
  let init_tabs () =
    current := begin try
        let requested = arg "tab" in
        if List.mem requested names then requested else "suite"
      with Not_found -> "suite"
    end ;
    List.iter
      (fun name ->
         Manip.removeClass
           (find_component ("learnocaml-tab-" ^ name))
           "front-tab" )
      names ;
    select_tab !current in
  init_tabs, select_tab 

let id = arg "id";;               
let name = match getElementById_coerce "name" CoerceTo.input with
    None -> failwith "element inconnu"
   |Some s -> s;;
let ty = match getElementById_coerce "ty" CoerceTo.input with
    None -> failwith "element inconnu"
   |Some s -> s;;
let solution = getElementById "solution";;
let spec = getElementById "spec";;
let suite = getElementById "suite";;
let input_suite = match getElementById_coerce "input" CoerceTo.textarea with
    None -> failwith "element inconnu"
  | Some s -> s;;
let output_suite = match getElementById_coerce "output" CoerceTo.textarea with
    None -> failwith "element inconnu"
  | Some s -> s;;
let save = getElementById "save";;

let setInnerHtml elt s =    
  elt##.innerHTML:=Js.string s ;;
let _ = solution##.onclick:= handler (fun _ -> select_tab "solution"; Js._true);;

let _ = spec##.onclick:= handler (fun _ -> select_tab "spec"; Js._true);;
let _ = suite##.onclick:= handler (fun _ -> select_tab "suite"; Js._true);;
let _ = save##.onclick:= handler (fun _ ->
   if arg "tab" = "suite" then
      begin
        let open Learnocaml_exercise_state in
        let name = Js.to_string name##.value in
        let ty = Js.to_string ty##.value in
        let type_question = Suite in
        let input = Js.to_string input_suite##.value in
        let output = Js.to_string  output_suite##.value in
        let extra_alea = 0 in
        let question = {name; ty; type_question; input; output; extra_alea} in
        let open Editor_lib in
        let testhaut = get_testhaut id in
        let testhaut = StringMap.add "1" question testhaut in
        save_testhaut testhaut id;
        Dom_html.window##.location##assign
        (Js.string ("editor.html#id=" ^ id ^ "&action=open"))
        
      end;
   Js._true)
