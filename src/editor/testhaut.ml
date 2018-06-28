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
    None -> failwith "element name inconnu"
   |Some s -> s;;
let ty = match getElementById_coerce "ty" CoerceTo.input with
    None -> failwith "element ty inconnu"
   |Some s -> s;;
let solution = match getElementById_coerce "solution" CoerceTo.input with
    None -> failwith ""
  | Some s -> s;;
let spec = match getElementById_coerce "spec" CoerceTo.input with
    None -> failwith ""
  |Some s -> s;;
let suite = match getElementById_coerce "suite" CoerceTo.input with
    None -> failwith ""
  |Some s-> s;;

let extraAleaSol =match getElementById_coerce "sol-gen" CoerceTo.input with
    None -> failwith "element extraAleaSol inconnu"
  | Some s -> s;;

let extraAleaSpec = match getElementById_coerce "spec-gen" CoerceTo.input with
    None -> failwith "element extraAleaSpec inconnu"
  | Some s -> s;;

let save = getElementById "save";;

let setInnerHtml elt s =    
  elt##.innerHTML:=Js.string s ;;

let compute_question_id test_haut =
  let key_list =List.map (fun (a,b)->int_of_string a) (StringMap.bindings test_haut) in
  let mi coulvois =
    let rec aux c n=match c with
        []->n
      |x::l->if x<>n then aux l n else aux coulvois (n+1)
    in aux coulvois 1
  in string_of_int (mi key_list)
;;

open Editor_lib
open Learnocaml_exercise_state

let input_solution_editor = find_component "learnocaml-tab-solution-input";;
let editor_input_solution = Ocaml_mode.create_ocaml_editor (Tyxml_js.To_dom.of_div input_solution_editor) ;;
let ace_input_sol = Ocaml_mode.get_editor editor_input_solution ;;
let _ = Ace.set_contents ace_input_sol (get_testml id);
        Ace.set_font_size ace_input_sol 18;;

let input_spec_editor = find_component "learnocaml-tab-spec-input" 
let editor_input_spec = Ocaml_mode.create_ocaml_editor (Tyxml_js.To_dom.of_div input_spec_editor) 
let ace_input_spec = Ocaml_mode.get_editor editor_input_spec 
let _ = Ace.set_contents ace_input_spec (get_testml id);
        Ace.set_font_size ace_input_spec 18;;

let spec_spec_editor = find_component "learnocaml-tab-spec-spec"
let editor_spec_spec = Ocaml_mode.create_ocaml_editor (Tyxml_js.To_dom.of_div spec_spec_editor) 
let ace_spec_spec = Ocaml_mode.get_editor editor_spec_spec 
let _ =  Ace.set_contents ace_spec_spec (get_testml id);
         Ace.set_font_size ace_spec_spec 18;;

let input_suite_editor = find_component "learnocaml-tab-suite-input" 
let editor_input_suite = Ocaml_mode.create_ocaml_editor (Tyxml_js.To_dom.of_div input_suite_editor)
let ace_input_suite = Ocaml_mode.get_editor editor_input_suite 
let _ = Ace.set_contents ace_input_suite (get_testml id);
        Ace.set_font_size ace_input_suite 18;;

let output_suite_editor = find_component "learnocaml-tab-suite-output" 
let editor_output_suite = Ocaml_mode.create_ocaml_editor (Tyxml_js.To_dom.of_div output_suite_editor) 
let ace_output_suite = Ocaml_mode.get_editor editor_output_suite 
let _ = Ace.set_contents ace_output_suite (get_testml id);
        Ace.set_font_size ace_output_suite 18;;
       
let save_suite () =
  let name = Js.to_string name##.value in
  let ty = Js.to_string ty##.value in
  let type_question = Suite in
  let input = Ace.get_contents ace_input_suite in
  let output = Ace.get_contents ace_output_suite in
  let extra_alea = 0 in
  let question = {name; ty; type_question; input; output; extra_alea} in
  let testhaut =  get_testhaut id in
  let question_id = match arg "questionid" with
    |exception Not_found ->compute_question_id testhaut
    |qid->qid
  in
  let testhaut = StringMap.add question_id question testhaut in
  save_testhaut testhaut id;
  Dom_html.window##.location##assign
    (Js.string ("editor.html#id=" ^ id ^ "&action=open"));;

let save_solution () =
  let name = Js.to_string name##.value in
  let ty = Js.to_string ty##.value in
  let type_question = Solution in
  let input = Ace.get_contents ace_input_sol in
  let output = "" in
  let extra_alea = int_of_string (Js.to_string extraAleaSol##.value) in
  let question = {name; ty; type_question; input; output; extra_alea} in
  let testhaut = get_testhaut id in
  let question_id = compute_question_id testhaut in
  let testhaut = StringMap.add question_id question testhaut in
  save_testhaut testhaut id;
  Dom_html.window##.location##assign
    (Js.string ("editor.html#id=" ^ id ^ "&action=open"))
  
let save_spec () =
  let open Learnocaml_exercise_state in
  let name = Js.to_string name##.value in
  let ty = Js.to_string ty##.value in
  let type_question = Spec in
  let input = Ace.get_contents ace_input_spec in
  let output = Ace.get_contents ace_spec_spec in
  let extra_alea = int_of_string (Js.to_string extraAleaSpec##.value) in
  let question = {name; ty; type_question; input; output; extra_alea} in
  let open Editor_lib in
  let testhaut = get_testhaut id in
  let question_id = compute_question_id testhaut in
  let testhaut = StringMap.add question_id question testhaut in
  save_testhaut testhaut id;
  Dom_html.window##.location##assign
    (Js.string ("editor.html#id=" ^ id ^ "&action=open"))
    
(* restore suite fields if they are not empty *)
    
let _ = match arg "questionid" with
    exception Not_found -> ()
  | qid ->let testhaut=get_testhaut id in
          let name_elt=name in
          let ty_elt=ty in
          match StringMap.find qid testhaut with
            {name;ty;type_question;input;output} ->
             match type_question with
             | Suite ->
                begin
                  Ace.set_contents ace_input_suite input;
                  Ace.set_contents ace_output_suite output;
                  name_elt##.value:=Js.string name;
                  suite##.checked := Js.bool true;
                  ty_elt##.value:=Js.string ty;
                  select_tab "suite"
                end;
             | Spec ->
                begin
                  Ace.set_contents ace_input_spec input;
                  Ace.set_contents ace_spec_spec output;
                  name_elt##.value:=Js.string name;
                  spec##.checked := Js.bool true;
                  ty_elt##.value:=Js.string ty;
                  select_tab "spec"
                end;
             | _ ->
                begin
                  Ace.set_contents ace_input_sol input;
                  name_elt##.value:=Js.string name;
                  solution##.checked := Js.bool true;
                  ty_elt##.value:=Js.string ty;
                  select_tab "solution"
                end
;;




let _ = solution##.onclick:= handler (fun _ -> select_tab "solution"; Js._true);;
let _ = spec##.onclick:= handler (fun _ -> select_tab "spec"; Js._true);;
let _ = suite##.onclick:= handler (fun _ -> select_tab "suite"; Js._true);;
let _ = save##.onclick:= handler (fun _ ->
   if arg "tab" = "suite" then
     save_suite ();
   if arg "tab" = "solution" then
     save_solution ();
   if arg "tab" = "spec" then
     save_spec ();
   Js._true)
