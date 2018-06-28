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
let solution = getElementById "solution";;
let spec = getElementById "spec";;
let suite = match getElementById_coerce "suite" CoerceTo.input with
    None -> failwith ""
  |Some s-> s
;;


let solutionInput = match getElementById_coerce "sol-tests" CoerceTo.textarea with
    None -> failwith "element solutionInput inconnu"
  | Some s -> s;;
let extraAleaSol =match getElementById_coerce "sol-gen" CoerceTo.input with
    None -> failwith "element extraAleaSol inconnu"
  | Some s -> s;;

let specInput = match getElementById_coerce "spec-tests" CoerceTo.textarea with
    None -> failwith "element specInput inconnu"
  | Some s -> s;;

let extraAleaSpec = match getElementById_coerce "spec-gen" CoerceTo.input with
    None -> failwith "element extraAleaSpec inconnu"
  | Some s -> s;;
let specif = match getElementById_coerce "specif" CoerceTo.textarea with
    None -> failwith "element spec inconnu"
  | Some s -> s;;


let input_suite = match getElementById_coerce "input" CoerceTo.textarea with
    None -> failwith "element input_suite inconnu"
  | Some s -> s;;
let output_suite = match getElementById_coerce "output" CoerceTo.textarea with
    None -> failwith "element output_suite inconnu"
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
    
       let save_suite () =
        let name = Js.to_string name##.value in
        let ty = Js.to_string ty##.value in
        let type_question = Suite in
        let input = Js.to_string input_suite##.value in
        let output = Js.to_string  output_suite##.value in
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
  
  
                      
(* restore suite fields if they are not empty *)
    
let _ =match arg "questionid" with
  exception Not_found -> ()
  | qid ->let testhaut=get_testhaut id in
      let name_elt=name in
      let ty_elt=ty in
      match StringMap.find qid testhaut with
        {name;ty;type_question;input;output}->if type_question= Suite then
          input_suite##.value:=Js.string input;
          output_suite##.value:=Js.string output;
          name_elt##.value:=Js.string name;
          suite##.checked := Js.bool true;
          ty_elt##.value:=Js.string ty;
          select_tab "suite";
;;


let _ = solution##.onclick:= handler (fun _ -> select_tab "solution"; Js._true);;
let _ = spec##.onclick:= handler (fun _ -> select_tab "spec"; Js._true);;
let _ = suite##.onclick:= handler (fun _ -> select_tab "suite"; Js._true);;
let _ = save##.onclick:= handler (fun _ ->
   if arg "tab" = "suite" then
     save_suite ();
   if arg "tab" = "solution" then
     begin
        let name = Js.to_string name##.value in
        let ty = Js.to_string ty##.value in
        let type_question = Solution in
        let input = Js.to_string solutionInput##.value in
        let output = "" in
        let extra_alea = int_of_string (Js.to_string extraAleaSol##.value) in
        let question = {name; ty; type_question; input; output; extra_alea} in
        let testhaut = get_testhaut id in
        let question_id = compute_question_id testhaut in
        let testhaut = StringMap.add question_id question testhaut in
        save_testhaut testhaut id;
        Dom_html.window##.location##assign
        (Js.string ("editor.html#id=" ^ id ^ "&action=open"))        
     end;
   if arg "tab" = "spec" then
     begin
        let open Learnocaml_exercise_state in
        let name = Js.to_string name##.value in
        let ty = Js.to_string ty##.value in
        let type_question = Spec in
        let input = Js.to_string specInput##.value in
        let output = Js.to_string specif##.value in
        let extra_alea = int_of_string (Js.to_string extraAleaSpec##.value) in
        let question = {name; ty; type_question; input; output; extra_alea} in
        let open Editor_lib in
        let testhaut = get_testhaut id in
        let question_id = compute_question_id testhaut in
        let testhaut = StringMap.add question_id question testhaut in
        save_testhaut testhaut id;
        Dom_html.window##.location##assign
        (Js.string ("editor.html#id=" ^ id ^ "&action=open"))
     end;
   Js._true)
                       
