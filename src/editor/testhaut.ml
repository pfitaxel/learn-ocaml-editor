open Js_of_ocaml
open Js_utils
open Str
open Lwt.Infix
open Dom_html
open Learnocaml_common

module StringMap = Map.Make (String)


let () = Translate.set_lang ()
let () =
  let translations = [
    "cancel", [%i"Cancel"];
    "save", [%i"Save"];
    "txt_test", [%i"Test"];
    "txt_name", [%i"Function name: "];
    "txt_ty", [%i"Type: "];
    "txt_sol", [%i"Solution"];
    "txt_spec", [%i"Specification"];
    "txt_suite", [%i"Tests suite"];
    "txt_input_sol", [%i"Arguments:<br>"];
    "txt_gen_sol", [%i"Number of generated tests:<br>"];
    "txt_datalist_sol", [%i"Tester:<br>"];
    "txt_sampler_sol", [%i"Sampler:<br>"];
    "txt_input_spec", [%i"Arguments:<br>"];
    "txt_gen_spec", [%i"Number of generated tests:<br>"];
    "txt_datalist_spec", [%i"Tester:<br>"];
    "txt_sampler_spec", [%i"Sampler:<br>"];
    "txt_spec_specification", [%i"Specification:<br>"];
    "txt_suite_input", [%i"Arguments and results:<br>"];
    "txt_datalist_suite", [%i"Tester:<br>"];
  ] in Translate.set_string_translations translations


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
    None -> failwith "unknown element name"
   |Some s -> s;;
let ty = match getElementById_coerce "ty" CoerceTo.input with
    None -> failwith "unknown element ty"
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

let samplerSol = match getElementById_coerce "sol-sampler" CoerceTo.input with
  | None -> failwith "unknown element sampler sol"
  | Some s -> s;;
                
let samplerSpec = match getElementById_coerce "spec-sampler" CoerceTo.input with
  | None -> failwith "unknown element sampler spec"
  | Some s -> s;;
                
let extraAleaSol =match getElementById_coerce "sol-gen" CoerceTo.input with
    None -> failwith "unknown element extraAleaSol"
  | Some s -> s;;

let extraAleaSpec = match getElementById_coerce "spec-gen" CoerceTo.input with
    None -> failwith "unknown element extraAleaSpec"
  | Some s -> s;;

let datalistSol = match getElementById_coerce "sol-datalist" CoerceTo.input with
  | None -> failwith "unknown element datalistSol"
  | Some s -> s;;

let datalistSpec = match getElementById_coerce "spec-datalist" CoerceTo.input with
    None -> failwith "unknown element datalistSpec"
  | Some s -> s;;

let datalistSuite = match getElementById_coerce "suite-datalist" CoerceTo.input with
    None -> failwith "unknown element datalistSuite"
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
  in string_of_int (mi key_list);;

open Editor_lib
open Learnocaml_exercise_state

let input_solution_editor = find_component "learnocaml-tab-solution-input";;
let editor_input_solution = Ocaml_mode.create_ocaml_editor (Tyxml_js.To_dom.of_div input_solution_editor) ;;
let ace_input_sol = Ocaml_mode.get_editor editor_input_solution ;;
let _ = Ace.set_contents ace_input_sol ("[]");
        Ace.set_font_size ace_input_sol 18;;

let input_spec_editor = find_component "learnocaml-tab-spec-input" 
let editor_input_spec = Ocaml_mode.create_ocaml_editor (Tyxml_js.To_dom.of_div input_spec_editor) 
let ace_input_spec = Ocaml_mode.get_editor editor_input_spec 
let _ = Ace.set_contents ace_input_spec ("[]");
        Ace.set_font_size ace_input_spec 18;;

let spec_spec_editor = find_component "learnocaml-tab-spec-spec"
let editor_spec_spec = Ocaml_mode.create_ocaml_editor (Tyxml_js.To_dom.of_div spec_spec_editor) 
let ace_spec_spec = Ocaml_mode.get_editor editor_spec_spec 
let _ =  Ace.set_contents ace_spec_spec ("fun f args ret -> \n ...");
         Ace.set_font_size ace_spec_spec 18;;

let input_suite_editor = find_component "learnocaml-tab-suite-input" 
let editor_input_suite = Ocaml_mode.create_ocaml_editor (Tyxml_js.To_dom.of_div input_suite_editor)
let ace_input_suite = Ocaml_mode.get_editor editor_input_suite 
let _ = Ace.set_contents ace_input_suite ("[]");
        Ace.set_font_size ace_input_suite 18;;
      

let save_suite () =
  let name = Js.to_string name##.value in
  let ty = Js.to_string ty##.value in
  let input = Ace.get_contents ace_input_suite in
  let datalist = Js.to_string datalistSuite##.value in
  let question = TestSuite {name; ty; suite= input;tester=datalist} in
  let testhaut =  get_testhaut id in
  let question_id = match arg "questionid" with
    |exception Not_found ->compute_question_id testhaut
    |qid->qid
  in
  let testhaut = StringMap.add question_id question testhaut in
  save_testhaut testhaut id;;

let save_solution () =
  let name = Js.to_string name##.value in
  let ty = Js.to_string ty##.value in
  let input = Ace.get_contents ace_input_sol in
  let extra_alea = int_of_string (Js.to_string extraAleaSol##.value) in
  let datalist = Js.to_string datalistSol##.value in
  let sampler= Js.to_string samplerSol##.value in
  let question = TestAgainstSol {name; ty; suite=input; gen= extra_alea;tester=datalist;sampler} in
  let testhaut = get_testhaut id in
  let question_id =  match arg "questionid" with
    |exception Not_found ->compute_question_id testhaut
    |qid->qid in
  let testhaut = StringMap.add question_id question testhaut in
  save_testhaut testhaut id;;

  
let save_spec () =
  let open Learnocaml_exercise_state in
  let name = Js.to_string name##.value in
  let ty = Js.to_string ty##.value in
  let input = Ace.get_contents ace_input_spec in
  let output = Ace.get_contents ace_spec_spec in
  let extra_alea = int_of_string (Js.to_string extraAleaSpec##.value) in
  let datalist = Js.to_string datalistSpec##.value in
  let sampler = Js.to_string samplerSpec##.value in
  let question = TestAgainstSpec {name; ty; suite=input;spec= output;gen= extra_alea;tester=datalist;sampler} in
  let open Editor_lib in
  let testhaut = get_testhaut id in
  let question_id =  match arg "questionid" with
    |exception Not_found ->compute_question_id testhaut
    |qid->qid in
  let testhaut = StringMap.add question_id question testhaut in
  save_testhaut testhaut id;;
    
(* restore suite fields if they are not empty *)
    
let _ = match arg "questionid" with
    exception Not_found -> select_tab "suite"; suite##.checked := Js.bool true
  | qid ->let testhaut=get_testhaut id in

      let name_elt=name in
      let ty_elt=ty in
      let suite_elt=suite in
      let spec_elt=spec in
          match StringMap.find qid testhaut with
             | TestSuite {name;ty;suite;tester} ->
                begin
                  Ace.set_contents ace_input_suite suite;
                  name_elt##.value:=Js.string name;
                  suite_elt##.checked := Js.bool true;
                  ty_elt##.value:=Js.string ty;
                  datalistSuite##.value:= Js.string tester;
                  select_tab "suite"
                end;
             | TestAgainstSpec {name;ty;gen;tester;sampler;suite;spec} ->
                begin
                  Ace.set_contents ace_input_spec suite;
                  Ace.set_contents ace_spec_spec spec;
                  name_elt##.value:=Js.string name;
                  spec_elt##.checked := Js.bool true;
                  ty_elt##.value:=Js.string ty;
                  extraAleaSpec##.value:= Js.string (string_of_int gen);
                  datalistSpec##.value:= Js.string tester;
                  samplerSpec##.value:= Js.string sampler;
                  select_tab "spec"
                end;
             | TestAgainstSol {name;ty;gen;tester;sampler;suite} ->
                begin
                  Ace.set_contents ace_input_sol suite;
                  name_elt##.value:=Js.string name;
                  solution##.checked := Js.bool true;
                  ty_elt##.value:=Js.string ty;
                  extraAleaSol##.value:= Js.string (string_of_int gen);
                  datalistSol##.value:=Js.string tester;
                  samplerSol##.value:=Js.string sampler;
                  select_tab "solution"
                end;;



let _ = solution##.onclick:= handler (fun _ -> select_tab "solution"; Js._true);;
let _ = spec##.onclick:= handler (fun _ -> select_tab "spec"; if Ace.get_contents ace_spec_spec ="" then Ace.set_contents ace_spec_spec "fun f args ret -> \n ..."; Js._true);;
let _ = suite##.onclick:= handler (fun _ -> select_tab "suite"; Js._true);;


let transResultOption = function
  |None -> false
  |Some s-> true;;
let nameOk s = transResultOption (Regexp.string_match (Regexp.regexp "^.+") s 0);;
let typeOk s = transResultOption (Regexp.string_match (Regexp.regexp "^.+") s 0);;

let close_frame () =
  let window=Dom_html.window in
  let window=window##.parent in
  let document=window##.document in
  let div= Js.Opt.case (document##getElementById (Js.string "learnocaml-exo-loading"))
      (fun ()-> failwith "titi")
      (fun node->node)
  in
  let exo_list=Js.Opt.case (document##getElementById (Js.string "learnocaml-exo-testhaut-pane"))
      (fun () -> failwith "toto")
      (fun pnode -> pnode)
  in
  let exo_list=Tyxml_js.Of_dom.of_element exo_list in
  Manip.removeChildren exo_list;
  
  let _ =testhaut_init exo_list id  in ();
  div##setAttribute (Js.string "class") (Js.string "loading-layer loaded");;


let toString = function
  |None -> failwith "incorrect_input"
  |Some input -> Js.to_string input##.value

let name_error = getElementById "name_error"
let type_error = getElementById "type_error"

let _ = save##.onclick:= handler (fun _ ->
  let name = Js.to_string name##.value in
  let ty = Js.to_string ty##.value in
  let name_correct = nameOk name in
  let type_correct = typeOk ty in
  if not name_correct && not type_correct then
    begin
      setInnerHtml name_error [%i"Incorrect name: a name can't be empty"];
      setInnerHtml type_error [%i"Incorrect type: a type can't be empty"]
    end
  else if name_correct && not type_correct then
    begin
      setInnerHtml name_error "";
      setInnerHtml type_error [%i"Incorrect type: a type can't be empty"]
    end
  else if not name_correct && type_correct then
    begin
      setInnerHtml name_error [%i"Incorrect name: a name can't be empty"];
      setInnerHtml type_error ""
    end
  else
    begin
      setInnerHtml name_error "";
      setInnerHtml type_error "";
      if arg "tab" = "suite" then
	save_suite ();  
      if arg "tab" = "solution" then
	save_solution ();
      if arg "tab" = "spec" then
        save_spec ();
      close_frame ();
    end;
   Js._true
)

(* Cancel button *)
let cancel = getElementById "cancel"
let _ = cancel##.onclick := handler (fun _ ->
    let _ = close_frame () in ();
    Js._true)
