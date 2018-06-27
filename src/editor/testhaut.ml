open Js_of_ocaml
open Str
open Js_of_ocaml
open Dom_html
open Learnocaml_common
       
let toString = function
  |None -> failwith "incorrect_input"
  |Some input -> Js.to_string input##.value ;;


let id = arg "id" ;;
let id_question = arg "id_question";;

let checkedSpec = ref false ;;
let checkedSolution = ref false ;;
let checkedSuite = ref false ;;

let save = getElementById "save" ;;

let nameInput = getElementById_coerce "name" CoerceTo.input ;;
let typeInput = getElementById_coerce "ty" CoerceTo.input ;;
let spec = getElementById "spec" ;;
let solution = getElementById "solution" ;;
let suite = getElementById "suite" ;;

let suiteInput = getElementById_coerce "input" CoerceTo.textarea;;
let suiteOutput = getElementById_coerce "output" CoerceTo.textarea;;

let solutionInput = getElementById_coerce "sol-tests" CoerceTo.input;;
let extraAleaSol = getElementById_coerce "sol-gen" CoerceTo.input;;

let specInput = getElementById_coerce "spec-tests" CoerceTo.input;;
let extraAleaSpec = getElementById_coerce "spec-gen" CoerceTo.input;;
let spec = getElementById_coerce "spec" CoerceTo.textarea;;

let name = toString nameInput ;;
let typeFct = toString typeInput ;;
let suiteIn = toString suiteInput;;
let suiteOut = toString suiteOutput ;;
let solIn = toString solutionInput ;;
let aleaSol = toString extraAleaSol ;;
let specIn = toString specInput ;;
let aleaSpec = toString extraAleaSpec ;;
let specif = toString spec ;;

let constructSpec (chkSpec,chkSuite,chkSol) = match (chkSpec,chkSuite,chkSol) with
  |(true,false,false) -> {}
  |(false,true,false) -> {}
  |(false,false,true) -> {}
  |_->failwith "error" ;;




let _ = spec##.onclick := handler
	                     (fun _ -> checkedSpec = ref true;
                                       checkedSuite = ref false;
                                       checkedSolution = ref false;
                                       Js._true);;

let _ = suite##.onclick := handler
	                     (fun _ -> checkedSpec = ref false;
                                       checkedSuite = ref true;
                                       checkedSolution = ref false;
                                       Js._true);;


let _ = solution##.onclick := handler
	                     (fun _ -> checkedSpec = ref false;
                                       checkedSuite = ref false;
                                       checkedSolution = ref true;
                                       Js._true);;

(*
    Learnocaml_local_storage.(store (editor_state name))
      { Learnocaml_test_state.name ; extra_alea ; output ; input ; ty ; type_question ; mtime = gettimeofday () }
      ;;*)






let _ = save##.onclick := handler (fun _ ->





 let extra_alea = int_of_string "" ;
 match Learnocaml_local_storage.(retrieve (editor_state id) ) with
   {id;titre;prepare;diff;solution;question;template;test;prelude;mtime}->
     let testhaut = {id;ty;type_question;input;output;extra_alea}
     let mtime=gettimeofday () in
     let test ={testml=test.testml;testhaut} in
     let nvexo= {id;titre;prepare;diff;solution;question;template;test;prelude;mtime} in
     Learnocaml_local_storage.(store (editor_state id)) nvexo
; Js._true);;

                                  
