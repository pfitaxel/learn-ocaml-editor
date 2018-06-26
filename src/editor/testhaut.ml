(*open Js_of_ocaml
open Str
open Js_of_ocaml
open Dom_html
open Learnocaml_common
       
let toString = function
  |None -> failwith "incorrect_input"
  |Some input -> Js.to_string input##.value ;;


let checkedSpec = ref false ;;
let checkedSolution = ref false ;;
let checkedSuite = ref false ;;

let nameInput = getElementById_coerce "name" CoerceTo.input ;;
let typeInput = getElementById_coerce "ty" CoerceTo.input ;;
let spec = getElementById "spec" ;;
let solution = getElementById "solution" ;;
let suite = getElementById "suite" ;;

let suiteInput = getElementById "elementSuiteEnEntree";;
let suiteOutput = getElementById "elementSuiteEnSortie";;

let solutionInput = getElementById "elementSolEnEntree";;
let aleaSol = getElementById "testSolAlea";;

let specOutput = getElementById "elementSpecEnSortie";;
let aleaSpec = getElementById "testSpecAlea";;

let name = toString nameInput ;;
let typeFct = toString typeInput ;;

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


    Learnocaml_local_storage.(store (test_state name))
      { Learnocaml_test_state.name ;extra_alea ; output ; input ; ty ; type_question ; mtime = gettimeofday () }
;;

(*___________________________pour testAgainstSol______________________________________*)


let rec decomposition str n = 
if (n+1= String.length str) then [(str.[n])]
else ( (str.[n])::(decomposition str (n+1)) );;

let rec rechercheParenthese listeChar n =
	if n=0 
	then listeChar 
	else 
		match listeChar with
		|[]-> failwith "error type"
		|'('::l ->(rechercheParenthese l (n+1))
		|')'::l->(rechercheParenthese l (n-1))
		|ch::l->rechercheParenthese l n ;;


let rec nbArgs listeChar = match listeChar with
|[] -> 0
|'-'::'>'::suite -> 1+(nbArgs suite)
|ch::s -> if (ch = '(') then (nbArgs (rechercheParenthese listeChar 1)) else (nbArgs s) ;;

let test_fun ty = "test_function_"^(string_of_int (nbArgs (decomposition ty 0)))^"_against_solution" ;;

let testAlea nombreTestAlea = " ~gen:"^(string_of_int nombreTestAlea) ;;

let typeFct ty name = "[%ty : "^ty^" ] \""^name^"\"" ;;

let librairie = "open Test_lib ;;\n open Report ;;\n" ;;

let init = "let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->\n" ;;

(*le variable b est un booléen qui pourra peut être servir pour les tests polymorphe*)


let sectionSol fct= match fct with
|(name,typeF,nbAlea,jdt,b)->"Section
      		        	([ Text \"Function:\" ; Code \""^name^"\" ],\n"
      				^(test_fun typeF)^(testAlea nbAlea)^"\n"
      				^(typeFct typeF name)^"\n"
      				^jdt^" )"
|_->failwith "error" ;;

let rec constructSectionSol listeFonction = match listeFonction with
|[]->"]"
|fct::suite->if ((constructSectionSol suite)<>"]") 
			then ((sectionSol fct)^" ;\n"^(constructSectionSol suite)) 
			else ((sectionSol fct)^(constructSectionSol suite)) ;;

let constructFinalSol listeFonction = 
  librairie^init^"["^(constructSectionSol listeFonction)^";;";;

let listeFct = 2;;


(*let listeFct = [(nom1,type1,nbAlea1,jdt1,b1);(nom2,type2,nbAlea2,jdt2,b2);(nom3,type3,nbAlea3,jdt3,b3);(nom4,type4,nbAlea4,jdt4,b4)]*)


(*____________________________________________________________________________________*)

(*Fonctions pour compiler*)

let compiler = getElementById "BoutonCompiler";;


;;
(*il faut recuperer la liste des questions dans le local storage et pour chaque questions : *)

let get_questions id  = Learnocaml_local_storage.(retrieve (editor_state id)).test;;

let getListeQuestions quest
match quest with
|String a -> failwith ""
|Index -> 


(*
let get_tyFct nomQuestion = Learnocaml_local_storage.(retrieve (editor_state nomQuestion)).tyFct;;
let get_nbAlea nomQuestion = Learnocaml_local_storage.(retrieve (editor_state nomQuestion)).aleatoire;;
let get_jdtEntree nomQuestion = Learnocaml_local_storage.(retrieve (editor_state nomQuestion)).jdtEntree;;
let get_jdtSortie nomQuestion = Learnocaml_local_storage.(retrieve (editor_state nomQuestion)).jdtSortie;;
*)

let rec constructJdt listeQuestion = match listeQuestion with
  |[]->[]
  |nameQuestion::tail -> (nameQuestion,(get_tyFct nameQuestion),(get_nbAlea nameQuestion),(get_jdtEntree nameQuestion),false)::(constructJdt tail) ;;

let listeQuestion = [] (*TODO*) ;;

let _ = compiler##.onclick := handler (fun _ -> let listeFonction = constructJdt listeQuestion in let tests = constructFinalSol listeFonction in ; Js._true);;
*)
