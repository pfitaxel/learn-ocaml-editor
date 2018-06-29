open Learnocaml_exercise_state
module StringMap=Map.Make(String)
open Learnocaml_common
              
let get_titre id = Learnocaml_local_storage.(retrieve (editor_state id)).titre

let get_diff id = Learnocaml_local_storage.(retrieve (editor_state id)).diff
let get_solution id = Learnocaml_local_storage.(retrieve (editor_state id)).solution
let get_question id = Learnocaml_local_storage.(retrieve (editor_state id)).question
let get_template id = Learnocaml_local_storage.(retrieve (editor_state id)).template
let get_testml id = Learnocaml_local_storage.(retrieve (editor_state id)).test.testml
let get_testhaut id = Learnocaml_local_storage.(retrieve (editor_state id)).test.testhaut
let get_prelude id = Learnocaml_local_storage.(retrieve (editor_state id)).prelude
let get_prepare id = Learnocaml_local_storage.(retrieve (editor_state id)).prepare
                       
let get_test_liste id = Learnocaml_local_storage.(retrieve (editor_state id)).test.testhaut
let get_test_string id  = Learnocaml_local_storage.(retrieve (editor_state id)).test.testml                             

let get_ty id idQuestion= let test_list = get_test_liste id in StringMap.(find idQuestion test_list).ty
let get_name_question id idQuestion= let test_list = get_test_liste id in StringMap.(find idQuestion test_list).name                                                                       
let get_type_question id idQuestion= let test_list = get_test_liste id in StringMap.(find idQuestion test_list).type_question
let get_extra_alea id idQuestion= let test_list = get_test_liste id in StringMap.(find idQuestion test_list).extra_alea
let get_input id idQuestion= let test_list = get_test_liste id in StringMap.(find idQuestion test_list).input
let get_output id idQuestion= let test_list = get_test_liste id in StringMap.(find idQuestion test_list).output
    

let get_buffer id = StringMap.find "0" (get_testhaut id)

let ajout_question testhaut question id =StringMap.add id question testhaut;; 



let save_testhaut testhaut id =
  match Learnocaml_local_storage.(retrieve (editor_state id) ) with
    {id;titre;prepare;diff;solution;question;template;test;prelude;mtime}->
      let mtime=gettimeofday () in
      let test ={testml=test.testml;testhaut} in
      let nvexo= {id;titre;prepare;diff;solution;question;template;test;prelude;mtime} in
      
  Learnocaml_local_storage.(store (editor_state id)) nvexo ;;


let fetch_test_index id=
    let index= get_testhaut id   
  in
  let open Learnocaml_exercise_state in
  let open Learnocaml_index in
  let json =
    Json_repr_browser.Json_encoding.construct
     testhaut_enc index
    in
  try Lwt.return (Json_repr_browser.Json_encoding.destruct testhaut_enc json) with exn ->
    Lwt.fail (failwith "" )





(* ---------- Fonctions pour generer le test ---------- *)

let rec listFst liste= match liste with
  |[]->[]
  |a::b -> (fst a)::(listFst b)

                      
let rec redondanceAux liste elem= match liste with
  |[]->[]
  |e::s -> if (e=elem) then (redondanceAux s elem) else (e::(redondanceAux s elem)) 


let rec redondance liste = match liste with
  |[]->[]
  |e::s -> e :: (redondance (redondanceAux s e))


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

let test_fun ty =
  let nb = (nbArgs (decomposition ty 0)) in
  if nb<=4
  then "test_function_"^(string_of_int nb)^"_against_solution"
  else "test_function_against";;

let testAlea nombreTestAlea = " ~gen:"^(string_of_int nombreTestAlea) ;;

let typeFct ty name = "[%ty : "^ty^" ] \""^name^"\"" ;;

let librairie = "open Test_lib ;;\n open Report ;;\n" ;;

let init = "let () =
            set_result @@
            ast_sanity_check code_ast @@ fun () ->\n" ;;


let rec suppr_id_0 listKey = match listKey with
  |[]->[]
  |k::suite -> if k="0" then (suppr_id_0 suite) else k::(suppr_id_0 suite)


let get_id_question id = let test_list = get_test_liste id  in let all_id = StringMap.bindings test_list in suppr_id_0 (redondance (listFst all_id))
                                                                                                                       

let rec constructListeQuest listKey id = match listKey with
  |[]->[]
  |key::suite -> ((get_name_question id key),(get_ty id key),(get_extra_alea id key),
                  (get_input id key),false)::(constructListeQuest suite id)


let sectionSol fct= match fct with
  |(name,typeF,nbAlea,jdt,b)->"Section
      		               ([ Text \"Function:\" ; Code \""^name^"\" ],\n"
      			      ^(test_fun typeF)^(testAlea nbAlea)^"\n"
      			      ^(typeFct typeF name)^"\n["
      			      ^jdt^"] )"
  |_->failwith "error" ;;

let rec constructSectionSol listeFonction = match listeFonction with
  |[]->"]"
  |fct::suite->if ((constructSectionSol suite)<>"]") 
	       then ((sectionSol fct)^" ;\n"^(constructSectionSol suite)) 
	       else ((sectionSol fct)^(constructSectionSol suite)) ;;

let constructFinalSol listeFonction = 
  librairie^init^"["^(constructSectionSol listeFonction)^";;"


(*_________________________Fonctions pour le bouton Generate______________________________________*)

let string_of_char ch = String.make 1 ch ;;

let rec concatenation listech = match listech with
  |[]->""
  |c::l -> (string_of_char c)^(concatenation l);;


let rec supprRec listeChar = match listeChar with
  |[]->[]
  |'='::l->['=']
  |' '::'r'::'e'::'c'::' '::l->' '::supprRec l
  |'\n'::'r'::'e'::'c'::' '::l->' '::supprRec l
  |c::s -> c::supprRec s ;;

let rec trouver_egal listeChar = match listeChar with
  |[]->[]
  |'='::l -> ['=']
  |ch::suite -> ch :: (trouver_egal suite) ;;

let rec trouver_nom listeChar nom = match listeChar with
  |[]->nom
  |' '::suite-> trouver_nom suite nom
  |ch::' '::suite -> if (ch<>' ') then nom@[ch] else trouver_nom suite nom
  |ch::suite -> trouver_nom suite (nom@[ch]) ;;


let rec get_reste listeChar = match listeChar with
  |[]-> []
  |' '::suite -> get_reste suite
  |ch::' '::suite ->  if (ch=' ') then get_reste suite else ' '::suite
  |ch::suite -> get_reste suite ;;

let rec suppr_let listeChar = match listeChar with
  |[]->[]
  |' '::'l'::'e'::'t'::' '::suite -> suite
  |'\n'::'l'::'e'::'t'::' '::suite -> suite
  |ch::suite -> suppr_let suite ;;

let rec get_let listeChar = match listeChar with
  |[]->[]
  |' '::'l'::'e'::'t'::' '::suite -> trouver_egal suite
  |'\n'::'l'::'e'::'t'::' '::suite -> trouver_egal suite
  |ch::suite -> get_let suite ;;


let rec get_args listeChar nbArgs = match listeChar with
  |[]->nbArgs
  |'='::suite -> nbArgs
  |' '::suite -> get_args suite nbArgs
  |ch::' '::suite -> get_args suite (nbArgs+1)
  |ch::'='::suite -> nbArgs+1
  |ch::suite -> get_args suite nbArgs ;;

let rec get_fct listeChar listeRes = match listeChar with
  |[] -> listeRes
  |_ -> if (get_let listeChar)<>[] then (get_fct (suppr_let listeChar) (listeRes@[get_let listeChar]))
        else listeRes;;

let rec genQuestions listeDeListeChar res = match listeDeListeChar with
  |[]->res
  |l::suite -> genQuestions suite (res@[concatenation (trouver_nom l []),(get_args (get_reste l) 0)]) ;;

let rec gen_ty nbArgs =  match nbArgs with
  |0 -> "..."
  |_ -> "... -> "^(gen_ty (nbArgs-1)) ;;

(*_________________________Fonctions pour generer le template_____________________________________*)                             

let failchar = [' ';'f';'a';'i';'l';'w';'i';'t';'h';' ';'"';'T';'O';'D';'O';'"';'\n'] ;;

let tail l = match l with
  |[]->[]
  |e::l->l ;;

let rec decompositionSol str n = 
  if (n+1= String.length str) then [(str.[n])]
  else ( (str.[n])::(decompositionSol str (n+1)) );;

let rec commentaire listech cpt = match listech with
  |[]->[]
  |'*'::')'::l -> if cpt = 0 then l else commentaire l (cpt-1)
  |'('::'*'::l -> commentaire l (cpt+1) 
  |c::l->commentaire l cpt;;

let rec premierLet listech = match listech with 
  |[]->[]
  |'('::'*'::l -> premierLet (commentaire l 0)
  |c::'l'::'e'::'t'::' '::l -> if (c='\n'||c=' ') then ('l'::'e'::'t'::' '::l) else premierLet l
  |'l'::'e'::'t'::' '::l -> 'l'::'e'::'t'::' '::l 
  |' '::l-> premierLet l
  |'\n'::l-> premierLet l
  |_->[];;

let rec validationLet listech = match listech with
  |[]->false
  |' '::l->validationLet l
  |'\n'::l->validationLet l
  |'('::l->validationLet l
  |'l'::'e'::'t'::l->false
  |_-> true
;;

let rec rechercheEgal listech = match listech with
  |[]->0
  |'='::l->1
  |' '::'l'::'e'::'t'::' '::l->2
  |'\n'::'l'::'e'::'t'::' '::l->2
  |c::l->rechercheEgal l ;;

let rec rechercheLet listech b = match listech with
  |[] -> []
  |'('::'*'::l -> rechercheLet (commentaire l 0) b
  |';'::';'::l -> rechercheLet l true
  |'='::l -> rechercheLet l (validationLet l)
  |_::'t'::'h'::'e'::'n'::_::l -> rechercheLet l (validationLet l)
  |_::'e'::'l'::'s'::'e'::_::l -> rechercheLet l (validationLet l)
  |_::'i'::'n'::_::l -> rechercheLet l (validationLet l)
  |'-'::'>'::l->rechercheLet l (validationLet l)
  |'l'::'e'::'t'::' '::l ->
    if b && ((rechercheEgal l)=1) then 'l'::'e'::'t'::' '::l
    else (if ((rechercheEgal l)=0) then rechercheLet l false else rechercheLet l true)
  |c::suite -> rechercheLet suite b
;;

let rec decomposition2 listech = match listech with
  |[] -> []
  |'='::l -> ['=']
  |c::l-> c :: (decomposition2 l) ;;

let decompoFirst listech = match listech with
  |[]-> []
  |_->(decomposition2 listech)@failchar ;;

let rec genLet listech =
  let liste = rechercheLet listech true in
  match liste with
  |[]->[]
  |_-> (decomposition2 liste)@failchar@(genLet (tail liste)) ;;

let rec genTemplate chaine = if chaine="" then "" else
	                       concatenation (genLet (decompositionSol chaine 0));;
