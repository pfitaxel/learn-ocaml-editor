open Learnocaml_exercise_state
module StringMap=Map.Make(String)
open Learnocaml_common

let set_lang () =
	match Js.Optdef.to_option (Dom_html.window##.navigator##.language) with
	| Some l -> Ocplib_i18n.set_lang (Js.to_string l)
	| None ->
		match Js.Optdef.to_option (Dom_html.window##.navigator##.userLanguage) with
		| Some l -> Ocplib_i18n.set_lang (Js.to_string l)
		| None -> ()

let get_titre id = Learnocaml_local_storage.(retrieve (editor_state id)).metadata.titre

let get_description id = Learnocaml_local_storage.(retrieve (editor_state id)).metadata.description

let get_diff id = Learnocaml_local_storage.(retrieve (editor_state id)).metadata.diff
let get_solution id = Learnocaml_local_storage.(retrieve (editor_state id)).solution
let get_question id = Learnocaml_local_storage.(retrieve (editor_state id)).question
let get_template id = Learnocaml_local_storage.(retrieve (editor_state id)).template
let get_testml id = Learnocaml_local_storage.(retrieve (editor_state id)).test.testml
let get_testhaut id = Learnocaml_local_storage.(retrieve (editor_state id)).test.testhaut
let get_prelude id = Learnocaml_local_storage.(retrieve (editor_state id)).prelude
let get_prepare id = Learnocaml_local_storage.(retrieve (editor_state id)).prepare
                       
let get_test_liste id = Learnocaml_local_storage.(retrieve (editor_state id)).test.testhaut
let get_test_string id  = Learnocaml_local_storage.(retrieve (editor_state id)).test.testml                             


let get_ty id idQuestion= let test_list = get_test_liste id in
  match StringMap.(find idQuestion test_list) with
    TestAgainstSol a ->a.ty
  | TestAgainstSpec a -> a.ty
  |TestSuite a -> a.ty
                    
let get_name_question id idQuestion= let test_list = get_test_liste id in
  match StringMap.(find idQuestion test_list) with
    TestAgainstSol a ->a.name
  | TestAgainstSpec a -> a.name
  |TestSuite a -> a.name
                    
let get_type_question id idQuestion=
   let test_list = get_test_liste id in
  match StringMap.(find idQuestion test_list) with
    TestAgainstSol _ ->Solution
  | TestAgainstSpec _ -> Spec
  |TestSuite _ -> Suite 

let get_extra_alea id idQuestion=  let test_list = get_test_liste id in
  match StringMap.(find idQuestion test_list) with
    TestAgainstSol a ->a.gen
  | TestAgainstSpec a -> a.gen
  |_ -> failwith " ?"
                    
let get_input id idQuestion=
  let test_list = get_test_liste id in
  match StringMap.(find idQuestion test_list) with
    TestAgainstSol a ->a.suite
  | TestAgainstSpec a -> a.suite
  |TestSuite a -> a.suite
                    
                                                                    
let get_spec id idQuestion=  let test_list = get_test_liste id in
  match StringMap.(find idQuestion test_list) with  
   TestAgainstSpec a -> a.spec
  |_ -> failwith ""
                    

    

let get_buffer id =  Learnocaml_local_storage.(retrieve (editor_state id)).incipit


let ajout_question testhaut question id =StringMap.add id question testhaut;; 

let compute_question_id test_haut =
  let key_list =List.map (fun (a,b)->int_of_string a) (StringMap.bindings test_haut) in
  let mi coulvois =
    let rec aux c n=match c with
        []->n
      |x::l->if x<>n then aux l n else aux coulvois (n+1)
    in aux coulvois 1
  in string_of_int (mi key_list);;

let save_testhaut testhaut id =
  match Learnocaml_local_storage.(retrieve (editor_state id) ) with
    {metadata;incipit;prepare;solution;question;template;test;prelude;mtime}->
      let mtime=gettimeofday () in
      let test ={testml=test.testml;testhaut} in
      let nvexo= {metadata;incipit;prepare;solution;question;template;test;prelude;mtime} in
      
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
let testhaut_iframe = Dom_html.createIframe Dom_html.document ;;
let iframe_tyxml=Tyxml_js.Of_dom.of_iFrame testhaut_iframe ;;
open Lwt.Infix
open Js_utils
let find_div id =
  match Manip.by_id id with
    Some div -> div
  |None ->let window=Dom_html.window in
      let window=window##.parent in
      let document=window##.document in
      Tyxml_js.Of_dom.of_element (Js.Opt.case (document##getElementById (Js.string id))
       (fun ()-> raise Not_found)
         (fun node->node) )
;;
let hide_load id =
  let elt_lml=match find_div id with
    exception Not_found ->
      let div = Tyxml_js.Html.(div ~a:[ a_id id ]) [] in
      let window=Dom_html.window in
      let window=window##.parent in
      let document=window##.document in
      Manip.(appendChild (Tyxml_js.Of_dom.of_body document##.body) ) div;
      div
    | div ->div
  in
  Manip.(removeClass elt_lml "initial") ;
  Manip.(removeClass elt_lml "loading") ;
  Manip.(addClass elt_lml "loaded") ;;

let show_load id contents =
  let elt =  match find_div id with
    exception Not_found ->
      let div = Tyxml_js.Html.(div ~a:[ a_id id ]) [] in
      let window=Dom_html.window in
      let window=window##.parent in
      let document=window##.document in
      Manip.(appendChild (Tyxml_js.Of_dom.of_body document##.body) ) div;
      div
    | div ->div
  in
  Manip.(addClass elt "loading-layer") ;
  Manip.(removeClass elt "loaded") ;
  Manip.(addClass elt "loading") ;
  let chamo_src =
    "icons/tryocaml_loading_" ^ string_of_int (Random.int 8 + 1) ^ ".gif" in
  Manip.replaceChildren elt
    Tyxml_js.Html.[
      div ~a: [ a_id "chamo" ] [ img ~alt: "loading" ~src: chamo_src () ] ;
      div ~a: [ a_class [ "messages" ] ] contents
    ]
;;
let _=testhaut_iframe##.width :=Js.string "100%";;
let _=testhaut_iframe##.height:=Js.string "100%";;  
let _= Manip.SetCss.opacity iframe_tyxml (Some "1");;

let  rec testhaut_init content_div id =          
    fetch_test_index id >>= fun index ->  
  let format_question_list all_question_states =
    let  format_contents acc contents =
      let open Tyxml_js.Html5 in
            
          StringMap.fold 
            (fun question_id quest acc ->  
               let name,ty=match quest with
                  TestAgainstSol a ->a.name,a.ty
                 |TestAgainstSpec a -> a.name,a.ty
                 |TestSuite a ->a.name,a.ty
               in
                 div ~a:[a_id ("toolbar"); a_class ["button"]] [
              (div ~a:[a_id ("button_delete")] [
                  let button =button ~a:[a_id question_id]  [img ~src:("icons/icon_cleanup_dark.svg") ~alt:"" () ; pcdata "" ]in 
                  Manip.Ev.onclick button
                    (fun _ ->
                       begin
                         let messages = Tyxml_js.Html5.ul [] in
                         let aborted, abort_message =
                           let t, u = Lwt.task () in
                           let btn_no = Tyxml_js.Html5.(button [ pcdata [%i"No"] ]) in
                           Manip.Ev.onclick btn_no ( fun _ ->
                               hide_load "learnocaml-main-loading" ;
                               true) ;
                           let btn_yes = Tyxml_js.Html5.(button [ pcdata [%i"Yes"] ]) in
                           Manip.Ev.onclick btn_yes (fun _ ->
                               let rmv= get_testhaut id in                            
                               let testhaut = StringMap.remove question_id rmv in
                               save_testhaut testhaut id ;
                               hide_load "learnocaml-main-loading";
                               Manip.removeChildren content_div;
                               let _ = testhaut_init content_div id in ()  ; true) ;
                           let div =
                             Tyxml_js.Html5.(div ~a: [ a_class [ "dialog" ] ]
                                               [ pcdata [%i"Are you sure you want to delete this question?\n"] ;
                                                 btn_yes ;
                                                 pcdata " " ;
                                                 btn_no ]) in
                           Manip.SetCss.opacity div (Some "0") ;
                           t, div in 
                         Manip.replaceChildren messages
                           Tyxml_js.Html5.[ li [ pcdata "" ] ] ;
                         show_load "learnocaml-main-loading" [ abort_message ] ;
                         Manip.SetCss.opacity abort_message (Some "1") ;
                       end ;
                       true) ;button
              ] );
                    (div ~a:[a_id ("up")] [
                     let buttonUp = button ~a:[a_id question_id]  [img ~src:("icons/icon_down_dark.svg") ~alt:"" () ; pcdata "" ] in 
                     Manip.Ev.onclick buttonUp
                       (fun _ ->
                         begin
                           let qid = question_id in
                           let testhaut = get_testhaut id in
                            let question = StringMap.find qid testhaut in
                            let suivant = string_of_int ((int_of_string qid) +1) in
                            let testhaut  = match StringMap.find suivant testhaut with
                              | exception Not_found -> testhaut
                              | qsuivante -> let map = StringMap.add qid qsuivante testhaut in
                                             StringMap.add suivant question map in
                            save_testhaut testhaut id;
                            Manip.removeChildren content_div;
                            let _ = testhaut_init content_div id in ()
                          end;
                         true) ;
                     buttonUp;
            ]);
                  (div ~a:[a_id ("down")] [
                     let buttonDown =button ~a:[a_id question_id]  [img ~src:("icons/icon_up_dark.svg") ~alt:"" () ; pcdata "" ] in 
                     Manip.Ev.onclick buttonDown
                       (fun _ ->
                         begin
                           let qid = question_id in
                           let testhaut = get_testhaut id in
                           let question = StringMap.find qid testhaut in
                           let intp = (int_of_string qid) -1 in
                            let prec = string_of_int (if intp=0 then intp+1 else intp ) in
                            let testhaut  = match StringMap.find prec testhaut with
                              | exception Not_found -> testhaut
                              | qprec -> let map = StringMap.add qid qprec testhaut in
                                             StringMap.add prec question map in
                            save_testhaut testhaut id;
                            Manip.removeChildren content_div;
                            let _ = testhaut_init content_div id in ()
                          end;
                         true) ;
                     buttonDown;
                  ]);
                  (div ~a:[a_id ("duplicate")] [
                       let buttonDuplicate =button ~a:[a_id question_id] [img ~src:("icons/icon_list_dark.svg") ~alt:"" (); pcdata "" ] in
                       Manip.Ev.onclick buttonDuplicate
                         (fun _ ->
                           begin
                             let testhaut = get_testhaut id in
                             let question = StringMap.find question_id testhaut in
                             let qid = compute_question_id testhaut in
                             let testhaut = StringMap.add qid question testhaut in
                             save_testhaut testhaut id;
                             Manip.removeChildren content_div;
                             let _ = testhaut_init content_div id in ()
                           end; true);
                       buttonDuplicate;
                  ]) ]  
                 ::  a ~a:[ a_onclick (fun _ ->
                  
                  let elt = find_div "learnocaml-exo-loading" in
                  Manip.(addClass elt "loading-layer") ;
                  Manip.(removeClass elt "loaded") ;
                  Manip.(addClass elt "loading") ;
                  Manip.replaceChildren elt [iframe_tyxml]  ;
            testhaut_iframe##.src:=Js.string ("test.html#id="^id^"&questionid="^question_id^"&action=open");       
            true ) ; 
                     a_class [ "exercise" ] ] [
                div ~a:[ a_class [ "descr" ] ] [
                  h1 [ pcdata name ] ;
                  p [   pcdata ty ] ;
                ]          
              ] ::
              acc)
            contents acc
    in
    List.rev (format_contents (* ([Tyxml_js.Html5.a ~a:[ Tyxml_js.Html5.a_class ["patterns"]] [
    	Tyxml_js.Html.h1 [ Tyxml_js.Html5.pcdata [%i"Code quality and forbidden patterns"] ];
        Tyxml_js.Html5.div ~a:[ Tyxml_js.Html5.a_class [ "quality" ] ] [
            Tyxml_js.Html5.p [ Tyxml_js.Html5.pcdata [%i"Forbid undesirable code patterns"] ];
            let quality_box = Tyxml_js.Html5.input ~a:[ Tyxml_js.Html5.a_input_type `Checkbox ] () in
            Manip.Ev.onchange quality_box (fun _ -> let _ = ""; true); ];
        Tyxml_js.Html5.div ~a:[ Tyxml_js.Html5.a_class [ "imperative" ] ] [
            Tyxml_js.Html5.p [ Tyxml_js.Html5.pcdata [%i"Forbid imperative features"] ];
            let imperative_box = Tyxml_js.Html5.input ~a:[ Tyxml_js.Html5.a_input_type `Checkbox ] () in
            Manip.Ev.onchange imperative_box (fun _ -> let _ = ""; true); ] ]) @ *)
       ([Tyxml_js.Html5.a ~a:[ Tyxml_js.Html5.a_onclick
       (fun _ ->
         let elt = find_div "learnocaml-exo-loading" in
         Manip.(addClass elt "loading-layer") ;
         Manip.(removeClass elt "loaded") ;
         Manip.(addClass elt "loading") ;
         Manip.replaceChildren elt [iframe_tyxml]  ;
         testhaut_iframe##.src:=Js.string ("test.html#id="^id^"&action=open");       
         true); 
      Tyxml_js.Html5.a_class [ "exercise" ] ] [
         Tyxml_js.Html5.div ~a:[ Tyxml_js.Html5.a_class [ "descr" ] ] [
             Tyxml_js.Html.h1 [ Tyxml_js.Html5.pcdata [%i"New question"] ];
             Tyxml_js.Html5.p [Tyxml_js.Html5.pcdata [%i"Create a new question"]];
           ];
       ]]) index) in 
  let list_div =
   Tyxml_js.Html5.(div ~a: [Tyxml_js.Html5.a_id "learnocaml-main-exercise-list" ])
      (format_question_list index) in
  Dom.appendChild (Tyxml_js.To_dom.of_div content_div) (Tyxml_js.To_dom.of_div list_div ) ;
  
  Lwt.return_unit;;

   





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


let sectionSol fct = match fct with
  | (name,typeF,nbAlea,jdt,b)->"Section
      		               ([ Text \"Function:\" ; Code \""^name^"\" ],\n"
      			      ^(test_fun typeF)^(testAlea nbAlea)^"\n"
      			      ^(typeFct typeF name)^"\n"
      			      ^jdt^" )"
let section name report= "Section
      		               ([ Text \"Function:\" ; Code \""^name^"\" ], "^report^" ); \n" ;;

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

let _ = set_lang ()
