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
