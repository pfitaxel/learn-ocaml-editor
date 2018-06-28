open Learnocaml_exercise_state

let get_titre id = Learnocaml_local_storage.(retrieve (editor_state id)).titre

let get_diff id = Learnocaml_local_storage.(retrieve (editor_state id)).diff
let get_solution id = Learnocaml_local_storage.(retrieve (editor_state id)).solution
let get_question id = Learnocaml_local_storage.(retrieve (editor_state id)).question
let get_template id = Learnocaml_local_storage.(retrieve (editor_state id)).template
let get_testml id = Learnocaml_local_storage.(retrieve (editor_state id)).test.testml
let get_testhaut id = Learnocaml_local_storage.(retrieve (editor_state id)).test.testhaut                      
let get_prelude id = Learnocaml_local_storage.(retrieve (editor_state id)).prelude
let get_prepare id = Learnocaml_local_storage.(retrieve (editor_state id)).prepare
                       
module StringMap=Map.Make(String)
    
let ajout_question testhaut question id =StringMap.add id question testhaut;; 

open Learnocaml_common

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

let testhaut_iframe = Dom_html.createIframe Dom_html.document ;;
let iframe=Tyxml_js.Of_dom.of_iFrame testhaut_iframe ;;

let close =ref ()
