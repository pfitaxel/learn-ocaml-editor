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
            (fun question_id {name;
                               ty ;
                               type_question ;
                               input;
                               output;
                               extra_alea
                                } acc ->  
              
              (div ~a:[a_id ("button_delete")] [
                  let button =button ~a:[a_id question_id]  [img ~src:("icons/icon_cleanup_dark.svg") ~alt:"" () ; pcdata "" ]in 
                  Manip.Ev.onclick button
                    (fun _ ->
                       begin
                         let messages = Tyxml_js.Html5.ul [] in
                         let aborted, abort_message =
                           let t, u = Lwt.task () in
                           let btn_no = Tyxml_js.Html5.(button [ pcdata "No" ]) in
                           Manip.Ev.onclick btn_no ( fun _ ->
                               hide_load "learnocaml-main-loading" ;
                               true) ;
                           let btn_yes = Tyxml_js.Html5.(button [ pcdata "Yes" ]) in
                           Manip.Ev.onclick btn_yes (fun _ ->
                               let rmv= get_testhaut id in                            
                               let testhaut = StringMap.remove question_id rmv in
                               save_testhaut testhaut id ;
                               hide_load "learnocaml-main-loading";
                               Manip.removeChildren content_div;
                               let _ = testhaut_init content_div id in ()  ; true) ;
                           let div =
                             Tyxml_js.Html5.(div ~a: [ a_class [ "dialog" ] ]
                                               [ pcdata "Are you sure you want to delete this question ?\n" ;
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
                ] ) ::
              a ~a:[ a_onclick (fun _ ->
                  
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
  
  
     
    List.rev (format_contents  [Tyxml_js.Html5.a ~a:[ Tyxml_js.Html5.a_onclick 
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
        Tyxml_js.Html.h1 [ Tyxml_js.Html5.pcdata "New question" ];
        Tyxml_js.Html5.p [Tyxml_js.Html5.pcdata "Create a new question"];];
      ]] index) in 
  let list_div =
   Tyxml_js.Html5.(div ~a: [Tyxml_js.Html5.a_id "learnocaml-main-exercise-list" ])
      (format_question_list index) in
  Dom.appendChild (Tyxml_js.To_dom.of_div content_div) (Tyxml_js.To_dom.of_div list_div ) ;
  
  Lwt.return_unit;;

   
