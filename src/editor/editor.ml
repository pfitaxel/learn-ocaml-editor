(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2016 OCamlPro.
 *
 * Learn-OCaml is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your op1tion) any later version.
 *
 * Learn-OCaml is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>. *)

open Js_utils
open Lwt.Infix
open Learnocaml_common
open Learnocaml_exercise_state
open Js_of_ocaml
open Editor_lib
open Dom_html
(*
module Report = Learnocaml_report

let test_lib ?callback ?(timeout: int option)
 (module Introspection : Introspection_intf.INTROSPECTION) =
 let set_progress =
   match callback with
   | None -> (fun _ -> ())
   | Some set_progress -> set_progress in

 let results : Learnocaml_report.report option ref = ref None in

 let module M (* : Params *) = struct
   let results = results
   let set_progress = set_progress
   let timeout = timeout
   module Introspection = Introspection
 end in
 let module TL = Test_lib.Make(M) in
(module TL : Test_lib.S)
*)

(*
module Dummy_Functor (Introspection :
                        Introspection_intf.INTROSPECTION) = struct
  module Dummy_Params = struct
    let results = ref None
    let set_progress _ = ()
    module Introspection = Introspection            
  end       
  module Test_lib = Test_lib.Make(Dummy_Params)
  module Report = Learnocaml_report
*)
let auto_save_interval = 120.0 ;; (* in seconds*)

module StringMap = Map.Make (String)
                         
let recovering_callback = ref (fun ()->())


let id=arg "id"



(*_________________________Fonctions pour le bouton Generate______________________________________*)

let compute_question_id test_haut =
  let key_list =List.map (fun (a,b)->int_of_string a) (StringMap.bindings test_haut) in
  let mi coulvois =
    let rec aux c n=match c with
        []->n
       |x::l->if x<>n then aux l n else aux coulvois (n+1)
    in aux coulvois 1
  in string_of_int (mi key_list) ;;

    (*chacun des couples est sauvegardé dans le local storage*)
let rec save_quest listeQuestions id = match listeQuestions with
  |[]->()
  |(nom,nbArgs)::suite ->
    let name = nom in
    let ty= gen_ty nbArgs in
    let type_question = Solution in
    let input = "[]" in
    let output = "[]" in
    let extra_alea = 0 in
    let question = {name ; ty ; type_question ; input ; output ; extra_alea} in
    let testhaut =  get_testhaut id in
    let question_id = compute_question_id testhaut in
    let new_testhaut = StringMap.add question_id question testhaut in
    let () = save_testhaut new_testhaut id in
    save_quest suite id;;
(*-------------------------------------------------------------------------*)
let id = arg "id";; 
let grade_black =ref (fun ()->());;
let grade_red=ref (fun()->());;
let init_tabs, select_tab =
  let names = [ "toplevel" ; "report" ; "editor" ; "template" ; "test" ;
                "question" ; "prelude" ; "prepare" ; "testhaut" ] in
  let current = ref "question" in
  let select_tab name =
    set_arg "tab" name ;
   
    if (name = "testhaut") then
      !grade_red ()

    else
      !grade_black ();

    Manip.removeClass
      (find_component ("learnocaml-exo-button-" ^ !current))
      "front-tab" ;
    Manip.removeClass
      (find_component ("learnocaml-exo-tab-" ^ !current))
      "front-tab" ;
    Manip.enable
      (find_component ("learnocaml-exo-button-" ^ !current)) ;
    Manip.addClass
      (find_component ("learnocaml-exo-button-" ^ name))
      "front-tab" ;
    Manip.addClass
      (find_component ("learnocaml-exo-tab-" ^ name))
      "front-tab" ;
    Manip.disable
      (find_component ("learnocaml-exo-button-" ^ name)) ;
    current := name in
  let init_tabs () =
    current := begin try
        let requested = arg "tab" in
        if List.mem requested names then requested else "question"
      with Not_found -> "question"
    end ;
    List.iter
      (fun name ->
         Manip.removeClass
           (find_component ("learnocaml-exo-button-" ^ name))
           "front-tab" ;
         Manip.removeClass
           (find_component ("learnocaml-exo-tab-" ^ name))
           "front-tab" ;
         Manip.Ev.onclick
           (find_component ("learnocaml-exo-button-" ^ name))
           (fun _ -> select_tab name ; true))
      names ;
    select_tab !current in
  init_tabs, select_tab


let display_report exo report =
  (* let score, failed = Learnocaml_report.result_of_report report in *)
  let report_button = find_component "learnocaml-exo-button-report" in
  Manip.removeClass report_button "success" ;
  Manip.removeClass report_button "failure" ;
  Manip.removeClass report_button "partial" ;
  let grade = 100 in
  if grade >= 100 then begin
    Manip.addClass report_button "success" ;
    Manip.replaceChildren report_button
      Tyxml_js.Html5.[ pcdata [%i"Report"] ]
  end else if grade = 0 then begin
    Manip.addClass report_button "failure" ;
    Manip.replaceChildren report_button
      Tyxml_js.Html5.[ pcdata [%i"Report"] ]
  end else begin
    Manip.addClass report_button "partial" ;
    let pct = Format.asprintf "%2d%%" grade in
    Manip.replaceChildren report_button
      Tyxml_js.Html5.[ pcdata [%i"Report"] ;
                       span ~a: [ a_class [ "score" ] ] [ pcdata pct ]]
  end ;
  let report_container = find_component "learnocaml-exo-tab-report" in
  Manip.setInnerHtml report_container
    (Format.asprintf "%a" Learnocaml_report.(output_html_of_report ~bare: true) report) ;
  grade
    
let set_string_translations () =
  let translations = [
  "txt_preparing", [%i"Preparing the environment"];
  "learnocaml-exo-button-editor", [%i"Solution"];
  "learnocaml-exo-button-template", [%i"Template"];
  "learnocaml-exo-button-prelude", [%i"Prelude"];
  "learnocaml-exo-button-prepare", [%i"Prepare"];
  "learnocaml-exo-button-toplevel", [%i"Toplevel"];
  "learnocaml-exo-button-question", [%i"Question"];
  "learnocaml-exo-button-test", [%i"Test.ml"];
  "learnocaml-exo-button-testhaut", [%i"Test"];
  "learnocaml-exo-button-report", [%i"Report"];
  "learnocaml-exo-editor-pane", [%i"Editor"];
  "txt_grade_report", [%i"Click the Grade! button to test your solution"];
  "learnocaml-exo-test-pane", [%i"Editor"];
  ] in
  List.iter
  (fun (id, text) -> Manip.setInnerHtml (find_component id) text)
  translations

let set_string_translations_titles () =
  let translations = [
  "learnocaml-exo-button-editor", [%i"Type here the solution of the exercise"];
  "learnocaml-exo-button-template", [%i"Type here or generate the template the student will complete or correct"];
  "learnocaml-exo-button-prelude", [%i"Type here the definitions of types and functions given to the student"];
  "learnocaml-exo-button-prepare", [%i"Type here hidden definitions given to the student"];
  "learnocaml-exo-button-question", [%i"Type here the wording of the exercise in Markdown"];
  "learnocaml-exo-button-test", [%i"Type here the tests code"];
  "learnocaml-exo-button-testhaut", [%i"Generate here the tests code"];
  ] in
  List.iter
  (fun (id, text) -> Manip.setTitle (find_component id) text)
  translations

let set_lang () =
	match Js.Optdef.to_option (Dom_html.window##.navigator##.language) with
	| Some l -> Ocplib_i18n.set_lang (Js.to_string l)
	| None ->
		match Js.Optdef.to_option (Dom_html.window##.navigator##.userLanguage) with
		| Some l -> Ocplib_i18n.set_lang (Js.to_string l)
		| None -> ()

let () =
  Lwt.async_exception_hook := begin function
    | Failure message -> fatal message
    | Server_caller.Cannot_fetch message -> fatal message
    | exn -> fatal (Printexc.to_string exn)
  end ;
  Lwt.async @@ fun () ->
  set_lang ();
  set_string_translations ();
  set_string_translations_titles ();
  Learnocaml_local_storage.init () ;
               
  (* ---- launch everything --------------------------------------------- *)
  let toplevel_buttons_group = button_group () in
  disable_button_group toplevel_buttons_group (* enabled after init *) ;
  let toplevel_toolbar = find_component "learnocaml-exo-toplevel-toolbar" in
  let editor_toolbar = find_component "learnocaml-exo-editor-toolbar" in
  let template_toolbar = find_component "learnocaml-exo-template-toolbar" in
  let prelude_toolbar = find_component "learnocaml-exo-prelude-toolbar" in
  let prepare_toolbar = find_component "learnocaml-exo-prepare-toolbar" in
  let test_toolbar = find_component "learnocaml-exo-test-toolbar" in
  let testhaut_toolbar = find_component "learnocaml-exo-testhaut-toolbar" in
  let toplevel_button = button ~container: toplevel_toolbar ~theme: "dark" in
  let editor_button = button ~container: editor_toolbar ~theme: "light" in
  let test_button = button ~container: test_toolbar ~theme: "light" in
  let testhaut_button = button ~container: testhaut_toolbar ~theme: "light" in
  let template_button = button ~container: template_toolbar ~theme: "light" in
  let prelude_button = button ~container: prelude_toolbar ~theme: "light" in
  let prepare_button = button ~container: prepare_toolbar ~theme: "light" in
  let id = arg "id" in

  let after_init top =
    begin 
       Lwt.return true
    end >>= fun r1 ->
    Learnocaml_toplevel.load ~print_outcome:false top
     "" >>= fun r2 ->
    if not r1 || not r2 then failwith [%i"error in prelude"] ;
    Learnocaml_toplevel.set_checking_environment top >>= fun () ->
    Lwt.return () in
  let timeout_prompt =
    Learnocaml_toplevel.make_timeout_popup
      ~on_show: (fun () -> select_tab "toplevel")
      () in
  let flood_prompt =
    Learnocaml_toplevel.make_flood_popup
      ~on_show: (fun () -> select_tab "toplevel")
      () in
  let history =
    let storage_key =
      Learnocaml_local_storage.exercise_toplevel_history id in
    let on_update self =
      Learnocaml_local_storage.store storage_key
        (Learnocaml_toplevel_history.snapshot self) in
    let snapshot =
      Learnocaml_local_storage.retrieve storage_key in
    Learnocaml_toplevel_history.create
      ~gettimeofday
      ~on_update
      ~max_size: 99
      ~snapshot () in

  let toplevel_launch =
    Learnocaml_toplevel.create
      ~after_init ~timeout_prompt ~flood_prompt
      ~on_disable_input: (fun _ -> disable_button_group toplevel_buttons_group)
      ~on_enable_input: (fun _ -> enable_button_group toplevel_buttons_group)
      ~container:(find_component "learnocaml-exo-toplevel-pane")
      ~history () in
  init_tabs () ;
  toplevel_launch >>= fun top ->


  (* ---- toplevel pane ------------------------------------------------- *)
  begin toplevel_button
      ~group: toplevel_buttons_group
      ~icon: "cleanup" [%i"Clear"] @@ fun () ->
    Learnocaml_toplevel.clear top ;
    Lwt.return ()
  end ;
  begin toplevel_button
      ~icon: "reload" [%i"Reset"] @@ fun () ->
    toplevel_launch >>= fun top ->
    disabling_button_group toplevel_buttons_group (fun () -> Learnocaml_toplevel.reset top)
  end ;
  begin toplevel_button
      ~group: toplevel_buttons_group
      ~icon: "run" [%i"Eval phrase"] @@ fun () ->
    Learnocaml_toplevel.execute top ;
    Lwt.return ()
  end ;

  (* ---- testhaut edit ------------------------------------------------*)
  
  let name = "" in
  let ty = "" in
  let type_question = Suite in
  let input = match get_buffer id with
    exception Not_found -> ""
  | buff -> buff.input in
  let output = "" in
  let extra_alea = 0 in
  let question = {name; ty; type_question; input; output; extra_alea} in
  let testhaut = get_testhaut id in
  let question_id ="0" in
  let testhaut = StringMap.add question_id question testhaut in
  save_testhaut testhaut id;
  let editor_testhaut = find_component "learnocaml-exo-testhaut-edit" in
  let editor_th =Ocaml_mode.create_ocaml_editor (Tyxml_js.To_dom.of_div editor_testhaut ) in
  let ace_testhaut = Ocaml_mode.get_editor editor_th in
  let buffer = match get_buffer id with
  | exception Not_found -> ""
  | buff -> if (buff.input="")
            then [%i"(* Incipit: contains local definitions\n\
            		 that will be reachable when you will create\n\
        			 a new question *)\n"]
            else buff.input in
  Ace.set_contents ace_testhaut buffer ;
  Ace.set_font_size ace_testhaut 18;

  (* ---- test pane --------------------------------------------------- *)
  let editor_test = find_component "learnocaml-exo-test-pane" in
  let editor_t = Ocaml_mode.create_ocaml_editor (Tyxml_js.To_dom.of_div editor_test) in
  let ace_t = Ocaml_mode.get_editor editor_t in
  let contents=
    let a = get_testml id in
    if a = "" then
      [%i"(* Grader and tests code *)\n"]
    else
      a
  in

  Ace.set_contents ace_t  (contents); 
  Ace.set_font_size ace_t 18;

 (*let lib = " module Test_lib = Test_lib.Make(struct\n\
        \  let results = results\n\
        \  let set_progress = set_progress\n\
        \  module Introspection = Introspection\n\
              end)" in *)
  
   let typecheck set_class =
    Learnocaml_toplevel.check top (Ace.get_contents ace_t) >>= fun res ->
    let error, warnings =
      match res with
      | Toploop_results.Ok ((), warnings) -> None, warnings
      | Toploop_results.Error (err, warnings) -> Some err, warnings in
    let transl_loc { Toploop_results.loc_start ; loc_end } =
      { Ocaml_mode.loc_start ; loc_end } in
    let error = match error with
      | None -> None
      | Some { Toploop_results.locs ; msg ; if_highlight } ->
          Some { Ocaml_mode.locs = List.map transl_loc locs ;
                 msg = (if if_highlight <> "" then if_highlight else msg) } in
    let warnings =
      List.map
        (fun { Toploop_results.locs ; msg ; if_highlight } ->
           { Ocaml_mode.loc = transl_loc (List.hd locs) ;
             msg = (if if_highlight <> "" then if_highlight else msg) })
        warnings in
    Ocaml_mode.report_error ~set_class editor_t error warnings  >>= fun () ->
    Ace.focus ace_t ;
    Lwt.return () in
  begin test_button
      ~group: toplevel_buttons_group
      ~icon: "typecheck" [%i"Check"] @@ fun () ->
    typecheck true
  end ;


  (* ---- template pane --------------------------------------------------- *)
  let editor_template = find_component "learnocaml-exo-template-pane" in
  let editor_temp = Ocaml_mode.create_ocaml_editor (Tyxml_js.To_dom.of_div editor_template) in
  let ace_temp = Ocaml_mode.get_editor editor_temp in
  let contents=
    let a = get_template id in
    if a = "" then
      [%i"(* Code the student will have\n\
      	  when he will start the exercise *)\n"]
    else
      a
  in
  Ace.set_contents ace_temp contents ;
  Ace.set_font_size ace_temp 18;

  
  let typecheck set_class =
    Learnocaml_toplevel.check top (Ace.get_contents ace_temp) >>= fun res ->
    let error, warnings =
      match res with
      | Toploop_results.Ok ((), warnings) -> None, warnings
      | Toploop_results.Error (err, warnings) -> Some err, warnings in
    let transl_loc { Toploop_results.loc_start ; loc_end } =
      { Ocaml_mode.loc_start ; loc_end } in
    let error = match error with
      | None -> None
      | Some { Toploop_results.locs ; msg ; if_highlight } ->
          Some { Ocaml_mode.locs = List.map transl_loc locs ;
                 msg = (if if_highlight <> "" then if_highlight else msg) } in
    let warnings =
      List.map
        (fun { Toploop_results.locs ; msg ; if_highlight } ->
           { Ocaml_mode.loc = transl_loc (List.hd locs) ;
             msg = (if if_highlight <> "" then if_highlight else msg) })
        warnings in
    Ocaml_mode.report_error ~set_class editor_temp error warnings  >>= fun () ->
    Ace.focus ace_temp ;
    Lwt.return () in
  begin template_button
      ~group: toplevel_buttons_group
      ~icon: "typecheck" [%i"Check"] @@ fun () ->
    typecheck true
  end ;



  (*-------question pane  -------------------------------------------------*)
  let editor_question = find_component "learnocaml-exo-question-mark" in
  let ace_quest = Ace.create_editor (Tyxml_js.To_dom.of_div editor_question ) in
   let question =
    let a = get_question id in
    if a = "" then [%i"# Questions\n\n\
    You can write here your questions using\n\
    the **Markdown** markup language\n"]
    else a
  in
  
  Ace.set_contents ace_quest question ;
  Ace.set_font_size ace_quest 18;

  let question = get_question id
  in
  let question =Omd.to_html (Omd.of_string question) in
 
  let text_container = find_component "learnocaml-exo-question-html" in
  let text_iframe = Dom_html.createIframe Dom_html.document in
  Manip.replaceChildren text_container
    Tyxml_js.Html5.[ Tyxml_js.Of_dom.of_iFrame text_iframe ] ;
  Js.Opt.case
    (text_iframe##.contentDocument)
    (fun () -> failwith "cannot edit iframe document")
    (fun d ->
       let html = Format.asprintf
           "<!DOCTYPE html>\
            <html><head>\
            <title>%s - exercise text</title>\
            <meta charset='UTF-8'>\
            <link rel='stylesheet' href='css/learnocaml_standalone_description.css'>\
            </head>\
            <body>\
            %s\
            </body>\
            </html>"
           (get_titre id)
           question in
       d##open_;
       d##write (Js.string html);
       d##close);

let old_text = ref "" in
  
let onload () =
   let rec dyn_preview =
    let text = Ace.get_contents ace_quest in
      if text <> !old_text then begin
       Js.Opt.case
    (text_iframe##.contentDocument)
    (fun () -> failwith "cannot edit iframe document")
    (fun d ->
       let html = Format.asprintf
           "<!DOCTYPE html>\
            <html><head>\
            <title>%s - exercise text</title>\
            <meta charset='UTF-8'>\
            <link rel='stylesheet' href='css/learnocaml_standalone_description.css'>\
            </head>\
            <body>\
            %s\
            </body>\
            </html>"
           (get_titre id)
           (Omd.to_html (Omd.of_string text)) in
       d##open_;
       d##write (Js.string html);
       d##close);
       old_text := text
        end
   in
   dyn_preview; () in

  (* ---- prelude pane --------------------------------------------------- *)
  let editor_prelude = find_component "learnocaml-exo-prelude-pane" in
  let editor_prel = Ocaml_mode.create_ocaml_editor (Tyxml_js.To_dom.of_div editor_prelude) in
  let ace_prel = Ocaml_mode.get_editor editor_prel in
  let contents=
    let a = get_prelude id in
    if a = "" then
      [%i"(* Local definitions the student\n\
      	  will be able to see *)\n"]
    else
      a
  in
  Ace.set_contents ace_prel contents ;
  Ace.set_font_size ace_prel 18;
 
    let typecheck set_class =
    Learnocaml_toplevel.check top (Ace.get_contents ace_prel) >>= fun res ->
    let error, warnings =
      match res with
      | Toploop_results.Ok ((), warnings) -> None, warnings
      | Toploop_results.Error (err, warnings) -> Some err, warnings in
    let transl_loc { Toploop_results.loc_start ; loc_end } =
      { Ocaml_mode.loc_start ; loc_end } in
    let error = match error with
      | None -> None
      | Some { Toploop_results.locs ; msg ; if_highlight } ->
          Some { Ocaml_mode.locs = List.map transl_loc locs ;
                 msg = (if if_highlight <> "" then if_highlight else msg) } in
    let warnings =
      List.map
        (fun { Toploop_results.locs ; msg ; if_highlight } ->
           { Ocaml_mode.loc = transl_loc (List.hd locs) ;
             msg = (if if_highlight <> "" then if_highlight else msg) })
        warnings in
    Ocaml_mode.report_error ~set_class editor_prel error warnings  >>= fun () ->
    Ace.focus ace_prel ;
    Lwt.return () in
  begin prelude_button
      ~group: toplevel_buttons_group
      ~icon: "typecheck" [%i"Check"] @@ fun () ->
    typecheck true
  end ; 

    (* ---- prepare pane --------------------------------------------------- *)
  let editor_prepare = find_component "learnocaml-exo-prepare-pane" in
  let editor_prep = Ocaml_mode.create_ocaml_editor (Tyxml_js.To_dom.of_div editor_prepare) in
  let ace_prep = Ocaml_mode.get_editor editor_prep in
  let contents=
    let a= get_prepare id in
    if a = "" then
      [%i"(* Local definitions the student\n\
      	  won't be able to see *)\n"]
    else
      a
  in  
  Ace.set_contents ace_prep contents ;
  Ace.set_font_size ace_prep 18;

  let typecheck set_class =
    Learnocaml_toplevel.check top (Ace.get_contents ace_prep) >>= fun res ->
    let error, warnings =
      match res with
      | Toploop_results.Ok ((), warnings) -> None, warnings
      | Toploop_results.Error (err, warnings) -> Some err, warnings in
    let transl_loc { Toploop_results.loc_start ; loc_end } =
      { Ocaml_mode.loc_start ; loc_end } in
    let error = match error with
      | None -> None
      | Some { Toploop_results.locs ; msg ; if_highlight } ->
          Some { Ocaml_mode.locs = List.map transl_loc locs ;
                 msg = (if if_highlight <> "" then if_highlight else msg) } in
    let warnings =
      List.map
        (fun { Toploop_results.locs ; msg ; if_highlight } ->
           { Ocaml_mode.loc = transl_loc (List.hd locs) ;
             msg = (if if_highlight <> "" then if_highlight else msg) })
        warnings in
    Ocaml_mode.report_error ~set_class editor_prep error warnings  >>= fun () ->
    Ace.focus ace_prep ;
    Lwt.return () in
  begin prepare_button
      ~group: toplevel_buttons_group
      ~icon: "typecheck" [%i"Check"] @@ fun () ->
    typecheck true
  end ;
  
  (* ---- editor pane --------------------------------------------------- *)
  let editor_pane = find_component "learnocaml-exo-editor-pane" in
  let editor = Ocaml_mode.create_ocaml_editor (Tyxml_js.To_dom.of_div editor_pane) in
  let ace = Ocaml_mode.get_editor editor in

  let contents =
    let a= get_solution id in
  if a = "" then
    [%i"(* Your solution *)\n"]
  else
    a
      in
  Ace.set_contents ace contents;
  Ace.set_font_size ace 18;
  let save_buffer_test () =
  let ty = "" in
  let type_question = Suite in
  let input = Ace.get_contents ace_testhaut in
  let output = "" in
  let extra_alea = 0 in
  let question = {name; ty; type_question; input; output; extra_alea} in
  let testhaut = get_testhaut id in
  let question_id ="0" in
  let testhaut = StringMap.add question_id question testhaut in
  save_testhaut testhaut id in
  let recovering () =
    let solution = Ace.get_contents ace in
    let titre = get_titre id  in
    let question = Ace.get_contents ace_quest in
    let template = Ace.get_contents ace_temp in
    let testml = Ace.get_contents ace_t in
    save_buffer_test ();
    let testhaut= get_testhaut id in
    let prepare= Ace.get_contents ace_prep in
    let prelude =Ace.get_contents ace_prel in 
    let test ={testml;testhaut} in
    let  diff =
      match Learnocaml_local_storage.(retrieve (editor_state id)) with
      | { Learnocaml_exercise_state.diff } -> diff
      | exception Not_found -> None in
    Learnocaml_local_storage.(store (editor_state id))
      { Learnocaml_exercise_state.id ; solution ; titre ; question ; template ;
        diff ; test ; prepare ; prelude;
        mtime = gettimeofday () } in
  recovering_callback:=recovering ;
  let messages = Tyxml_js.Html5.ul [] in
  begin template_button
      ~icon: "sync" [%i"Gen. template"] @@ fun () ->
    if (Ace.get_contents ace_temp) = "" then        
        Ace.set_contents ace_temp (genTemplate (Ace.get_contents ace) )
    else
      begin
       let aborted, abort_message =
         let t, u = Lwt.task () in
         let btn_cancel = Tyxml_js.Html5.(button [ pcdata [%i"Cancel"] ]) in
         Manip.Ev.onclick btn_cancel ( fun _ ->
                                       hide_loading ~id:"learnocaml-exo-loading" () ; true) ;
         let btn_yes = Tyxml_js.Html5.(button [ pcdata [%i"Yes"] ]) in
         Manip.Ev.onclick btn_yes (fun _ -> Ace.set_contents ace_temp (genTemplate (Ace.get_contents ace));
                                            hide_loading ~id:"learnocaml-exo-loading" ();
                                            true) ;
         let div =
           Tyxml_js.Html5.(div ~a: [ a_class [ "dialog" ] ]
                             [ pcdata [%i"Do you want to crush the template?\n"] ;
                               btn_yes ;
                               pcdata " " ;
                               btn_cancel ]) in
         Manip.SetCss.opacity div (Some "0") ;
         t, div in
       Manip.replaceChildren messages
         Tyxml_js.Html5.[ li [ pcdata "" ] ] ;
       show_loading ~id:"learnocaml-exo-loading" [ abort_message ] ;
       Manip.SetCss.opacity abort_message (Some "1") 
      end;
    Lwt.return ()
  end ;
 
  begin editor_button
      ~icon: "save" [%i"Save"] @@ fun () ->
    recovering () ;
    Lwt.return ()
  end ;

  begin editor_button
      ~icon: "download" [%i"Download"] @@ fun () ->
    recovering () ;
    let name = id ^ ".json" in
    let content =Learnocaml_local_storage.(retrieve (editor_state id)) in  
  let json =
    Json_repr_browser.Json_encoding.construct
      Learnocaml_exercise_state.editor_state_enc
      content in
  let contents =
      (Js._JSON##stringify (json)) in
    Learnocaml_common.fake_download ~name ~contents ;
    Lwt.return ()
  end ;


  let typecheck set_class =
    Learnocaml_toplevel.check top (Ace.get_contents ace) >>= fun res ->
    let error, warnings =
      match res with
      | Toploop_results.Ok ((), warnings) -> None, warnings
      | Toploop_results.Error (err, warnings) -> Some err, warnings in
    let transl_loc { Toploop_results.loc_start ; loc_end } =
      { Ocaml_mode.loc_start ; loc_end } in
    let error = match error with
      | None -> None
      | Some { Toploop_results.locs ; msg ; if_highlight } ->
          Some { Ocaml_mode.locs = List.map transl_loc locs ;
                 msg = (if if_highlight <> "" then if_highlight else msg) } in
    let warnings =
      List.map
        (fun { Toploop_results.locs ; msg ; if_highlight } ->
           { Ocaml_mode.loc = transl_loc (List.hd locs) ;
             msg = (if if_highlight <> "" then if_highlight else msg) })
        warnings in
    Ocaml_mode.report_error ~set_class editor error warnings  >>= fun () ->
    Ace.focus ace ;
    Lwt.return () in
  begin editor_button
      ~group: toplevel_buttons_group
      ~icon: "typecheck" [%i"Check"] @@ fun () ->
    typecheck true
  end ;
  begin toplevel_button
      ~group: toplevel_buttons_group
      ~icon: "run" [%i"Eval code"] @@ fun () ->
    Learnocaml_toplevel.execute_phrase top (Ace.get_contents ace) >>= fun _ ->
    Lwt.return ()
  end ;


  (* ---- testhaut pane --------------------------------------------------- *)
(*let editor_testhaut = find_component "learnocaml-exo-test-pane" in
  let editor_thaut = Ocaml_mode.create_ocaml_editor (Tyxml_js.To_dom.of_div editor_test) in
  let ace_thaut = Ocaml_mode.get_editor editor_thaut in
  Ace.set_contents ace_thaut  (get_testml id); 
  Ace.set_font_size ace_t 18;
*)
   (* let typecheck set_class =
    Learnocaml_toplevel.check top (Ace.get_contents ace_t) >>= fun res ->
    let error, warnings =
      match res with
      | Toploop_results.Ok ((), warnings) -> None, warnings
      | Toploop_results.Error (err, warnings) -> Some err, warnings in
    let transl_loc { Toploop_results.loc_start ; loc_end } =
      { Ocaml_mode.loc_start ; loc_end } in
    let error = match error with
      | None -> None
      | Some { Toploop_results.locs ; msg ; if_highlight } ->
          Some { Ocaml_mode.locs = List.map transl_loc locs ;
                 msg = (if if_highlight <> "" then if_highlight else msg) } in
    let warnings =
      List.map
        (fun { Toploop_results.locs ; msg ; if_highlight } ->
           { Ocaml_mode.loc = transl_loc (List.hd locs) ;
             msg = (if if_highlight <> "" then if_highlight else msg) })
        warnings in
    Ocaml_mode.report_error ~set_class editor_t error warnings  >>= fun () ->
    Ace.focus ace_t ;
    Lwt.return () in *)
  let _ = testhaut_init (find_component "learnocaml-exo-testhaut-pane") id in ();
  begin testhaut_button
      ~group: toplevel_buttons_group
      ~icon: "sync" [%i"Generate"] @@ fun () ->
    let sol = genTemplate (Ace.get_contents ace) in
    if (sol<>"") then    
        (let listeChars = supprRec (' '::(decompositionSol sol 0)) in
        save_quest (genQuestions (get_fct listeChars []) []) id ;
        Manip.removeChildren (find_component "learnocaml-exo-testhaut-pane");
        testhaut_init (find_component "learnocaml-exo-testhaut-pane") id )
    else Lwt.return ();
  end ;                             
  begin testhaut_button
      ~group: toplevel_buttons_group
      ~icon: "typecheck" [%i"Check"] @@ fun () ->
    Lwt.return ()
  end ;
  let compile () = let listeFonction = constructListeQuest (get_id_question id) id in
               let tests = constructFinalSol listeFonction in 
               match Learnocaml_local_storage.(retrieve (editor_state id) ) with
               |{id;titre;prepare;diff;solution;question;template;test;prelude;mtime}->
                 let mtime=gettimeofday () in
                 let test ={testml=tests;testhaut=test.testhaut} in
                 let nvexo= {id;titre;prepare;diff;solution;question;template;test;prelude;mtime} in    
                 Learnocaml_local_storage.(store (editor_state id)) nvexo;
                 Ace.set_contents ace_t  (get_testml id);
                 Manip.removeChildren (find_component "learnocaml-exo-testhaut-pane");
                 testhaut_init (find_component "learnocaml-exo-testhaut-pane") id ;
                 select_tab "test" in
  begin testhaut_button
      ~group: toplevel_buttons_group
      ~icon: "run" [%i"Compile"] @@ fun () ->
         let aborted, abort_message =
           let t, u = Lwt.task () in
           let btn_no = Tyxml_js.Html5.(button [ pcdata [%i"No"] ]) in
           Manip.Ev.onclick btn_no ( fun _ ->
              hide_loading ~id:"learnocaml-exo-loading" () ; true) ;
           let btn_yes = Tyxml_js.Html5.(button [ pcdata [%i"Yes"] ]) in
           Manip.Ev.onclick btn_yes (fun _ ->
               hide_loading ~id:"learnocaml-exo-loading" ();
               compile () ; true) ;
           let div =
             Tyxml_js.Html5.(div ~a: [ a_class [ "dialog" ] ]
                          [ pcdata [%i"Are you sure you want to overwrite the content of Test.ml?\n"] ;
                            btn_yes ;
                            pcdata " " ;
                            btn_no; ]) in
      Manip.SetCss.opacity div (Some "0") ;
      t, div in 
    Manip.replaceChildren messages
      Tyxml_js.Html5.[ li [ pcdata "" ] ] ;
    show_loading ~id:"learnocaml-exo-loading" [ abort_message ] ;
    Manip.SetCss.opacity abort_message (Some "1") ;
      Lwt.return ()
  end ;



  
  (* ---- main toolbar -------------------------------------------------- *)
  let exo_toolbar = find_component "learnocaml-exo-toolbar" in
  let toolbar_button = button ~container: exo_toolbar ~theme: "light" in
  let toolbar_button2 = button2 ~container: exo_toolbar ~theme: "light" in
  begin toolbar_button
      ~icon: "left" [%i"Metadata"] @@ fun () ->
      recovering () ;
      Dom_html.window##.location##assign
        (Js.string ("new_exercise.html#id=" ^ id ^ "&action=open"));
    Lwt.return ()
  end;
  
  begin toolbar_button
      ~icon: "upload" [%i"Experiment"] @@ fun ()->
    recovering () ; 

  let exo ()=
  let titre =  get_titre id in
  let description="" in
  
  let exo1= Learnocaml_exercise.set  Learnocaml_exercise.id id Learnocaml_exercise.empty in
  let exo2= Learnocaml_exercise.set Learnocaml_exercise.title titre exo1 in
  let exo3 =Learnocaml_exercise.set Learnocaml_exercise.max_score 1 exo2 in
  let exo4 =Learnocaml_exercise.set Learnocaml_exercise.prepare (get_prepare id) exo3 in
  let exo5 =Learnocaml_exercise.set Learnocaml_exercise.prelude (get_prelude id) exo4 in
  let exo6 =Learnocaml_exercise.set Learnocaml_exercise.solution (get_solution id) exo5 in
  let exo7 =Learnocaml_exercise.set Learnocaml_exercise.test (get_testml id) exo6 in
  let exo8 =Learnocaml_exercise.set Learnocaml_exercise.template (get_template id) exo7 in
  Learnocaml_exercise.set Learnocaml_exercise.descr description exo8
  in
    
  let aborted, abort_message =
     let t, u = Lwt.task () in
     let btn = Tyxml_js.Html5.(button [ pcdata [%i"abort"] ]) in
     Manip.Ev.onclick btn (fun _ -> Lwt.wakeup u () ; true) ;
     let div =
        Tyxml_js.Html5.(div ~a: [ a_class [ "dialog" ] ]
                          [ pcdata [%i"Grading is taking a lot of time, "] ;
                            btn ;
                            pcdata "?" ]) in
     Manip.SetCss.opacity div (Some "0") ;
     t, div in
  let worker = ref (Grading_jsoo.get_grade (exo ())) in
  let correction = Learnocaml_exercise.get Learnocaml_exercise.solution (exo ()) in
    let grading = 
      !worker correction >>= fun (report, _, _, _) ->
      Lwt.return report in
    let abortion =
      Lwt_js.sleep 5. >>= fun () ->
      Manip.SetCss.opacity abort_message (Some "1") ;
      aborted >>= fun () ->
      Lwt.return Learnocaml_report.[ Message ([ Text [%i"Grading aborted by user."] ], Failure) ] in
    Lwt.pick [ grading ; abortion ] >>= fun report_correction ->
    let score_maxi, failed2 = Learnocaml_report.result_of_report report_correction in
    
    Dom_html.window##.location##assign
        (Js.string ("exercise.html#id=." ^ id ^ "&score="^(string_of_int (score_maxi))^"&action=open"));
    Lwt.return_unit
  end; 
 
  
  let messages = Tyxml_js.Html5.ul [] in
  begin toolbar_button
      ~icon: "list" [%i"Exercises"] @@ fun () ->
    let aborted, abort_message =
      let t, u = Lwt.task () in
      let btn_cancel = Tyxml_js.Html5.(button [ pcdata [%i"Cancel"] ]) in
      Manip.Ev.onclick btn_cancel ( fun _ ->
        hide_loading ~id:"learnocaml-exo-loading" () ; true) ;
      let btn_yes = Tyxml_js.Html5.(button [ pcdata [%i"Yes"] ]) in
      Manip.Ev.onclick btn_yes (fun _ ->
      recovering () ;
      Dom_html.window##.location##assign
        (Js.string "index.html#activity=editor") ; true) ;
      let btn_no = Tyxml_js.Html5.(button [ pcdata [%i"No"] ]) in
      Manip.Ev.onclick btn_no (fun _ -> 
      Dom_html.window##.location##assign
        (Js.string "index.html#activity=editor") ; true);
      let div =
        Tyxml_js.Html5.(div ~a: [ a_class [ "dialog" ] ]
                          [ pcdata [%i"Do you want to save before closing?\n"] ;
                            btn_yes ;
                            pcdata " " ;
                            btn_no ;
                            pcdata " " ;
                            btn_cancel ]) in
      Manip.SetCss.opacity div (Some "0") ;
      t, div in 
    Manip.replaceChildren messages
      Tyxml_js.Html5.[ li [ pcdata "" ] ] ;
    show_loading ~id:"learnocaml-exo-loading" [ abort_message ] ;
    Manip.SetCss.opacity abort_message (Some "1") ;
    Lwt.return ()
  end ;
  let messages = Tyxml_js.Html5.ul [] in
  let callback text =
    Manip.appendChild messages Tyxml_js.Html5.(li [ pcdata text ]) in
  let exo ()=
  let titre =  get_titre id in
  let description="" in
  
  let exo1= Learnocaml_exercise.set  Learnocaml_exercise.id id Learnocaml_exercise.empty in
  let exo2= Learnocaml_exercise.set Learnocaml_exercise.title titre exo1 in
  let exo3 =Learnocaml_exercise.set Learnocaml_exercise.max_score 1 exo2 in
  let exo4 =Learnocaml_exercise.set Learnocaml_exercise.prepare (get_prepare id) exo3 in
  let exo5 =Learnocaml_exercise.set Learnocaml_exercise.prelude (get_prelude id) exo4 in
  let exo6 =Learnocaml_exercise.set Learnocaml_exercise.solution (get_solution id) exo5 in
  let exo7 =Learnocaml_exercise.set Learnocaml_exercise.test (get_testml id) exo6 in
  let exo8 =Learnocaml_exercise.set Learnocaml_exercise.template (get_template id) exo7 in
  Learnocaml_exercise.set Learnocaml_exercise.descr description exo8
  in
  let worker () = ref (Grading_jsoo.get_grade ~callback (exo () )  ) in
  let grade () = let aborted, abort_message =
      let t, u = Lwt.task () in
      let btn = Tyxml_js.Html5.(button [ pcdata [%i "abort" ]]) in
      Manip.Ev.onclick btn (fun _ -> Lwt.wakeup u () ; true) ;
      let div =
        Tyxml_js.Html5.(div ~a: [ a_class [ "dialog" ] ]
                          [ pcdata [%i"Grading is taking a lot of time, "] ;
                            btn ;
                            pcdata "?" ]) in
      Manip.SetCss.opacity div (Some "0") ;
      t, div in
    Manip.replaceChildren messages
      Tyxml_js.Html5.[ li [ pcdata [%i"Launching the grader"] ] ] ;
    show_loading ~id:"learnocaml-exo-loading" [ messages ; abort_message ];
    Lwt_js.sleep 1. >>= fun () ->
    let solution = Ace.get_contents ace in
    Learnocaml_toplevel.check top solution >>= fun res ->
    match res with
    | Toploop_results.Ok ((), _) ->
        let grading =
          !(worker ()) solution >>= fun (report, _, _, _) ->
          Lwt.return report in
        let abortion =
          Lwt_js.sleep 5. >>= fun () ->
          Manip.SetCss.opacity abort_message (Some "1") ;
          aborted >>= fun () ->
          Lwt.return Learnocaml_report.[ Message ([ Text [%i"Grading aborted by user."] ], Failure) ] in
        Lwt.pick [ grading ; abortion ] >>= fun report ->
        let grade = display_report (exo () ) report in
        (worker() ) := Grading_jsoo.get_grade ~callback ( exo () ) ;
        Learnocaml_local_storage.(store (exercise_state id))
          { Learnocaml_exercise_state.grade = Some grade ; solution ; report = Some report ;
            mtime = gettimeofday () } ;
        select_tab "report" ;
        Lwt_js.yield () >>= fun () ->
        hide_loading ~id:"learnocaml-exo-loading" () ;
        Lwt.return ()
    | Toploop_results.Error _ ->
        (* let msg =
          Learnocaml_report.[ Text [%i"Error in your code."] ; Break ;
                   Text [%i"Cannot start the grader if your code does not typecheck."] ] in
        let report = Learnocaml_report.[ Message (msg, Failure) ] in
        let grade = display_report (exo () ) report in
        Learnocaml_local_storage.(store (exercise_state id))
          { Learnocaml_exercise_state.grade = Some grade ; solution ; report = Some report ;
            mtime = gettimeofday () } ; *)
        select_tab "report" ;
        Lwt_js.yield () >>= fun () ->
        hide_loading ~id:"learnocaml-exo-loading" () ;
        typecheck true in
  begin toolbar_button2
      ~icon: "reload" [%i"Grade!"] @@ fun () ->
    recovering () ;
    if arg "tab" = "testhaut" then
      begin
        let aborted, abort_message =
          let t, u = Lwt.task () in
          let btn_cancel = Tyxml_js.Html5.(button [ pcdata [%i"Cancel"] ]) in
          Manip.Ev.onclick btn_cancel ( fun _ ->
                                        hide_loading ~id:"learnocaml-exo-loading" () ; true) ;
          let btn_compile = Tyxml_js.Html5.(button [ pcdata [%i"Compile"] ]) in
          Manip.Ev.onclick btn_compile (fun _ ->
              recovering () ;
              compile ();
              grade (); true) ;
          let btn_no = Tyxml_js.Html5.(button [ pcdata [%i"Grade without compiling Test"] ]) in
          Manip.Ev.onclick btn_no (fun _ -> grade () ; true);
          let div =
            Tyxml_js.Html5.(div ~a: [ a_class [ "dialog" ] ]
                              [ pcdata [%i"The Grade feature relies on the contents of Test.ml. \
                              Do you want to compile the high-level tests and overwrite Test.ml?\n"] ;
                                btn_compile ;
                                pcdata " " ;
                                btn_no ;
                                pcdata " " ;
                                btn_cancel ]) in
          Manip.SetCss.opacity div (Some "0") ;
          t, div in 
        Manip.replaceChildren messages
          Tyxml_js.Html5.[ li [ pcdata "" ] ] ;
        show_loading ~id:"learnocaml-exo-loading" [ abort_message ] ;
        Manip.SetCss.opacity abort_message (Some "1") ;
        Lwt.return ()
      end
    else
      grade ()
  end ;
  grade_black:= (fun ()->
    let button_grade = getElementById "grade_id" in
    button_grade##.style##.background := (Js.string "#222") );
  grade_red:= (fun ()->
    let button_grade = getElementById "grade_id" in
    button_grade##.style##.background := (Js.string "#aaa") );                  
  (* ---- return -------------------------------------------------------- *)
  toplevel_launch >>= fun _ ->
  typecheck false >>= fun () ->
  hide_loading ~id:"learnocaml-exo-loading" () ;
  let () = Lwt.async @@ fun () ->
     let _ = Dom_html.window##setInterval (Js.wrap_callback (fun () -> onload ())) 200.0; in
     Lwt.return () in
  Lwt.return ();;

let () = Lwt.async @@ fun ()->
    let _= Dom_html.window##setInterval (Js.wrap_callback (fun () -> !recovering_callback ()))
    (auto_save_interval *. 1000.0);
    in 
    Lwt.return_unit ;;
