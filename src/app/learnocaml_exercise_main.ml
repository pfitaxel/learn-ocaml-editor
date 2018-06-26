(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2016 OCamlPro.
 *
 * Learn-OCaml is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
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
let get_titre id = Learnocaml_local_storage.(retrieve (editor_state id)).titre

let get_diff id = Learnocaml_local_storage.(retrieve (editor_state id)).diff
let get_solution id = Learnocaml_local_storage.(retrieve (editor_state id)).solution
let get_question id = Learnocaml_local_storage.(retrieve (editor_state id)).question
let get_template id = Learnocaml_local_storage.(retrieve (editor_state id)).template
let get_testml id = Learnocaml_local_storage.(retrieve (editor_state id)).test.testml
let get_prelude id = Learnocaml_local_storage.(retrieve (editor_state id)).prelude
let get_prepare id = Learnocaml_local_storage.(retrieve (editor_state id)).prepare
                                      
let ref_grade = ref 150 ;;
let init_tabs, select_tab =
  let names = [ "text" ; "toplevel" ; "report" ; "editor"] in
  let current = ref "text" in
  let select_tab name =
    set_arg "tab" name ;
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
        if List.mem requested names then requested else "text"
      with Not_found -> "text"
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

(*
let score_maxi exo =               
let aborted, abort_message =
     let t, u = Lwt.task () in
     let btn = Tyxml_js.Html5.(button [ pcdata [%i"abort"] ]) in
     Manip.Ev.onclick btn (fun _ -> Lwt.wakeup u () ; true) ;
     let div =
        Tyxml_js.Html5.(div ~a: [ a_class [ "dialog" ] ]
                          [ pcdata [%i"Grading is taking a lot of time, "] ;
                            btn ;
                            pcdata " ?" ]) in
     Manip.SetCss.opacity div (Some "0") ;
     t, div in
  let worker = ref (Grading_jsoo.get_grade exo) in
  let correction = Learnocaml_exercise.get Learnocaml_exercise.solution exo in
    let grading = 
      !worker correction >>= fun (report, _, _, _) ->
      Lwt.return report in
    let abortion =
      Lwt_js.sleep 5. >>= fun () ->
      Manip.SetCss.opacity abort_message (Some "1") ;
      aborted >>= fun () ->
      Lwt.return Learnocaml_report.[ Message ([ Text [%i"Grading aborted by user."] ], Failure) ] in
    Lwt.pick [ grading ; abortion ] >>= fun report_correction ->
    let r,f = Learnocaml_report.result_of_report report_correction in let q = ref_grade = ref r in Lwt.return r  ;;
 *)




               
let display_report exo report =
  let aborted, abort_message =
     let t, u = Lwt.task () in
     let btn = Tyxml_js.Html5.(button [ pcdata [%i"abort"] ]) in
     Manip.Ev.onclick btn (fun _ -> Lwt.wakeup u () ; true) ;
     let div =
        Tyxml_js.Html5.(div ~a: [ a_class [ "dialog" ] ]
                          [ pcdata [%i"Grading is taking a lot of time, "] ;
                            btn ;
                            pcdata " ?" ]) in
     Manip.SetCss.opacity div (Some "0") ;
     t, div in
  let worker = ref (Grading_jsoo.get_grade exo) in
  let correction = Learnocaml_exercise.get Learnocaml_exercise.solution exo in
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
  let score, failed = Learnocaml_report.result_of_report report in
  let report_button = find_component "learnocaml-exo-button-report" in
  Manip.removeClass report_button "success" ;
  Manip.removeClass report_button "failure" ;
  Manip.removeClass report_button "partial" ;
  let grade = score * 100 /score_maxi(*Learnocaml_exercise.get Learnocaml_exercise.max_score exo*) in
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
  ref_grade = ref grade;
  Lwt.return ()
             
             
let set_string_translations () =
  let translations = [
    "txt_preparing", [%i"Preparing the environment"];
    "learnocaml-exo-button-editor", [%i"Editor"];
    "learnocaml-exo-button-toplevel", [%i"Toplevel"];
    "learnocaml-exo-button-report", [%i"Report"];
    "learnocaml-exo-button-text", [%i"Exercise"];
    "learnocaml-exo-editor-pane", [%i"Editor"];
    "txt_grade_report", [%i"Click the Grade button to get your report"];
  ] in
  List.iter
    (fun (id, text) ->
       Manip.setInnerHtml (find_component id) text)
    translations

let () =
  Lwt.async_exception_hook := begin function
    | Failure message -> fatal message
    | Server_caller.Cannot_fetch message -> fatal message
    | exn -> fatal (Printexc.to_string exn)
  end ;
  (match Js_utils.get_lang() with Some l -> Ocplib_i18n.set_lang l | None -> ());
  Lwt.async @@ fun () ->
  set_string_translations ();
  Learnocaml_local_storage.init () ;
  (* ---- launch everything --------------------------------------------- *)
  let toplevel_buttons_group = button_group () in
  disable_button_group toplevel_buttons_group (* enabled after init *) ;
  let toplevel_toolbar = find_component "learnocaml-exo-toplevel-toolbar" in
  let editor_toolbar = find_component "learnocaml-exo-editor-toolbar" in
  let toplevel_button = button ~container: toplevel_toolbar ~theme: "dark" in
  let editor_button = button ~container: editor_toolbar ~theme: "light" in
  let transResultOption = function
  |None -> false
  |Some s-> true in
  let idEditor s = transResultOption (Regexp.string_match (Regexp.regexp "^[\.]+") s 0) in
  let id = arg "id" in
  
  let exercise_fetch = match idEditor id with
    | false -> Server_caller.fetch_exercise id
    | _ -> let id = String.sub id 1 ((String.length id)-1) in
  let exo0 ()=
  let titre =  get_titre id in
  let question =get_question id in
  let question =Omd.to_html (Omd.of_string question) in

  let exo1= Learnocaml_exercise.set  Learnocaml_exercise.id id Learnocaml_exercise.empty in
  let exo2= Learnocaml_exercise.set Learnocaml_exercise.title titre exo1 in
  let exo3 =Learnocaml_exercise.set Learnocaml_exercise.max_score 80 exo2 in
  let exo4 =Learnocaml_exercise.set Learnocaml_exercise.prepare (get_prepare id) exo3 in
  let exo5 =Learnocaml_exercise.set Learnocaml_exercise.prelude (get_prelude id) exo4 in
  let exo6 =Learnocaml_exercise.set Learnocaml_exercise.solution (get_solution id) exo5 in
  let exo7 =Learnocaml_exercise.set Learnocaml_exercise.test (get_testml id) exo6 in
  let exo8 =Learnocaml_exercise.set Learnocaml_exercise.template (get_template id) exo7 in
  Learnocaml_exercise.set Learnocaml_exercise.descr (question) exo8
  in
   Lwt.return (exo0 () )
in  
  let after_init top =
    exercise_fetch >>= fun exo ->
    begin match Learnocaml_exercise.(get prelude) exo with
      | "" -> Lwt.return true
      | prelude ->
          Learnocaml_toplevel.load ~print_outcome:true top
            ~message: [%i"loading the prelude..."]
            prelude
    end >>= fun r1 ->
    Learnocaml_toplevel.load ~print_outcome:false top
      (Learnocaml_exercise.(get prepare) exo) >>= fun r2 ->
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
  exercise_fetch >>= fun exo ->


  
  let solution = match Learnocaml_local_storage.(retrieve (exercise_state id)) with
    | { Learnocaml_exercise_state.report = Some report ; solution } ->
        let _ : int = (display_report exo report);!ref_grade in
        Some solution
    | { Learnocaml_exercise_state.report = None ; solution } ->
        Some solution
    | exception Not_found -> None in
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
  (* ---- text pane ----------------------------------------------------- *)
  let text_container = find_component "learnocaml-exo-tab-text" in
  let text_iframe = Dom_html.createIframe Dom_html.document in
  Manip.replaceChildren text_container
    Tyxml_js.Html5.[ h1 [ pcdata (Learnocaml_exercise.(get title) exo) ] ;
                     Tyxml_js.Of_dom.of_iFrame text_iframe ] ;
  let prelude = Learnocaml_exercise.(get prelude) exo in
  if prelude <> "" then begin
    let open Tyxml_js.Html5 in
    let state = ref (match arg "prelude" with
        | exception Not_found -> true
        | "shown" -> true
        | "hidden" -> false
        | _ -> failwith "Bad format for argument prelude.") in
    let prelude_btn = button [] in
    let prelude_title = h1 [ pcdata [%i"OCaml prelude"] ;
                             prelude_btn ] in
    let prelude_container =
      pre ~a: [ a_class [ "toplevel-code" ] ]
        (Learnocaml_toplevel_output.format_ocaml_code prelude) in
    let update () =
      if !state then begin
        Manip.replaceChildren prelude_btn [ pcdata ("↳ "^[%i"Hide"]) ] ;
        Manip.SetCss.display prelude_container "" ;
        set_arg "prelude" "shown"
      end else begin
        Manip.replaceChildren prelude_btn [ pcdata ("↰ "^[%i"Show"]) ] ;
        Manip.SetCss.display prelude_container "none" ;
        set_arg "prelude" "hidden"
      end in
    update () ;
    Manip.Ev.onclick prelude_btn
      (fun _ -> state := not !state ; update () ; true) ;
    Manip.appendChildren text_container
      Tyxml_js.Html5.[ prelude_title ; prelude_container ]
  end ;
  Js.Opt.case
    (text_iframe##.contentDocument)
    (fun () -> failwith "cannot edit iframe document")
    (fun d ->
       let mathjax_url =
         "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.1.0/MathJax.js\
          ?config=AM_HTMLorMML-full"
       in
       let html = Format.asprintf
           "<!DOCTYPE html>\
            <html><head>\
            <title>%s - exercise text</title>\
            <meta charset='UTF-8'>\
            <link rel='stylesheet' href='css/learnocaml_standalone_description.css'>\
            <script type='text/javascript' src='%s'></script>\
            </head>\
            <body>\
            %s\
            </body>\
            </html>"
           (Learnocaml_exercise.(get title) exo)
           mathjax_url
           (Learnocaml_exercise.(get descr) exo) in
       d##open_;
       d##write (Js.string html);
       d##close) ;
  (* ---- editor pane --------------------------------------------------- *)
  let editor_pane = find_component "learnocaml-exo-editor-pane" in
  let editor = Ocaml_mode.create_ocaml_editor (Tyxml_js.To_dom.of_div editor_pane) in
  let ace = Ocaml_mode.get_editor editor in
  Ace.set_contents ace
    (match solution with
     | Some solution -> solution
     | None -> Learnocaml_exercise.(get template) exo) ;
  Ace.set_font_size ace 18;
  begin editor_button
      ~icon: "cleanup" [%i"Reset"] @@ fun () ->
    Ace.set_contents ace (Learnocaml_exercise.(get template) exo) ;
    Lwt.return ()
  end ;
  begin editor_button
      ~icon: "save" [%i"Save"] @@ fun () ->
    let solution = Ace.get_contents ace in
    let report, grade =
      match Learnocaml_local_storage.(retrieve (exercise_state id)) with
      | { Learnocaml_exercise_state.report ; grade } -> report, grade
      | exception Not_found -> None, None in
    Learnocaml_local_storage.(store (exercise_state id))
      { Learnocaml_exercise_state.report ; grade ; solution ;
        mtime = gettimeofday () } ;
    Lwt.return ()
  end ;
  begin editor_button
      ~icon: "download" [%i"Download"] @@ fun () ->
    let name = id ^ ".ml" in
    let contents = Js.string (Ace.get_contents ace) in
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
  (* ---- main toolbar -------------------------------------------------- *)
  let exo_toolbar = find_component "learnocaml-exo-toolbar" in
  let toolbar_button = button ~container: exo_toolbar ~theme: "light" in
  let () = if idEditor id then
    begin
    let id = String.sub id 1 ((String.length id)-1) in
    begin toolbar_button
       ~icon: "upload" "Editor" @@ fun ()->
       Dom_html.window##.location##assign
       (Js.string ("editor.html#id=" ^ id ^ "&action=open"));                                    
       Lwt.return_unit 
    end;
    end;
  in 
  begin toolbar_button
      ~icon: "list" [%i"Exercises"] @@ fun () ->
    Dom_html.window##.location##assign
      (Js.string "index.html#activity=exercises") ;
    Lwt.return ()
  end; (*   marque fin de la barre au dessus de celle a creer   ééé   *)
  let messages = Tyxml_js.Html5.ul [] in
  let callback text =
    Manip.appendChild messages Tyxml_js.Html5.(li [ pcdata text ]) in
  let worker = ref (Grading_jsoo.get_grade ~callback exo) in
  begin toolbar_button
      ~icon: "reload" [%i"Grade!"] @@ fun () ->
    let aborted, abort_message =
      let t, u = Lwt.task () in
      let btn = Tyxml_js.Html5.(button [ pcdata [%i"abort"] ]) in
      Manip.Ev.onclick btn (fun _ -> Lwt.wakeup u () ; true) ;
      let div =
        Tyxml_js.Html5.(div ~a: [ a_class [ "dialog" ] ]
                          [ pcdata [%i"Grading is taking a lot of time, "] ;
                            btn ;
                            pcdata " ?" ]) in
      Manip.SetCss.opacity div (Some "0") ;
      t, div in
    Manip.replaceChildren messages
      Tyxml_js.Html5.[ li [ pcdata [%i"Launching the grader"] ] ] ;
    show_loading ~id:"learnocaml-exo-loading" [ messages ; abort_message ] ;
    Lwt_js.sleep 1. >>= fun () ->
    let solution = Ace.get_contents ace in
    Learnocaml_toplevel.check top solution >>= fun res ->
    match res with
    | Toploop_results.Ok ((), _) ->
        let grading =
          !worker solution >>= fun (report, _, _, _) ->
          Lwt.return report in
        let abortion =
          Lwt_js.sleep 5. >>= fun () ->
          Manip.SetCss.opacity abort_message (Some "1") ;
          aborted >>= fun () ->
          Lwt.return Learnocaml_report.[ Message ([ Text [%i"Grading aborted by user."] ], Failure) ] in
        Lwt.pick [ grading ; abortion ] >>= fun report ->
        let grade =(display_report exo report);!ref_grade  in
        (*if not(idEditor id) then
          begin
        worker := Grading_jsoo.get_grade ~callback exo ;
        Learnocaml_local_storage.(store (exercise_state id))
          { Learnocaml_exercise_state.grade = Some grade ; solution ; report = Some report ;
            mtime = gettimeofday () } end;*)
        select_tab "report" ;
        Lwt_js.yield () >>= fun () ->
        hide_loading ~id:"learnocaml-exo-loading" () ;
        Lwt.return ()
    | Toploop_results.Error _ ->
        let msg =
          Learnocaml_report.[ Text [%i"Error in your code."] ; Break ;
                   Text [%i"Cannot start the grader if your code does not typecheck."] ] in
        let report = Learnocaml_report.[ Message (msg, Failure) ] in
        let grade = (display_report exo report);!ref_grade in
        (*if not(idEditor id) then
          begin
        Learnocaml_local_storage.(store (exercise_state id))
          { Learnocaml_exercise_state.grade = Some grade ; solution ; report = Some report ;
            mtime = gettimeofday () } end ;*)
        select_tab "report" ;
        Lwt_js.yield () >>= fun () ->
        hide_loading ~id:"learnocaml-exo-loading" () ;
        typecheck true
  end ;
  (* ---- return -------------------------------------------------------- *)
  toplevel_launch >>= fun _ ->
  typecheck false >>= fun () ->
  hide_loading ~id:"learnocaml-exo-loading" () ;
  Lwt.return ()
;;
