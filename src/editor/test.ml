open Js_of_ocaml
open Js_utils
open Str
open Lwt.Infix
open Dom_html
open Learnocaml_common

module StringMap = Map.Make (String)

let setInnerHtml elt s =
  elt##.innerHTML := JS.string s;;

let toString = function
  |None -> failwith "incorrect_input"
  |Some input -> Js.to_string input##.value
let toStringOpt = function
  | None -> None
  | Some input -> Some (Js.to_string input##.value)
let toFloatOpt = function
  | None -> None
  | Some input -> float_of_string_opt (Js.to_string input##.value)


let init_tabs, select_tab =
  let names = [ "solution"; "spec"; "suite" ] in
  let current = ref "question" in
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
        if List.mem requested names then requested else "solution"
      with Not_found -> "solution"
    end ;
    List.iter
      (fun name ->
         Manip.removeClass
           (find_component ("learnocaml-tab-" ^ name))
           "front-tab" ;)
      names ;
    select_tab !current in
  init_tabs, select_tab
                                      
let name = getElementById_coerce "name" CoerceTo.input;;
let ty = getElementById_coerce "ty" CoerceTo.input;;
let solution = getElementById "solution";;
let spec = getElementById "spec";;
let suite = getElementById "suite";;

let toplevel_buttons_group = button_group () in
  enable_button_group toplevel_buttons_group;
let test_toolbar = find_component "learnocaml-test-toolbar" in
let test_button = button ~container: test_toolbar ~theme: "light" in
let id = arg "id" in
begin test_button
        ~group:toplevel_buttons_group
        ~icon: "cancel" [%i "Cancel"] @@ fun () ->
    Dom_html.window##.location##assign
      (Js.string ("editor.html#id="^id^"action=open"));
    Lwt.return ()
end;
begin test_button
        ~group:toplevel_buttons_group
        ~icon: "save" [%i "Save"] @@ fun () ->
    Dom_html.window##.location##assign
      (Js.string ("editor.html#id="^id^"action=open"));
    Lwt.return ()
end;


solution##.onclick := handler (fun _ ->
     select_tab "solution";
     Js._true);
spec##.onclick := handler (fun _ ->
     select_tab "spec";
     Js._true);
suite##.onclick := handler (fun _ ->
     select_tab "suite";
     Js._true);
             
