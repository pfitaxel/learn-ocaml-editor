open Js_of_ocaml
open Js_utils
open Str
open Lwt.Infix
open Dom_html
open Learnocaml_common

module StringMap = Map.Make (String)


let init_tabs, select_tab =
  let names = [ "solution"; "spec"; "suite" ] in
  let current = ref "suite" in
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
        if List.mem requested names then requested else "suite"
      with Not_found -> "suite"
    end ;
    List.iter
      (fun name ->
         Manip.removeClass
           (find_component ("learnocaml-tab-" ^ name))
           "front-tab" )
      names ;
    select_tab !current in
  init_tabs, select_tab 
                                      
let name = match getElementById_coerce "name" CoerceTo.input with
    None -> failwith "element inconnu"
   |Some s -> s;;
let ty = getElementById_coerce "ty" CoerceTo.input;;
let solution = getElementById "solution";;
let spec = getElementById "spec";;
let suite = getElementById "suite";;


let para = getElementById ("txt_name");;

let setInnerHtml elt s =    
  elt##.innerHTML:=Js.string s ;;
let _ = solution##.onclick:= handler (fun _ -> select_tab "solution"; Js._true);;

let _ = spec##.onclick:= handler (fun _ -> select_tab "spec"; Js._true);;
let _ = suite##.onclick:= handler (fun _ -> select_tab "suite"; Js._true);;
