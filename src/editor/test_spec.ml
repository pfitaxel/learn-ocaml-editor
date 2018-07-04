module type TYPING = sig
  (** should return a representation of a type from its string serialisation *)
  val ty_of : string -> 'a Ty.ty
end

module Make(Test_lib : Test_lib.S) (Typing : TYPING) = struct

open Test_lib
open Learnocaml_report

(* sampler: (unit -> ('ar -> 'row, 'ar -> 'urow, 'ret) args) *)

type test_qst_untyped =
  | TestAgainstSol of
      { name: string
      ; ty: string 
      ; gen: int
      ; suite: string }
  | TestAgainstSpec of
      { name: string
      ; ty: string
      ; gen: int
      ; suite: string
      ; spec : string }
  | TestSuite of
      { suite: string
      ; ty: string
      ; name: string }

type outcome =
  | Correct of string option
  | Wrong of string option

(* TODO val get_test_qst : test_qst_untyped -> test_qst_typed *)

type test_qst_typed =
  | TestAgainstSol :
      { name: string
      ; prot: (('ar -> 'row) Ty.ty, 'ar -> 'urow, 'ret) prot
      ; gen: int
      ; suite: ('ar -> 'row, 'ar -> 'urow, 'ret) args list } -> test_qst_typed
  | TestAgainstSpec :
      { name: string
      ; prot: (('ar -> 'row) Ty.ty, 'ar -> 'urow, 'ret) prot
      ; gen: int
      ; suite: ('ar -> 'row, 'ar -> 'urow, 'ret) args list
      ; spec : ('ar -> 'row) -> ('ar -> 'row, 'ar -> 'urow, 'ret) args -> 'ret -> outcome } -> test_qst_typed
  | TestSuite :
      { name: string
      ; prot: (('ar -> 'row) Ty.ty, 'ar -> 'urow, 'ret) prot
      ; suite: (('ar -> 'row, 'ar -> 'urow, 'ret) args * (unit -> 'ret)) list } -> test_qst_typed

(** Notation for TestAgainstSpec *)
let (~~) b = if b then Correct None else Wrong None
(** Notations for TestSuite *)
let (==>) a b = (a, fun () -> b)
(* let (=>) a b = (a, fun () -> Lazy.force b) (* needs module Lazy *) *)
(** Notations for heterogeneous lists *)
let (@:) a l = arg a @@ l
let (!!) b = last b
let (@:!!) a b = a @: !! b
(* Homogeneous case, for testing purposes
let (@:) a l = a :: l
let (!!) b = b :: []
let (@:!!) a b = a @: !! b
 *)
(* TODO missing: nth_arg *)
(*
let example_constr_sol =
  TestAgainstSol
    { name = "opp"; 
      prot = (last_ty [%ty: int] [%ty: int] );
      gen = 0;
      suite = [!! 0; !! 1; !! 2; !! ~-1]
    }
*)


    

let example_constr_spec =
  TestAgainstSpec
    { name = "idempotent";
      prot = (last_ty [%ty: (int)] [%ty: int]);
      gen = 0;
      suite = [!! 0; !! 1; !! 2];
      spec = fun f args ret -> (* ret = apply f args *)
      (* Function f should be idempotent *)
      ~~ (ret = apply f (!! ret))
    }

let example_constr_suite =
  TestSuite
    {
      name = "xor";
      prot = (arg_ty [%ty: bool] (last_ty [%ty: bool] [%ty: bool]));
      suite = [false @:!! false ==> false;
               false @:!! true ==> true;
               true @:!! false ==> true;
               true @:!! true ==> false]
    }

let local_dummy : 'a sampler = fun () -> failwith "dummy sampler"
(* à n'utiliser que si on passe l'argument ~gen:0 (pas d'alea) *)
                                               
let test_question (t : test_qst_typed) =
  match t with
  | TestAgainstSol t ->
     test_function_against
       ~gen:0 ~sampler:local_dummy
       ~test:test (* could take into account exceptions/sorted lists/etc. *)
       t.prot
       (lookup_student (ty_of_prot t.prot) t.name)
       (lookup_solution (ty_of_prot t.prot) t.name)
       t.suite
  | TestAgainstSpec t ->
     let to_string ty v = Format.asprintf "%a" (typed_printer ty) v in
     let stud = lookup_student (ty_of_prot t.prot) t.name in
     test_value stud @@ fun uf ->
     (* no sampler for the moment *)
     let open Learnocaml_report in
     List.flatten @@ List.map (fun args ->
       let code = Format.asprintf "@[<hv 2>%s,%a@]" t.name (print t.prot) args in
       let ret_ty = get_ret_ty (ty_of_prot t.prot) args in
       Message ([ Text [%i"Checking spec for"] ; Code code ], Informative) ::
         let ret = apply uf args in
         let value = to_string ret_ty ret in
       let (text, note) = match t.spec uf args ret with
         | Correct None -> ([%i"Correct spec"], Success 1)
         | Correct (Some message) -> (message, Success 1)
         | Wrong None -> ([%i"Wrong spec"], Failure)
         | Wrong (Some message) -> (message, Failure) in
       [Message ([Text [%i"Got value"]; Code value; Text (": " ^ text)], note)])
     t.suite
  | TestSuite t ->
     test_function
       ~test:test (* could take into account exceptions/sorted lists/etc. *)
       t.prot
       (lookup_student (ty_of_prot t.prot) t.name)
       t.suite
end

open Editor_lib
 
(*let types_de_base =
  [ ['i';'n';'t'];['c';'h';'a';'r'];['f';'l';'o';'a';'t'];
    ['s';'t';'r';'i';'n';'g'];['b';'o';'o';'l'] ] ;;
*)
let rec to_string_aux char_list =match char_list with
    []-> ""
  |c::l -> (string_of_char c) ^( to_string_aux l)
;;
  
let to_ty str= "[%ty :"^str^" ]";;



let parse_type string =
  let without_spaces = List.filter (fun c ->c <> ' ') in
  let char_list_ref = ref (List.rev (without_spaces (decomposition string 0))) in
  (*if (nbArgs (List.rev !char_list_ref)) < 1 then failwith "titi" ;*)
  let para_cpt =ref 0 in
  (*reverse char_list before using it *)
  let rec last_arg char_list acc= 
    match char_list with
      []->char_list_ref:=[];acc
    |elt :: l ->
        if elt = ')' then
          para_cpt:= !para_cpt + 1;
        if elt ='(' then
          para_cpt:= !para_cpt - 1;
        if elt='>' && !para_cpt=0 then
          match l with
            '-'::l2 -> char_list_ref:=l2;acc
          |_ -> failwith "toto"
        else
          last_arg l ( elt::acc )
  in

  
  let init_acc () =
    let arg1=last_arg (!char_list_ref ) [] in                               
    let arg2=last_arg (!char_list_ref)  [] in
    let ty1=to_ty (to_string_aux arg1) in 
    let ty2=to_ty (to_string_aux arg2) in
    "last_ty "^ty2^" "^ty1
  in 
  let acc =ref (init_acc ()) in
  while !char_list_ref <>[] do
    let arg=last_arg (!char_list_ref) [] in
    let ty= to_ty (to_string_aux arg) in
    acc:="arg_ty "^ty^" ("^(!acc)^")" ;
  done;
  !acc;;
    
  

(*parse_type (string : ex: int -> int) ==> (string : prot)*)

let question_typed question id_question = 
  let open Learnocaml_exercise_state in
  let acc="\n\nlet name"^id_question^" = \"" ^ question.name in
  let acc=acc ^ "\" ;; \nlet prot"^id_question^" = " ^ (parse_type question.ty) in
  let acc=(match question.type_question with
    | Suite -> acc ^ " ;;\nlet suite"^id_question^" =" ^ question.input ^ "  ;;\nlet question"^id_question^" =  TestSuite {name=name"^id_question^"; prot=prot"^id_question^"; suite=suite"^id_question^"}"
    | Solution -> acc ^ " ;;\nlet suite"^id_question^" =" ^ question.input ^ ";; \n let gen"^id_question^" =" ^ (string_of_int question.extra_alea) ^  " ;;\nlet question"^id_question^" = TestAgainstSol {name=name"^id_question^"; prot=prot"^id_question^"; gen=gen"^id_question^"; suite=suite"^id_question^"}"
    | Spec -> acc ^ ";;\nlet spec"^id_question^" =" ^ question.output ^ " ;; \n let suite"^id_question^" =" ^ question.input ^ ";; \n let gen"^id_question^" =" ^ string_of_int(question.extra_alea) ^ ";; \nlet question"^id_question^" = TestAgainstSpec {name=name"^id_question^"; prot=prot"^id_question^"; gen=gen"^id_question^"; suite=suite"^id_question^"; spec=spec"^id_question^"}") in
  acc;;

