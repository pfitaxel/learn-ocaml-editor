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

let example_constr_sol =
  TestAgainstSol
    { name = "opp";
      prot = (last_ty [%ty: int] [%ty: int]);
      gen = 0;
      suite = [!! 0; !! 1; !! 2; !! ~-1]
    }

let example_constr_spec =
  TestAgainstSpec
    { name = "idempotent";
      prot = (last_ty [%ty: int] [%ty: int]);
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
       Message ([ Text "Checking spec for" ; Code code ], Informative) ::
         let ret = apply uf args in
         let value = to_string ret_ty ret in
       let (text, note) = match t.spec uf args ret with
         | Correct None -> ("Correct spec", Success 1)
         | Correct (Some message) -> (message, Success 1)
         | Wrong None -> ("Wrong spec", Failure)
         | Wrong (Some message) -> (message, Failure) in
       [Message ([Text "Got value"; Code value; Text (": " ^ text)], note)])
     t.suite
  | TestSuite t ->
     test_function
       ~test:test (* could take into account exceptions/sorted lists/etc. *)
       t.prot
       (lookup_student (ty_of_prot t.prot) t.name)
       t.suite

end
