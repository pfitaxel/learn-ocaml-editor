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
  | Right of string option
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
      ; spec : ('ar -> 'row, 'ar -> 'urow, 'ret) args -> 'ret -> outcome } -> test_qst_typed
  | TestSuite :
      { suite: (('ar -> 'row, 'ar -> 'urow, 'ret) args * (unit -> 'ret)) list
      ; prot: (('ar -> 'row) Ty.ty, 'ar -> 'urow, 'ret) prot
      ; name: string } -> test_qst_typed

let example_constr_sol =
  TestAgainstSol
    { name = "opp";
      prot = (last_ty [%ty: int] [%ty: int]);
      gen = 0;
      suite = [last 0; last 1; last 2; last ~-1]
    }

let example_constr_spec =
  TestAgainstSpec
    { name = "idem";
      prot = (last_ty [%ty: int] [%ty: int]);
      gen = 0;
      suite = [last 0; last 1; last 2; last ~-1];
      spec = fun args (res : int) -> (* might be simplified *)
      let nth0 = apply (fun n -> n) in
      let arg = nth0 args in
      if res = 2 * arg then Right None else Wrong None
    }

(*
let example_constr_suite =
  TestSuite
    {

    }
 *)

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
     let after = fun args (va, _, _) (_, _, _) ->
       let (text, note) = match t.spec args va with
         | Right None -> ("Correct spec", Success 1)
         | Right (Some message) -> (message, Success 1)
         | Wrong None -> ("Wrong spec", Failure)
         | Wrong (Some message) -> (message, Failure) in
       Learnocaml_report.[ Message ([ Text text ;
           Code (to_string (get_ret_ty (ty_of_prot t.prot) args) va) ], note) ] in
     let stud = lookup_student (ty_of_prot t.prot) t.name in
     test_function_against
       ~gen:0 ~sampler:local_dummy
       ~test:(fun _ _ _ -> [])
       ~after:after
       t.prot
       stud
       stud
       t.suite
  | TestSuite t ->
     test_function
       ~test:test (* could take into account exceptions/sorted lists/etc. *)
       t.prot
       (lookup_student (ty_of_prot t.prot) t.name)
       t.suite

end
