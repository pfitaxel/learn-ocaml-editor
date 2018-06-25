module type TYPING = sig
  val ty_of : string -> 'a Ty.ty
end

module Make(Test_lib : Test_lib.S) (Typing : TYPING) = struct
open Test_lib
open Learnocaml_report

(* ty: (('ar -> 'row) Ty.ty, 'ar -> 'urow, 'ret) prot *)
(* sampler: (unit -> ('ar -> 'row, 'ar -> 'urow, 'ret) args) *)
(* suite: ('ar -> 'row, 'ar -> 'urow, 'ret) args list *)
(*     OU (('ar -> 'row, 'ar -> 'urow, 'ret) args * (unit -> 'ret)) list *)
(* spec : ('ar -> 'row, 'ar -> 'urow, 'ret) args -> 'ret -> report *)
type test_fun =
(*
  | TestAgainstSol of
      { name: string
      ; ty: string 
      ; gen: int
      ; suite: string
      }
  | TestAgainstSpec of
      { name: string
      ; ty: string
      ; gen: int
      ; suite: string
      ; spec : string
      } *)
  | TestSuite of
      { suite: string
      ; ty: string
      ; name: string
      }

type test_fun_typed =
  | TestSuiteTyped :
      { suite: ('ar -> 'row, 'ar -> 'urow, 'ret) args list
      ; ty: string
      ; name: string
      } -> test_fun_typed

end
