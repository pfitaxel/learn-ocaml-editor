open Test_lib
open Report

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [ Section
      ([ Text "Function:" ; Code "rev" ],
       test_function_1_against_solution
         [%ty : int list -> int list ] "rev"
         [ []  ; [1;2;3] ]) ;
    Section
      ([ Text "Function:" ; Code "append" ],
       test_function_2_against_solution ~gen:0
         [%ty : int list -> int list -> int list ] "append"
         [ [],[]  ; [1;2;3],[4;5;6]; [1;2],[] ]) ]
     
