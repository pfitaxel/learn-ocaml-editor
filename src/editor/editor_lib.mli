
val get_titre : string -> string

val get_diff :string -> float option
val get_solution : string -> string
val get_question : string -> string
val get_template : string -> string 
val get_testml  : string -> string
val get_testhaut :string -> Learnocaml_exercise_state.question_state Map.Make(String).t                
val get_prelude :string -> string
val get_prepare :string -> string

val get_test_liste : string -> Learnocaml_exercise_state.question_state Map.Make(String).t
val get_test_string : string -> string                             
val get_ty : string -> Map.Make(String).key -> string
val get_name_question : string -> Map.Make(String).key -> string
val get_type_question : string -> Map.Make(String).key -> Learnocaml_exercise_state.type_question
val get_extra_alea :string -> Map.Make(String).key -> int
val get_input : string -> Map.Make(String).key -> string
val get_output : string -> Map.Make(String).key -> string                                                 
                             
val save_testhaut : Learnocaml_exercise_state.question_state Map.Make(String).t -> string -> unit  

val fetch_test_index: string -> Learnocaml_exercise_state.question_state Map.Make(String).t Lwt.t
