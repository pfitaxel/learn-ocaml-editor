val get_titre : string -> string

val get_diff :string -> float option
val get_solution : string -> string
val get_question : string -> string
val get_template : string -> string 
val get_testml  : string -> string
val get_testhaut :string -> Learnocaml_exercise_state.question_state Map.Make(String).t                
val get_prelude :string -> string
val get_prepare :string -> string
val get_buffer :string -> Learnocaml_exercise_state.question_state
                             
val save_testhaut : Learnocaml_exercise_state.question_state Map.Make(String).t -> string -> unit  

val fetch_test_index: string -> Learnocaml_exercise_state.question_state Map.Make(String).t Lwt.t
