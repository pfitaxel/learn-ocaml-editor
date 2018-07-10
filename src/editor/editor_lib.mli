

val get_titre : string -> string
val get_description : string -> string
val get_diff : string -> float
val get_solution : string -> string
val get_question : string -> string
val get_template : string -> string
val get_testml : string -> string
val get_testhaut : string -> Learnocaml_exercise_state.test_qst_untyped Map.Make(String).t
val get_prelude : string -> string
val get_prepare : string -> string
val get_imperative : string -> bool
val get_undesirable : string -> bool
val get_test_liste : string -> Learnocaml_exercise_state.test_qst_untyped Map.Make(String).t
val get_test_string : string -> string

val get_ty : string -> Map.Make(String).key -> string
val get_name_question : string -> Map.Make(String).key -> string
val get_type_question : string -> Map.Make(String).key -> Learnocaml_exercise_state.type_question
val get_extra_alea : string -> Map.Make(String).key -> int
val get_input : string -> Map.Make(String).key -> string
val get_spec : string -> Map.Make(String).key -> string
                                            
val get_buffer : string -> string
                             
val compute_question_id : 'a Map.Make(String).t -> string
val save_testhaut : Learnocaml_exercise_state.test_qst_untyped Map.Make(String).t -> string -> unit
val remove_exo : Map.Make(String).key -> unit                     
val titleUnique : string -> bool
val idUnique : string -> bool                             
val store_in_index : Learnocaml_exercise_state.metadata -> unit
val setInnerHtml : < innerHTML : < set : Js.js_string Js.t -> unit; .. >
                                                                      Js_of_ocaml.Js.gen_prop; .. > Js_of_ocaml.Js.t -> string -> unit
val recovering_callback : (unit -> unit) ref
val testhaut_init : [< Html_types.div ] Tyxml_js.Html5.elt -> string -> unit Lwt.t

val redondance : 'a list -> 'a list
val decomposition : string -> int -> char list
val init : string                                          
val section : string -> string -> string
val string_of_char : char -> string
val get_all_val : char list -> char list list -> char list list
val get_only_fct : char list -> char list -> char list
val get_questions : char list list -> (string * string) list -> (string * string) list
val decompositionSol : string -> int -> char list
val polymorph_detector : ('a * string) list -> ('a * string) list

val genTemplate : string -> string

val typecheck : bool -> 'a Ace.editor -> Ocaml_mode.editor -> Learnocaml_toplevel.t -> unit Lwt.t

val exo_creator : string -> Learnocaml_exercise.t
val get_answer : Learnocaml_toplevel.t -> string
