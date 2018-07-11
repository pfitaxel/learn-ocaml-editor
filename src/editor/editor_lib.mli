
(*   getters of an editor exercise the argument is the id *) 
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
val get_buffer : string -> string

(*getters of a question of and editor exercise arguments : exercise_id question_id *)
val get_ty : string -> Map.Make(String).key -> string
val get_name_question : string -> Map.Make(String).key -> string
val get_type_question : string -> Map.Make(String).key -> Learnocaml_exercise_state.type_question
val get_extra_alea : string -> Map.Make(String).key -> int
val get_input : string -> Map.Make(String).key -> string
val get_spec : string -> Map.Make(String).key -> string
                                            
(* the question ids are integers stocked in strings 
this function computes the smallest integer not used yet *)                             
val compute_question_id : 'a Map.Make(String).t -> string

(* setter of testhaut arguments: new StringMap exercise_id *)  
val save_testhaut : Learnocaml_exercise_state.test_qst_untyped Map.Make(String).t -> string -> unit

(* remove an exercise from the local storage *)
val remove_exo : Map.Make(String).key -> unit

(* functions returning a bool depending on whether the title/id is already  used or not*)
val titleUnique : string -> bool
val idUnique : string -> bool
  
(*store an exercise in the dynamic index of editor exercises *)
val store_in_index : Learnocaml_exercise_state.metadata -> unit

(* arguments Dom element , string *)
val setInnerHtml : < innerHTML : < set : Js.js_string Js.t -> unit; .. >
                                                                      Js_of_ocaml.Js.gen_prop; .. > Js_of_ocaml.Js.t -> string -> unit

(*trick to call the recovering function outside of it definition enveroniment *)
val recovering_callback : (unit -> unit) ref

(* creates the testhaut pane blindfolds *)
val testhaut_init : [< Html_types.div ] Tyxml_js.Html5.elt -> string -> unit Lwt.t

(* removes extra_copies of a value in the list (each value of the list is unique now)  *) 
val redondance : 'a list -> 'a list

(* creates the corresponding char list of a string (second parameter must be 0 ) *)
val decomposition : string -> int -> char list

(* fragment of a test.ml code ,see definition*)
val init : string

(*creates the code of 1 Section, arguments : name of the function , report associated *)
val section : string -> string -> string
  
val string_of_char : char -> string
  
(* arguments : content of the toplevel, [[]],  returns a list . The first value is the type of the first val etc. *)
val get_all_val : char list -> char list list -> char list list

(* ça fait ce que ça dit *)
val get_only_fct : char list -> char list -> char list
val get_questions : char list list -> (string * string) list -> (string * string) list
val decompositionSol : string -> int -> char list
val polymorph_detector : ('a * string) list -> ('a * string) list

val genTemplate : string -> string

(*refacoring of typecheck functions *)
val typecheck : bool -> 'a Ace.editor -> Ocaml_mode.editor -> Learnocaml_toplevel.t -> unit Lwt.t
val typecheck_spec : bool -> 'a Ace.editor -> Ocaml_mode.editor -> Learnocaml_toplevel.t -> unit Lwt.t


(*creates an exercise with the data of the local storage argument: editor_exercise_id *)
val exo_creator : string -> Learnocaml_exercise.t

(*returns the output of toplevel buffer *) 
val get_answer : Learnocaml_toplevel.t -> string


