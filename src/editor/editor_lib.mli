
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
val get_buffer :string -> Learnocaml_exercise_state.question_state
                             
val save_testhaut : Learnocaml_exercise_state.question_state Map.Make(String).t -> string -> unit  

val fetch_test_index: string -> Learnocaml_exercise_state.question_state Map.Make(String).t Lwt.t


val listFst : ('a * 'b) list -> 'a list
                              
val redondanceAux : 'a list -> 'a -> 'a list
                                        
val redondance : 'a list -> 'a list
                               
val decomposition : string -> int -> char list
                                          
val rechercheParenthese : char list -> int -> char list
                                                   
val nbArgs : char list -> int
                            
val test_fun : string -> string
                           
val testAlea : int -> string
                        
val typeFct : string -> string -> string

val librairie : string

val init : string

val get_id_question : string -> (Map.Make(String).key) list

val constructListeQuest :  (Map.Make(String).key) list -> string -> (string * string * int * string * bool) list

val sectionSol : string * string * int * string * 'a -> string

val constructSectionSol : (string * string * int * string * 'a) list -> string

val constructFinalSol : (string * string * int * string * 'a) list -> string
  
val string_of_char : char -> string

val concatenation : char list -> string

val supprRec : char list -> char list

val trouver_egal : char list -> char list

val trouver_nom : char list -> char list -> char list

val get_reste : char list -> char list

val suppr_let : char list -> char list

val get_let : char list -> char list

val get_args : char list -> int -> int

val get_fct : char list -> char list list -> char list list

val genQuestions : char list list -> (string * int) list -> (string * int) list

val gen_ty : int -> string
  
val failchar : char list

val tail : 'a list -> 'a list

val decompositionSol : string -> int -> char list

val commentaire : char list -> int -> char list

val premierLet : char list -> char list

val validationLet : char list -> bool

val rechercheEgal : char list -> int

val rechercheLet : char list -> bool -> char list

val decomposition2 : char list -> char list

val decompoFirst : char list -> char list

val genLet : char list -> char list

val genTemplate : string -> string
                              
