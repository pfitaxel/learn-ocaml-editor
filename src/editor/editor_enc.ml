type editor_state =
  { id : string ;
    titre : string;
    description : string ;
    diff : float ;
    solution : string ;
    question : string ;
    template : string ;
    test : string ;
    report : Learnocaml_report.report option ;    
    mtime : float }

open Json_encoding

let editor_state_enc =
  
  conv
    (fun {id ; titre ; description; diff;solution ; question ;template ; test; report ; mtime } ->
       (id , titre , description, diff, solution , question , template , test, report , mtime))
    (fun (id , titre , description, diff, solution , question , template , test, report , mtime) ->
       {id ; titre ; description; diff;solution ; question ;template ; test; report ; mtime })
    (obj10
       (req "id" string)
       (req "titre" string)
       (req "description" string)
       (req "diff" float )
       (req "solution" string)
       (req "question" string)
       (req "template" string)
       (req "test" string)
       (opt "report" Learnocaml_report.report_enc)
       (dft "mtime" float 0.))

