(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2016 OCamlPro.
 *
 * Learn-OCaml is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * Learn-OCaml is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>. *)

type exercise_state =
  { solution : string ;
    grade : int (* \in [0, 100] *) option ;
    report : Learnocaml_report.report option ;
    mtime : float }

val exercise_state_enc : exercise_state Json_encoding.encoding

type type_question= Suite | Solution | Spec ;;

type question_state =
  {name :string;
   ty :string;
   type_question : type_question;
   input :string;
   output:string;
   extra_alea:int;   
  }

type test_state = {testml : string;
                   testhaut : question_state Map.Make (String).t}

val testhaut_enc : question_state Map.Make (String).t Json_encoding.encoding
    
type metadata =
  { id :string;
    titre :string;
    description :string;
    diff :float
  }


type editor_state =
  { metadata :metadata;    
    prepare :string;
    solution : string ;
    question : string ;
    template : string ;
    test : test_state ;
    prelude : string;    
    mtime : float }


val editor_state_enc : editor_state Json_encoding.encoding
open Learnocaml_index
type index_state =
  {
     exos : Learnocaml_index.exercise Map.Make(String).t;
     mtime : float;
  }
  

val index_state_enc : index_state Json_encoding.encoding
