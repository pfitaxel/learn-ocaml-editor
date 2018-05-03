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

type history

type snapshot =
  { phrases : string list ;
    mtime : float }

val snapshot_enc : snapshot Json_encoding.encoding

val empty_snapshot : snapshot

val create:
  gettimeofday: (unit -> float) ->
  ?on_update: (history -> unit) ->
  ?max_size: int ->
  ?snapshot: snapshot ->
  unit -> history

val current : history -> string

val update : history -> string -> unit

val go_backward : history -> unit

val go_forward : history -> unit

val push : history -> unit

val discard : history -> unit

val snapshot : history -> snapshot
