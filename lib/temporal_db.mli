(* A small application that makes it easy to enter and track data to qualify
   for artist status in Belgium. It is a pro bono application designed at the
   request of "Les Amis d'ma mère", a Belgian non-profit organisation that
   promotes (and supports) artists in Belgium.

   Copyright (C) 2023 Funkywork

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>. *)

(** A key value storage indexed by dates. *)

type 'a t
type 'a find_result = Found of Date.t * 'a | Out_of_bound of Date.t * 'a

(** {1 Creation} *)

val from_list : (Date.t * 'a) list -> 'a t

(** {1 Find operation} *)

val find : 'a t -> Date.t -> 'a option

val find_minimal_after :
  ?included:bool -> 'a t -> Date.t -> (Date.t * 'a) option

val find_maximal_after :
  ?included:bool -> 'a t -> Date.t -> (Date.t * 'a) option

val find_for : ?included:bool -> 'a t -> Date.t -> 'a find_result

(** {1 Util} *)

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val pp_find_result :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a find_result -> unit

val equal_find_result :
  ('a -> 'a -> bool) -> 'a find_result -> 'a find_result -> bool

val to_list : 'a t -> (Date.t * 'a) list
