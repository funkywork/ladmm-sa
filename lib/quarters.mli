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

(** Defines a list of 8 quarters (used to calculate the different days in
    stages). *)

type t

(** {1 Construction} *)

val init : Date.t -> t

(** {1 Util} *)

val to_representable_list : t -> (int * Date.t * Date.t) list
val get_by_date : t -> Date.t -> (int, [> Sigs.quarters_error ]) result
val get : t -> int -> Date.t * Date.t

val get_by_range :
  t -> Date.t -> Date.t -> (int, [> Sigs.quarters_error ]) result

val get_name : t -> string
val get_interval : t -> Date.t * Date.t
val first_date : t -> Date.t
val compare : t -> t -> int

(** {1 Error util} *)

val pp_error : Format.formatter -> [> Sigs.quarters_error ] -> unit
val equal_error : [> Sigs.quarters_error ] -> [> Sigs.quarters_error ] -> bool
