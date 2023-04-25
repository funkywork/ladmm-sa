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

(** Allows the calculation of potential working days via an approximation. *)

(** {1 Status of a day}

    Builds an approximation of the type of day (weekend, holiday or working day)
    based on the databases defined in [Config]. *)

type status =
  | Workday of Date.t
  | Weekend of Date.t * Date.day_of_week
  | Reason of Date.t * string

val from_date : Date.t -> status
val pp_status : Format.formatter -> status -> unit
val equal_status : status -> status -> bool

(** {1 Diagnosis}

    Builds an approximation of a diagnostis of a Date range. *)

type diagnosis = {
    workdays : int
  ; saturdays : int
  ; sundays : int
  ; leaves : (Date.t * string) list
}

val from_range : Date.t -> Date.t -> diagnosis
