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

(** Some test helpers *)

open Ladmm_lib

exception Invalid_date

val get_date : (Date.t, [> Sigs.date_error ]) result -> Date.t
val date_testable : Date.t Alcotest.testable
val day_of_week_testable : Date.day_of_week Alcotest.testable

val date_result_testable :
  'a Alcotest.testable -> ('a, [> Sigs.date_error ]) result Alcotest.testable

val find_result_testable :
  'a Alcotest.testable -> 'a Temporal_db.find_result Alcotest.testable

val num_testable : Num.t Alcotest.testable
val status_testable : Workdays.status Alcotest.testable

val date_range :
  string -> string -> (Date.t * Date.t, [> Sigs.date_error ]) result

val date : string -> Date.t
val workday : string -> Workdays.status
val weekend : string -> Date.day_of_week -> Workdays.status
val leave : string -> string -> Workdays.status

val quarters_result_testable :
     'a Alcotest.testable
  -> ('a, [> Sigs.quarters_error ]) result Alcotest.testable

val check_quarters_by_date :
     Quarters.t
  -> string
  -> (int, [> Sigs.quarters_error ]) result
  -> Alcotest.return

val check_quarters_by_range :
     Quarters.t
  -> string
  -> string
  -> (int, [> Sigs.quarters_error ]) result
  -> Alcotest.return
