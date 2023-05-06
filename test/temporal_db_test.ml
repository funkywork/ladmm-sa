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

open Alcotest
open Ladmm_lib
open Test_util

let find_for_1 =
  test_case
    "when there is a temporal point it should return it wrapped into a founded"
    `Quick (fun () ->
      let given_date = date "22/04/2022" in
      let expected =
        Temporal_db.Found (date "01/04/2022", Num.from_float 69.47)
      in
      let computed =
        Temporal_db.find_for Config.daily_reference_salary given_date
      in
      check
        (find_result_testable num_testable)
        "should be equal" expected computed)

let find_for_2 =
  test_case
    "when there is a temporal point it should return it wrapped into a founded"
    `Quick (fun () ->
      let given_date = date "28/11/2026" in
      let expected =
        Temporal_db.Found (date "01/12/2022", Num.from_float 75.19)
      in
      let computed =
        Temporal_db.find_for Config.daily_reference_salary given_date
      in
      check
        (find_result_testable num_testable)
        "should be equal" expected computed)

let find_for_3 =
  test_case
    "when there is no temporal point it should return it wrapped into a \
     Out_ouf_bound"
    `Quick (fun () ->
      let given_date = date "28/12/2019" in
      let expected =
        Temporal_db.Out_of_bound (date "01/03/2020", Num.from_float 62.53)
      in
      let computed =
        Temporal_db.find_for Config.daily_reference_salary given_date
      in
      check
        (find_result_testable num_testable)
        "should be equal" expected computed)

let cases = ("Temporal_db", [ find_for_1; find_for_2; find_for_3 ])
