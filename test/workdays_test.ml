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

let from_date_1 =
  test_case "produce a status from a date" `Quick (fun () ->
      let date_str = "04/12/2023" in
      let expected = workday date_str in
      let computed = Workdays.from_date (date date_str) in
      check status_testable "should be equal" expected computed)

let from_date_2 =
  test_case "produce a status from a date" `Quick (fun () ->
      let date_str = "22/04/2023" in
      let expected = weekend date_str Date.Sat in
      let computed = Workdays.from_date (date date_str) in
      check status_testable "should be equal" expected computed)

let from_date_3 =
  test_case "produce a status from a date" `Quick (fun () ->
      let date_str = "23/04/2023" in
      let expected = weekend date_str Date.Sun in
      let computed = Workdays.from_date (date date_str) in
      check status_testable "should be equal" expected computed)

let from_date_4 =
  test_case "produce a status from a date" `Quick (fun () ->
      let date_str = "01/01/2037" in
      let expected = leave date_str "Jour de l'an" in
      let computed = Workdays.from_date (date date_str) in
      check status_testable "should be equal" expected computed)

let from_date_5 =
  test_case "produce a status from a date" `Quick (fun () ->
      let date_str = "11/11/2042" in
      let expected = leave date_str "Armistice de 1918" in
      let computed = Workdays.from_date (date date_str) in
      check status_testable "should be equal" expected computed)

let from_date_6 =
  test_case "produce a status from a date" `Quick (fun () ->
      let date_str = "18/04/2022" in
      let expected = leave date_str "Lundi de Pâques" in
      let computed = Workdays.from_date (date date_str) in
      check status_testable "should be equal" expected computed)

let from_date_7 =
  test_case "produce a status from a date" `Quick (fun () ->
      let date_str = "06/05/2027" in
      let expected = leave date_str "Ascension" in
      let computed = Workdays.from_date (date date_str) in
      check status_testable "should be equal" expected computed)

let cases =
  ( "Workdays"
  , [
      from_date_1
    ; from_date_2
    ; from_date_3
    ; from_date_4
    ; from_date_5
    ; from_date_6
    ; from_date_7
    ] )
