(* A small application that makes it easy to enter and track data to qualify
   for artist status in Belgium. It is a pro bono application designed at the
   request of "Les Amis d'ma mère", a Belgian non-profit organisation that
   promotes (and supports) artists in Belgium.

   Copyright (C) 2023  Funkywork

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

let date = testable Date.pp Date.equal
let day_of_week = testable Date.pp_day_of_week Date.equal_day_of_week
let result x = result x (testable Date.pp_error Date.equal_error)

let from_string_1 =
  test_case "make a valid date using [from_string]" `Quick (fun () ->
      let expected = Date.make ~year:2023 ~month:Date.Apr ~day:16 in
      let computed = Date.from_string "16/04/2023" in
      check (result date) "should be the same" expected computed;
      check (result day_of_week) "should be the same" (Ok Sun)
        Util.Result.(computed >|= Date.weekday_of))

let from_string_2 =
  test_case "make a valid date using [from_string]" `Quick (fun () ->
      let expected = Date.make ~year:2023 ~month:Date.Nov ~day:3 in
      let computed = Date.from_string "3/11/2023" in
      check (result date) "should be the same" expected computed;
      check (result day_of_week) "should be the same" (Ok Fri)
        Util.Result.(computed >|= Date.weekday_of))

let from_string_3 =
  test_case "make an invalid date using [from_string] because year is too small"
    `Quick (fun () ->
      let expected = Error (`Year_too_small 1968) in
      let computed = Date.from_string "01/05/1968" in
      check (result date) "should be the same" expected computed)

let from_string_4 =
  test_case "make an invalid date using [from_string] because year is too high"
    `Quick (fun () ->
      let expected = Error (`Year_too_big 3001) in
      let computed = Date.from_string "3/11/3001" in
      check (result date) "should be the same" expected computed)

let from_string_5 =
  test_case
    "make an invalid date using [from_string] because day is null or negative"
    `Quick (fun () ->
      let expected = Error (`Day_negative_or_null (-3)) in
      let computed = Date.from_string "-3/11/2010" in
      check (result date) "should be the same" expected computed)

let from_string_6 =
  test_case
    "make an invalid date using [from_string] because day is null or negative"
    `Quick (fun () ->
      let expected = Error (`Day_negative_or_null 0) in
      let computed = Date.from_string "000/11/2010" in
      check (result date) "should be the same" expected computed)

let from_string_7 =
  test_case "make an invalid date using [from_string] because month is invalid"
    `Quick (fun () ->
      let expected = Error (`Month_invalid 0) in
      let computed = Date.from_string "22/00/2010" in
      check (result date) "should be the same" expected computed)

let from_string_8 =
  test_case "make an invalid date using [from_string] because month is invalid"
    `Quick (fun () ->
      let expected = Error (`Month_invalid 23) in
      let computed = Date.from_string "22/23/2010" in
      check (result date) "should be the same" expected computed)

let from_string_9 =
  test_case "make an invalid date using [from_string] because string is invalid"
    `Quick (fun () ->
      let date_rep = "foo" in
      let expected = Error (`Date_invalid date_rep) in
      let computed = Date.from_string date_rep in
      check (result date) "should be the same" expected computed)

let from_string_10 =
  test_case "make an invalid date using [from_string] because string is invalid"
    `Quick (fun () ->
      let date_rep = "1/2/3/4" in
      let expected = Error (`Date_invalid date_rep) in
      let computed = Date.from_string date_rep in
      check (result date) "should be the same" expected computed)

let from_string_11 =
  test_case "make an invalid date using [from_string] because string is invalid"
    `Quick (fun () ->
      let date_rep = "foo/bar/2022" in
      let expected = Error (`Date_invalid date_rep) in
      let computed = Date.from_string date_rep in
      check (result date) "should be the same" expected computed)

let cases =
  ( "Date"
  , [
      from_string_1
    ; from_string_2
    ; from_string_3
    ; from_string_4
    ; from_string_5
    ; from_string_6
    ; from_string_7
    ; from_string_8
    ; from_string_9
    ; from_string_10
    ; from_string_11
    ] )
