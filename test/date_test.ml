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

let from_string_1 =
  test_case "make a valid date using [from_string]" `Quick (fun () ->
      let expected = Date.make ~year:2023 ~month:Date.Apr ~day:16 in
      let computed = Date.from_string "16/04/2023" in
      check
        (date_result_testable date_testable)
        "should be the same" expected computed;
      check
        (date_result_testable day_of_week_testable)
        "should be the same" (Ok Sun)
        Util.Result.(computed >|= Date.weekday_of))

let from_string_2 =
  test_case "make a valid date using [from_string]" `Quick (fun () ->
      let expected = Date.make ~year:2023 ~month:Date.Nov ~day:3 in
      let computed = Date.from_string "3/11/2023" in
      check
        (date_result_testable date_testable)
        "should be the same" expected computed;
      check
        (date_result_testable day_of_week_testable)
        "should be the same" (Ok Fri)
        Util.Result.(computed >|= Date.weekday_of))

let from_string_3 =
  test_case "make an invalid date using [from_string] because year is too small"
    `Quick (fun () ->
      let expected = Error (`Year_too_small 1968) in
      let computed = Date.from_string "01/05/1968" in
      check
        (date_result_testable date_testable)
        "should be the same" expected computed)

let from_string_4 =
  test_case "make an invalid date using [from_string] because year is too high"
    `Quick (fun () ->
      let expected = Error (`Year_too_big 3001) in
      let computed = Date.from_string "3/11/3001" in
      check
        (date_result_testable date_testable)
        "should be the same" expected computed)

let from_string_5 =
  test_case
    "make an invalid date using [from_string] because day is null or negative"
    `Quick (fun () ->
      let expected = Error (`Day_negative_or_null (-3)) in
      let computed = Date.from_string "-3/11/2010" in
      check
        (date_result_testable date_testable)
        "should be the same" expected computed)

let from_string_6 =
  test_case
    "make an invalid date using [from_string] because day is null or negative"
    `Quick (fun () ->
      let expected = Error (`Day_negative_or_null 0) in
      let computed = Date.from_string "000/11/2010" in
      check
        (date_result_testable date_testable)
        "should be the same" expected computed)

let from_string_7 =
  test_case "make an invalid date using [from_string] because month is invalid"
    `Quick (fun () ->
      let expected = Error (`Month_invalid 0) in
      let computed = Date.from_string "22/00/2010" in
      check
        (date_result_testable date_testable)
        "should be the same" expected computed)

let from_string_8 =
  test_case "make an invalid date using [from_string] because month is invalid"
    `Quick (fun () ->
      let expected = Error (`Month_invalid 23) in
      let computed = Date.from_string "22/23/2010" in
      check
        (date_result_testable date_testable)
        "should be the same" expected computed)

let from_string_9 =
  test_case "make an invalid date using [from_string] because string is invalid"
    `Quick (fun () ->
      let date_rep = "foo" in
      let expected = Error (`Date_invalid date_rep) in
      let computed = Date.from_string date_rep in
      check
        (date_result_testable date_testable)
        "should be the same" expected computed)

let from_string_10 =
  test_case "make an invalid date using [from_string] because string is invalid"
    `Quick (fun () ->
      let date_rep = "1/2/3/4" in
      let expected = Error (`Date_invalid date_rep) in
      let computed = Date.from_string date_rep in
      check
        (date_result_testable date_testable)
        "should be the same" expected computed)

let from_string_11 =
  test_case "make an invalid date using [from_string] because string is invalid"
    `Quick (fun () ->
      let date_rep = "foo/bar/2022" in
      let expected = Error (`Date_invalid date_rep) in
      let computed = Date.from_string date_rep in
      check
        (date_result_testable date_testable)
        "should be the same" expected computed)

let first_day_of_month_1 =
  test_case "make a valid date and move to the first day of the month" `Quick
    (fun () ->
      let expected = Date.from_string "01/04/2023" in
      let computed =
        Util.Result.(Date.(from_string "18/04/2023" >|= first_day_of_month))
      in
      check
        (date_result_testable date_testable)
        "should be the same" expected computed;
      check
        (date_result_testable day_of_week_testable)
        "should be the same" (Ok Sat)
        Util.Result.(computed >|= Date.weekday_of))

let last_day_of_month_1 =
  test_case "make a valid date and move to the last day of the month" `Quick
    (fun () ->
      let expected = Date.from_string "30/04/2023" in
      let computed =
        Util.Result.(Date.(from_string "18/04/2023" >|= last_day_of_month))
      in
      check
        (date_result_testable date_testable)
        "should be the same" expected computed;
      check
        (date_result_testable day_of_week_testable)
        "should be the same" (Ok Sun)
        Util.Result.(computed >|= Date.weekday_of))

let month_range_1 =
  test_case "make a valid date and compute the map range" `Quick (fun () ->
      let open Util.Result in
      let expected =
        let* a = Date.from_string "01/04/2023" in
        let+ b = Date.from_string "30/04/2023" in
        (a, b)
      in

      let computed = Date.(from_string "18/04/2023" >|= month_range) in
      let expected_dow = Ok Date.(Sat, Sun) in
      let computed_dow =
        let+ a, b = computed in
        (Date.weekday_of a, Date.weekday_of b)
      in
      check
        (date_result_testable @@ pair date_testable date_testable)
        "should be the same" expected computed;
      check
        (date_result_testable @@ pair day_of_week_testable day_of_week_testable)
        "should be the same" expected_dow computed_dow)

let month_range_2 =
  test_case "make a valid date and compute the map range" `Quick (fun () ->
      let open Util.Result in
      let expected =
        let* a = Date.from_string "01/02/2023" in
        let+ b = Date.from_string "28/02/2023" in
        (a, b)
      in

      let computed = Date.(from_string "7/2/2023" >|= month_range) in
      let expected_dow = Ok Date.(Wed, Tue) in
      let computed_dow =
        let+ a, b = computed in
        (Date.weekday_of a, Date.weekday_of b)
      in
      check
        (date_result_testable @@ pair date_testable date_testable)
        "should be the same" expected computed;
      check
        (date_result_testable @@ pair day_of_week_testable day_of_week_testable)
        "should be the same" expected_dow computed_dow)

let month_range_3 =
  test_case "make a valid date and compute the map range" `Quick (fun () ->
      let open Util.Result in
      let expected =
        let* a = Date.from_string "01/02/2032" in
        let+ b = Date.from_string "29/02/2032" in
        (a, b)
      in

      let computed = Date.(from_string "12/2/2032" >|= month_range) in
      let expected_dow = Ok Date.(Sun, Sun) in
      let computed_dow =
        let+ a, b = computed in
        (Date.weekday_of a, Date.weekday_of b)
      in
      check
        (date_result_testable @@ pair date_testable date_testable)
        "should be the same" expected computed;
      check
        (date_result_testable @@ pair day_of_week_testable day_of_week_testable)
        "should be the same" expected_dow computed_dow)

let check_matrix source index a b =
  let open Util.Result in
  let computed = source >|= fun source -> Array.get source index in
  let expected = date_range a b in
  check
    (date_result_testable @@ pair date_testable date_testable)
    "should be the same" expected computed

let quarter_1 =
  test_case "make a quarter matrix" `Quick (fun () ->
      let open Util.Result in
      let computed =
        let+ date = Date.from_string "26/02/2023" in
        Date.quarters date 8
      in
      check_matrix computed 0 "1/2/2023" "30/4/2023";
      check_matrix computed 1 "1/5/2023" "31/7/2023";
      check_matrix computed 2 "1/8/2023" "31/10/2023";
      check_matrix computed 3 "1/11/2023" "31/1/2024";
      check_matrix computed 4 "1/2/2024" "30/4/2024";
      check_matrix computed 5 "1/5/2024" "31/7/2024";
      check_matrix computed 6 "1/8/2024" "31/10/2024";
      check_matrix computed 7 "1/11/2024" "31/1/2025")

let quarter_2 =
  test_case "make a quarter matrix" `Quick (fun () ->
      let open Util.Result in
      let computed =
        let+ date = Date.from_string "24/10/2054" in
        Date.quarters date 8
      in
      check_matrix computed 0 "01/10/2054" "31/12/2054";
      check_matrix computed 1 "01/01/2055" "31/03/2055";
      check_matrix computed 2 "01/04/2055" "30/06/2055";
      check_matrix computed 3 "01/07/2055" "30/09/2055";
      check_matrix computed 4 "01/10/2055" "31/12/2055";
      check_matrix computed 5 "01/01/2056" "31/03/2056";
      check_matrix computed 6 "01/04/2056" "30/06/2056";
      check_matrix computed 7 "01/07/2056" "30/09/2056")

let next_1 =
  test_case "compute next days from date" `Quick (fun () ->
      let expected =
        [
          "24/10/2023"
        ; "01/11/1986"
        ; "06/05/1999"
        ; "29/02/2024"
        ; "01/03/2023"
        ; "01/01/2000"
        ]
        |> List.map (fun x -> date x)
      in
      let computed =
        [
          "23/10/2023"
        ; "31/10/1986"
        ; "05/05/1999"
        ; "28/02/2024"
        ; "28/02/2023"
        ; "31/12/1999"
        ]
        |> List.map (fun x -> Date.next (date x))
      in
      check (list date_testable) "should be equal" expected computed)

let prev_1 =
  test_case "compute previous days from date" `Quick (fun () ->
      let expected =
        [
          "22/10/2023"
        ; "31/10/1986"
        ; "04/05/1999"
        ; "28/02/2024"
        ; "28/02/2023"
        ; "31/12/1999"
        ]
        |> List.map (fun x -> date x)
      in
      let computed =
        [
          "23/10/2023"
        ; "01/11/1986"
        ; "05/05/1999"
        ; "29/02/2024"
        ; "01/03/2023"
        ; "01/01/2000"
        ]
        |> List.map (fun x -> Date.prev (date x))
      in
      check (list date_testable) "should be equal" expected computed)

let unfold_1 =
  test_case "unfold date range" `Quick (fun () ->
      let expected =
        [
          "12/01/2024"
        ; "13/01/2024"
        ; "14/01/2024"
        ; "15/01/2024"
        ; "16/01/2024"
        ; "17/01/2024"
        ; "18/01/2024"
        ; "19/01/2024"
        ; "20/01/2024"
        ; "21/01/2024"
        ; "22/01/2024"
        ; "23/01/2024"
        ; "24/01/2024"
        ; "25/01/2024"
        ; "26/01/2024"
        ; "27/01/2024"
        ; "28/01/2024"
        ; "29/01/2024"
        ; "30/01/2024"
        ; "31/01/2024"
        ; "01/02/2024"
        ; "02/02/2024"
        ; "03/02/2024"
        ; "04/02/2024"
        ; "05/02/2024"
        ; "06/02/2024"
        ; "07/02/2024"
        ; "08/02/2024"
        ; "09/02/2024"
        ; "10/02/2024"
        ; "11/02/2024"
        ; "12/02/2024"
        ; "13/02/2024"
        ; "14/02/2024"
        ; "15/02/2024"
        ; "16/02/2024"
        ; "17/02/2024"
        ; "18/02/2024"
        ; "19/02/2024"
        ; "20/02/2024"
        ; "21/02/2024"
        ; "22/02/2024"
        ; "23/02/2024"
        ; "24/02/2024"
        ; "25/02/2024"
        ; "26/02/2024"
        ; "27/02/2024"
        ; "28/02/2024"
        ; "29/02/2024"
        ; "01/03/2024"
        ; "02/03/2024"
        ]
        |> List.map date
      in
      let computed = Date.unfold (date "12/01/2024") (date "02/03/2024") in
      check (list date_testable) "should be equal" expected computed)

let unfold_2 =
  test_case "unfold date range" `Quick (fun () ->
      let expected =
        [
          "10/10/2022"
        ; "11/10/2022"
        ; "12/10/2022"
        ; "13/10/2022"
        ; "14/10/2022"
        ; "15/10/2022"
        ; "16/10/2022"
        ; "17/10/2022"
        ; "18/10/2022"
        ; "19/10/2022"
        ; "20/10/2022"
        ; "21/10/2022"
        ; "22/10/2022"
        ; "23/10/2022"
        ; "24/10/2022"
        ; "25/10/2022"
        ; "26/10/2022"
        ; "27/10/2022"
        ; "28/10/2022"
        ; "29/10/2022"
        ; "30/10/2022"
        ; "31/10/2022"
        ; "01/11/2022"
        ; "02/11/2022"
        ; "03/11/2022"
        ; "04/11/2022"
        ; "05/11/2022"
        ; "06/11/2022"
        ; "07/11/2022"
        ; "08/11/2022"
        ; "09/11/2022"
        ; "10/11/2022"
        ; "11/11/2022"
        ; "12/11/2022"
        ; "13/11/2022"
        ; "14/11/2022"
        ; "15/11/2022"
        ; "16/11/2022"
        ; "17/11/2022"
        ; "18/11/2022"
        ; "19/11/2022"
        ; "20/11/2022"
        ; "21/11/2022"
        ; "22/11/2022"
        ; "23/11/2022"
        ; "24/11/2022"
        ; "25/11/2022"
        ; "26/11/2022"
        ; "27/11/2022"
        ; "28/11/2022"
        ; "29/11/2022"
        ; "30/11/2022"
        ; "01/12/2022"
        ; "02/12/2022"
        ; "03/12/2022"
        ; "04/12/2022"
        ; "05/12/2022"
        ; "06/12/2022"
        ; "07/12/2022"
        ; "08/12/2022"
        ; "09/12/2022"
        ; "10/12/2022"
        ; "11/12/2022"
        ; "12/12/2022"
        ; "13/12/2022"
        ; "14/12/2022"
        ; "15/12/2022"
        ; "16/12/2022"
        ; "17/12/2022"
        ; "18/12/2022"
        ; "19/12/2022"
        ; "20/12/2022"
        ; "21/12/2022"
        ; "22/12/2022"
        ; "23/12/2022"
        ; "24/12/2022"
        ; "25/12/2022"
        ; "26/12/2022"
        ; "27/12/2022"
        ; "28/12/2022"
        ; "29/12/2022"
        ; "30/12/2022"
        ; "31/12/2022"
        ; "01/01/2023"
        ; "02/01/2023"
        ; "03/01/2023"
        ; "04/01/2023"
        ; "05/01/2023"
        ; "06/01/2023"
        ; "07/01/2023"
        ; "08/01/2023"
        ; "09/01/2023"
        ; "10/01/2023"
        ; "11/01/2023"
        ; "12/01/2023"
        ; "13/01/2023"
        ; "14/01/2023"
        ; "15/01/2023"
        ; "16/01/2023"
        ; "17/01/2023"
        ; "18/01/2023"
        ; "19/01/2023"
        ; "20/01/2023"
        ; "21/01/2023"
        ; "22/01/2023"
        ; "23/01/2023"
        ; "24/01/2023"
        ; "25/01/2023"
        ; "26/01/2023"
        ; "27/01/2023"
        ; "28/01/2023"
        ; "29/01/2023"
        ; "30/01/2023"
        ; "31/01/2023"
        ; "01/02/2023"
        ; "02/02/2023"
        ; "03/02/2023"
        ; "04/02/2023"
        ; "05/02/2023"
        ; "06/02/2023"
        ; "07/02/2023"
        ; "08/02/2023"
        ; "09/02/2023"
        ; "10/02/2023"
        ; "11/02/2023"
        ; "12/02/2023"
        ; "13/02/2023"
        ; "14/02/2023"
        ; "15/02/2023"
        ; "16/02/2023"
        ; "17/02/2023"
        ; "18/02/2023"
        ; "19/02/2023"
        ; "20/02/2023"
        ; "21/02/2023"
        ; "22/02/2023"
        ; "23/02/2023"
        ; "24/02/2023"
        ; "25/02/2023"
        ; "26/02/2023"
        ; "27/02/2023"
        ; "28/02/2023"
        ; "01/03/2023"
        ; "02/03/2023"
        ; "03/03/2023"
        ; "04/03/2023"
        ; "05/03/2023"
        ; "06/03/2023"
        ; "07/03/2023"
        ; "08/03/2023"
        ; "09/03/2023"
        ; "10/03/2023"
        ; "11/03/2023"
        ; "12/03/2023"
        ; "13/03/2023"
        ; "14/03/2023"
        ; "15/03/2023"
        ; "16/03/2023"
        ; "17/03/2023"
        ; "18/03/2023"
        ; "19/03/2023"
        ; "20/03/2023"
        ; "21/03/2023"
        ; "22/03/2023"
        ; "23/03/2023"
        ; "24/03/2023"
        ; "25/03/2023"
        ; "26/03/2023"
        ; "27/03/2023"
        ; "28/03/2023"
        ; "29/03/2023"
        ; "30/03/2023"
        ; "31/03/2023"
        ; "01/04/2023"
        ; "02/04/2023"
        ; "03/04/2023"
        ; "04/04/2023"
        ; "05/04/2023"
        ; "06/04/2023"
        ; "07/04/2023"
        ; "08/04/2023"
        ; "09/04/2023"
        ; "10/04/2023"
        ; "11/04/2023"
        ; "12/04/2023"
        ; "13/04/2023"
        ; "14/04/2023"
        ; "15/04/2023"
        ; "16/04/2023"
        ; "17/04/2023"
        ; "18/04/2023"
        ; "19/04/2023"
        ; "20/04/2023"
        ; "21/04/2023"
        ; "22/04/2023"
        ; "23/04/2023"
        ; "24/04/2023"
        ; "25/04/2023"
        ; "26/04/2023"
        ; "27/04/2023"
        ; "28/04/2023"
        ; "29/04/2023"
        ; "30/04/2023"
        ; "01/05/2023"
        ; "02/05/2023"
        ; "03/05/2023"
        ; "04/05/2023"
        ; "05/05/2023"
        ]
        |> List.map date
      in
      let computed = Date.unfold (date "10/10/2022") (date "05/05/2023") in
      check (list date_testable) "should be equal" expected computed)

let rev_unfold_1 =
  test_case "rev_unfold date range" `Quick (fun () ->
      let expected =
        [
          "10/10/2022"
        ; "11/10/2022"
        ; "12/10/2022"
        ; "13/10/2022"
        ; "14/10/2022"
        ; "15/10/2022"
        ; "16/10/2022"
        ; "17/10/2022"
        ; "18/10/2022"
        ; "19/10/2022"
        ; "20/10/2022"
        ; "21/10/2022"
        ; "22/10/2022"
        ; "23/10/2022"
        ; "24/10/2022"
        ; "25/10/2022"
        ; "26/10/2022"
        ; "27/10/2022"
        ; "28/10/2022"
        ; "29/10/2022"
        ; "30/10/2022"
        ; "31/10/2022"
        ; "01/11/2022"
        ; "02/11/2022"
        ; "03/11/2022"
        ; "04/11/2022"
        ; "05/11/2022"
        ; "06/11/2022"
        ; "07/11/2022"
        ; "08/11/2022"
        ; "09/11/2022"
        ; "10/11/2022"
        ; "11/11/2022"
        ; "12/11/2022"
        ; "13/11/2022"
        ; "14/11/2022"
        ; "15/11/2022"
        ; "16/11/2022"
        ; "17/11/2022"
        ; "18/11/2022"
        ; "19/11/2022"
        ; "20/11/2022"
        ; "21/11/2022"
        ; "22/11/2022"
        ; "23/11/2022"
        ; "24/11/2022"
        ; "25/11/2022"
        ; "26/11/2022"
        ; "27/11/2022"
        ; "28/11/2022"
        ; "29/11/2022"
        ; "30/11/2022"
        ; "01/12/2022"
        ; "02/12/2022"
        ; "03/12/2022"
        ; "04/12/2022"
        ; "05/12/2022"
        ; "06/12/2022"
        ; "07/12/2022"
        ; "08/12/2022"
        ; "09/12/2022"
        ; "10/12/2022"
        ; "11/12/2022"
        ; "12/12/2022"
        ; "13/12/2022"
        ; "14/12/2022"
        ; "15/12/2022"
        ; "16/12/2022"
        ; "17/12/2022"
        ; "18/12/2022"
        ; "19/12/2022"
        ; "20/12/2022"
        ; "21/12/2022"
        ; "22/12/2022"
        ; "23/12/2022"
        ; "24/12/2022"
        ; "25/12/2022"
        ; "26/12/2022"
        ; "27/12/2022"
        ; "28/12/2022"
        ; "29/12/2022"
        ; "30/12/2022"
        ; "31/12/2022"
        ; "01/01/2023"
        ; "02/01/2023"
        ; "03/01/2023"
        ; "04/01/2023"
        ; "05/01/2023"
        ; "06/01/2023"
        ; "07/01/2023"
        ; "08/01/2023"
        ; "09/01/2023"
        ; "10/01/2023"
        ; "11/01/2023"
        ; "12/01/2023"
        ; "13/01/2023"
        ; "14/01/2023"
        ; "15/01/2023"
        ; "16/01/2023"
        ; "17/01/2023"
        ; "18/01/2023"
        ; "19/01/2023"
        ; "20/01/2023"
        ; "21/01/2023"
        ; "22/01/2023"
        ; "23/01/2023"
        ; "24/01/2023"
        ; "25/01/2023"
        ; "26/01/2023"
        ; "27/01/2023"
        ; "28/01/2023"
        ; "29/01/2023"
        ; "30/01/2023"
        ; "31/01/2023"
        ; "01/02/2023"
        ; "02/02/2023"
        ; "03/02/2023"
        ; "04/02/2023"
        ; "05/02/2023"
        ; "06/02/2023"
        ; "07/02/2023"
        ; "08/02/2023"
        ; "09/02/2023"
        ; "10/02/2023"
        ; "11/02/2023"
        ; "12/02/2023"
        ; "13/02/2023"
        ; "14/02/2023"
        ; "15/02/2023"
        ; "16/02/2023"
        ; "17/02/2023"
        ; "18/02/2023"
        ; "19/02/2023"
        ; "20/02/2023"
        ; "21/02/2023"
        ; "22/02/2023"
        ; "23/02/2023"
        ; "24/02/2023"
        ; "25/02/2023"
        ; "26/02/2023"
        ; "27/02/2023"
        ; "28/02/2023"
        ; "01/03/2023"
        ; "02/03/2023"
        ; "03/03/2023"
        ; "04/03/2023"
        ; "05/03/2023"
        ; "06/03/2023"
        ; "07/03/2023"
        ; "08/03/2023"
        ; "09/03/2023"
        ; "10/03/2023"
        ; "11/03/2023"
        ; "12/03/2023"
        ; "13/03/2023"
        ; "14/03/2023"
        ; "15/03/2023"
        ; "16/03/2023"
        ; "17/03/2023"
        ; "18/03/2023"
        ; "19/03/2023"
        ; "20/03/2023"
        ; "21/03/2023"
        ; "22/03/2023"
        ; "23/03/2023"
        ; "24/03/2023"
        ; "25/03/2023"
        ; "26/03/2023"
        ; "27/03/2023"
        ; "28/03/2023"
        ; "29/03/2023"
        ; "30/03/2023"
        ; "31/03/2023"
        ; "01/04/2023"
        ; "02/04/2023"
        ; "03/04/2023"
        ; "04/04/2023"
        ; "05/04/2023"
        ; "06/04/2023"
        ; "07/04/2023"
        ; "08/04/2023"
        ; "09/04/2023"
        ; "10/04/2023"
        ; "11/04/2023"
        ; "12/04/2023"
        ; "13/04/2023"
        ; "14/04/2023"
        ; "15/04/2023"
        ; "16/04/2023"
        ; "17/04/2023"
        ; "18/04/2023"
        ; "19/04/2023"
        ; "20/04/2023"
        ; "21/04/2023"
        ; "22/04/2023"
        ; "23/04/2023"
        ; "24/04/2023"
        ; "25/04/2023"
        ; "26/04/2023"
        ; "27/04/2023"
        ; "28/04/2023"
        ; "29/04/2023"
        ; "30/04/2023"
        ; "01/05/2023"
        ; "02/05/2023"
        ; "03/05/2023"
        ; "04/05/2023"
        ; "05/05/2023"
        ]
        |> List.map date
        |> List.rev
      in
      let computed = Date.rev_unfold (date "10/10/2022") (date "05/05/2023") in
      check (list date_testable) "should be equal" expected computed)

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
    ; first_day_of_month_1
    ; last_day_of_month_1
    ; month_range_1
    ; month_range_2
    ; month_range_3
    ; quarter_1
    ; quarter_2
    ; next_1
    ; prev_1
    ; unfold_1
    ; unfold_2
    ; rev_unfold_1
    ] )
