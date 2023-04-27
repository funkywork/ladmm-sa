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

let init_1 =
  test_case "make a valid quarters and represent-it" `Quick (fun () ->
      let expected =
        [
          "1 - 01/02/2023 to 30/04/2023"
        ; "2 - 01/05/2023 to 31/07/2023"
        ; "3 - 01/08/2023 to 31/10/2023"
        ; "4 - 01/11/2023 to 31/01/2024"
        ; "5 - 01/02/2024 to 30/04/2024"
        ; "6 - 01/05/2024 to 31/07/2024"
        ; "7 - 01/08/2024 to 31/10/2024"
        ; "8 - 01/11/2024 to 31/01/2025"
        ]
      in
      let computed =
        date "28/02/2023"
        |> Quarters.init
        |> Quarters.to_representable_list
        |> List.map (fun (i, s, e) ->
               Format.asprintf "%d - %a to %a" i Date.pp s Date.pp e)
      in
      check (list string) "should be the same" expected computed)

let get_by_date_1 =
  test_case "get a valid date from a quarters" `Quick (fun () ->
      let q = date "28/02/2023" |> Quarters.init in
      check_quarters_by_date q "1/2/2023" (Ok 0);
      check_quarters_by_date q "22/04/2023" (Ok 0);
      check_quarters_by_date q "7/06/2023" (Ok 1);
      check_quarters_by_date q "18/09/2024" (Ok 6);
      check_quarters_by_date q "31/01/2025" (Ok 7);
      check_quarters_by_date q "22/01/2023" (Error (`No_quarter "22/01/2023"));
      check_quarters_by_date q "22/04/2025" (Error (`No_quarter "22/04/2025")))

let get_by_range_1 =
  test_case "get a valid range from a quarter" `Quick (fun () ->
      let q = date "28/02/2023" |> Quarters.init in
      check_quarters_by_range q "12/02/2023" "01/02/2023"
        (Error (`Range_invalid "12/02/2023 -> 01/02/2023"));
      check_quarters_by_range q "12/02/2023" "27/04/2023" (Ok 0);
      check_quarters_by_range q "01/02/2023" "27/04/2023" (Ok 0);
      check_quarters_by_range q "12/06/2023" "22/06/2023" (Ok 1);
      check_quarters_by_range q "03/11/2024" "24/11/2024" (Ok 7);
      check_quarters_by_range q "01/11/2024" "24/11/2024" (Ok 7);
      check_quarters_by_range q "12/02/2021" "27/04/2021"
        (Error (`No_quarter "12/02/2021 -> 27/04/2021"));
      check_quarters_by_range q "12/02/2025" "27/04/2028"
        (Error (`No_quarter "12/02/2025 -> 27/04/2028"));
      check_quarters_by_range q "04/04/2023" "02/06/2023"
        (Error (`Range_overlapping "04/04/2023 -> 02/06/2023")))

let cases = ("Quarters", [ init_1; get_by_date_1; get_by_range_1 ])
