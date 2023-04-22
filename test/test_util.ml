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

exception Invalid_date

let get_date = function Ok x -> x | Error _ -> raise Invalid_date
let date_testable = testable Date.pp Date.equal
let day_of_week_testable = testable Date.pp_day_of_week Date.equal_day_of_week
let date_result_testable x = result x (testable Date.pp_error Date.equal_error)

let date_range a b =
  let open Util.Result in
  let* a = Date.from_string a in
  let+ b = Date.from_string b in
  (a, b)

let date x = Date.from_string x |> get_date

let quarters_result_testable x =
  result x (testable Quarters.pp_error Quarters.equal_error)

let check_quarters_by_date q x e =
  let computed = Quarters.get_by_date q (date x) in
  check (quarters_result_testable int) "should be equal" e computed

let check_quarters_by_range q a b e =
  let computed = Quarters.get_by_range q (date a) (date b) in
  check (quarters_result_testable int) "should be equal" e computed
