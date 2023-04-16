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

let from_int_to_string_1 =
  test_case "from int, to string 1" `Quick (fun () ->
      let expected = "42.00" in
      let computed = Num.(42 |> from_int |> to_string) in
      check string "should be equal" expected computed)

let from_int64_to_string_1 =
  test_case "from int64, to string 1" `Quick (fun () ->
      let expected = "42.00" in
      let computed = Num.(42L |> from_int64 |> to_string) in
      check string "should be equal" expected computed)

let from_float_to_string_1 =
  test_case "from float, to string 1" `Quick (fun () ->
      let expected = "3.14" in
      let computed = Num.(3.14 |> from_float |> to_string) in
      check string "should be equal" expected computed)

let from_float_to_string_2 =
  test_case "from float, to string 2" `Quick (fun () ->
      let expected = "1.67" in
      let computed = Num.(1.6789890087655 |> from_float |> to_string) in
      check string "should be equal" expected computed)

let from_float_to_string_3 =
  test_case "from float, to string 1" `Quick (fun () ->
      let expected = "0.42" in
      let computed = Num.(0.42 |> from_float |> to_string) in
      check string "should be equal" expected computed)

let test_to_float_1 =
  test_case "to_float 1" `Quick (fun () ->
      let expected = 42.0 in
      let computed = Num.(42 |> from_int |> to_float) in
      check (float 2.0) "should be equal" expected computed)

let test_to_float_2 =
  test_case "to_float 2" `Quick (fun () ->
      let expected = 0.04 in
      let computed = Num.(0.04 |> from_float |> to_float) in
      check (float 2.0) "should be equal" expected computed)

let cases =
  ( "Num"
  , [
      from_int_to_string_1
    ; from_int64_to_string_1
    ; from_float_to_string_1
    ; from_float_to_string_2
    ; from_float_to_string_3
    ; test_to_float_1
    ; test_to_float_2
    ] )
