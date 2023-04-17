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

let from_int_1 =
  test_case "make a percent using [to_int]" `Quick (fun () ->
      let expected = "33.00%" in
      let computed = Percent.(33 |> from_int |> to_string) in
      check string "should be equal" expected computed)

let from_float_1 =
  test_case "make a percent using [to_float]" `Quick (fun () ->
      let expected = "33.33%" in
      let computed = Percent.(33.3344556 |> from_float |> to_string) in
      check string "should be equal" expected computed)

let apply_1 =
  test_case "apply a percent on a num using [apply]" `Quick (fun () ->
      let expected = "125.00" in
      let computed =
        Percent.(apply @@ from_int 50) (Num.from_int 250) |> Num.to_string
      in
      check string "should be equal" expected computed)

let apply_2 =
  test_case "apply a percent on a num using [apply]" `Quick (fun () ->
      let expected = "333.30" in
      let computed =
        Percent.(apply @@ ~%:33.33) (Num.from_int 1000) |> Num.to_string
      in
      check string "should be equal" expected computed)

let cases = ("Percent", [ from_int_1; from_float_1; apply_1; apply_2 ])
