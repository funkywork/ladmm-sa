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

type t = Int64.t

let from_int x = Int64.of_int (x * 100)
let from_int64 x = Int64.mul x 100L
let from_float x = Int64.of_float (x *. 100.)

let pp ppf x =
  let int_part = Int64.div x 100L and float_part = Int64.rem x 100L in
  Format.fprintf ppf "%Li.%02Li" int_part float_part

let to_string x = Format.asprintf "%a" pp x
