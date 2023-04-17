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

type t = Num.t

let from_float x =
  let r = x /. 100.0 in
  Num.from_float r

let from_int x = from_float (Float.of_int x)
let from_int64 x = from_float (Int64.to_float x)

let pp ppf x =
  let r = Num.(x * from_int 100) in
  Format.fprintf ppf "%a%c" Num.pp r '%'

let to_string x = Format.asprintf "%a" pp x
let apply percent value = Num.mul value percent
let ( ~%: ) x = from_float x
