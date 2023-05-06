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

type t = float

let from_int = float_of_int
let from_int64 = Int64.to_float
let from_float x = x
let ( ~!: ) x = from_float x

let from_string x =
  x
  |> float_of_string_opt
  |> Option.fold
       ~none:(Error (`Num_invalid_string x))
       ~some:(fun x -> Ok (from_float x))

let to_float x =
  let r = Float.to_int (x *. 100.0) in
  Float.of_int r /. 100.0

let add = Float.add
let sub = Float.sub
let mul = Float.mul
let div = Float.div
let rem = Float.rem
let pp ppf x = Format.fprintf ppf "%.2f" (to_float x)
let to_string x = Format.asprintf "%a" pp x
let equal = Float.equal
let compare = Float.compare
let ( = ) = equal
let ( <> ) x y = not (equal x y)
let ( > ) x y = compare x y > 0
let ( >= ) x y = compare x y >= 0
let ( < ) x y = compare x y < 0
let ( <= ) x y = compare x y <= 0
let ( + ) x y = add x y
let ( - ) x y = sub x y
let ( * ) x y = mul x y
let ( / ) x y = div x y
let ( mod ) = rem
