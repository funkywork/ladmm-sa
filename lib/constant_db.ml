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

module Constant_map = Map.Make (struct
  type t = Date.month * int

  let normalize (m, d) = ((Date.month_to_int m + 1) * 100) + d
  let compare a b = Int.compare (normalize a) (normalize b)
end)

type 'a t = 'a Constant_map.t

let from_list list =
  List.fold_left
    (fun cmap (key, value) -> Constant_map.add key value cmap)
    Constant_map.empty list

let pp aux ppf cmap =
  Format.fprintf ppf "Constant_map %a"
    (Format.pp_print_seq (fun ppf ((m, d), value) ->
         Format.fprintf ppf "%02d %a => %a" d Date.pp_month m aux value))
    (Constant_map.to_seq cmap)

let equal aux a b = Constant_map.equal aux a b

let find cmap date =
  let month = Date.month_of date in
  let day = Date.day_of date in
  let key = (month, day) in
  Constant_map.find_opt key cmap

let v m d v = ((m, d), v)
