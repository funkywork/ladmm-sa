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

module Temporal_map = Map.Make (Date)

type 'a t = 'a Temporal_map.t
type 'a find_result = Found of Date.t * 'a | Out_of_bound of Date.t * 'a

let from_list list =
  List.fold_left
    (fun tmap (key, value) -> Temporal_map.add key value tmap)
    Temporal_map.empty list

let pp aux_pp ppf tmap =
  Format.fprintf ppf "Temporal_map %a"
    (Format.pp_print_seq (fun ppf (key, value) ->
         Format.fprintf ppf "%a => %a" Date.pp key aux_pp value))
    (Temporal_map.to_seq tmap)

let equal aux a b = Temporal_map.equal aux a b
let find tmap key = Temporal_map.find_opt key tmap

let find_minimal_after ?(included = true) tmap date =
  let valid = if included then Date.( >= ) else Date.( > ) in
  Temporal_map.find_first_opt (fun key -> valid date key) tmap

let find_maximal_after ?(included = true) tmap date =
  let valid = if included then Date.( >= ) else Date.( > ) in
  Temporal_map.find_last_opt (fun key -> valid date key) tmap

let find_for ?(included = true) tmap date =
  match find_maximal_after ~included tmap date with
  | Some (d, v) -> Found (d, v)
  | None ->
      let d, v = Temporal_map.find_first (fun _ -> true) tmap in
      Out_of_bound (d, v)

let equal_find_result aux a b =
  match (a, b) with
  | Found (date_a, value_a), Found (date_b, value_b)
  | Out_of_bound (date_a, value_a), Out_of_bound (date_b, value_b) ->
      Date.equal date_a date_b && aux value_a value_b
  | _ -> false

let pp_find_result aux ppf result =
  Format.fprintf ppf "%s"
    (match result with
    | Found (d, v) -> Format.asprintf "Found (%a, %a)" Date.pp d aux v
    | Out_of_bound (d, v) ->
        Format.asprintf "Out_of_bound (%a, %a)" Date.pp d aux v)

let to_list tmap = Temporal_map.to_seq tmap |> List.of_seq
let find_last tmap = Temporal_map.find_last_opt (fun _ -> true) tmap
