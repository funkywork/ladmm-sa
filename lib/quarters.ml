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

type t = (Date.t * Date.t) array

let init date = Date.quarters date 8

let get_by_date quarters date =
  let rec aux i =
    if i >= 8 then Error (`No_quarter (Date.to_string date))
    else
      let start_date, end_date = Array.get quarters i in
      if Date.(date >= start_date && date <= end_date) then Ok i
      else aux (succ i)
  in
  aux 0

let str_range sdate edate =
  Format.asprintf "%a -> %a" Date.pp sdate Date.pp edate

let get_by_range quarters sdate edate =
  let rec aux i =
    if i >= 8 then Error (`No_quarter (str_range sdate edate))
    else
      let start_date, end_date = Array.get quarters i in
      if Date.(sdate > end_date) then aux (succ i)
      else if Date.(sdate < start_date) then
        Error (`No_quarter (str_range sdate edate))
      else if Date.(sdate >= start_date && edate <= end_date) then Ok i
      else Error (`Range_overlapping (str_range sdate edate))
  in
  if Date.(sdate > edate) then Error (`Range_invalid (str_range sdate edate))
  else aux 0

let to_representable_list quarters =
  List.init 8 (fun i ->
      let start_date, end_date = Array.get quarters i in
      (succ i, start_date, end_date))

let pp_error ppf err =
  Format.fprintf ppf "%s"
    (match err with
    | `No_quarter s ->
        Format.asprintf "Pas de trimestre trouvé pour la date [%s]" s
    | `Range_invalid s ->
        Format.asprintf
          "La date de fin de [%s] doit être égal ou supérieur à la date de \
           début"
          s
    | `Range_overlapping s ->
        Format.asprintf
          "L'intervalle de date [%s] superpose plusieurs trimestres" s
    | _ -> "unknown")

let equal_error a b =
  match (a, b) with
  | `No_quarter a, `No_quarter b -> String.equal a b
  | `Range_overlapping a, `Range_overlapping b -> String.equal a b
  | `Range_invalid a, `Range_invalid b -> String.equal a b
  | _ -> false

let get_interval quarters =
  let len = Array.length quarters in
  let soq = fst (Array.get quarters 0) in
  let eoq = snd (Array.get quarters (pred len)) in
  (soq, eoq)

let get_name quarters =
  let soq, eoq = get_interval quarters in
  Format.asprintf "%a_%a" Date.pp soq Date.pp eoq

let first_date quarters = fst (get_interval quarters)

let compare a b =
  let a = first_date a and b = first_date b in
  Date.compare a b
