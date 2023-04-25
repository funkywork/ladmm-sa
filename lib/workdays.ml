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

type status =
  | Workday of Date.t
  | Weekend of Date.t * Date.day_of_week
  | Reason of Date.t * string

let from_date date =
  match Date.weekday_of date with
  | (Date.Sat | Date.Sun) as dow -> Weekend (date, dow)
  | _ -> (
      match Temporal_db.find Config.volatil_annual_leave date with
      | Some x -> Reason (date, x)
      | None -> (
          match Constant_db.find Config.known_annual_leave date with
          | Some x -> Reason (date, x)
          | None -> Workday date))

let pp_status ppf = function
  | Workday d -> Format.fprintf ppf "Le %a est un jour régulier" Date.pp d
  | Weekend (d, dow) ->
      Format.fprintf ppf "Le %a est un %a" Date.pp d Date.pp_day_of_week dow
  | Reason (d, r) -> Format.fprintf ppf "Le %a correspond au %s" Date.pp d r

let equal_status a b =
  match (a, b) with
  | Workday a, Workday b -> Date.equal a b
  | Weekend (da, dowa), Weekend (db, dowb) ->
      Date.equal da db && Date.equal_day_of_week dowa dowb
  | Reason (da, ra), Reason (db, rb) -> Date.equal da db && String.equal ra rb
  | _, _ -> false

type diagnosis = {
    workdays : int
  ; saturdays : int
  ; sundays : int
  ; leaves : (Date.t * string) list
}

let from_range a b =
  let list = Date.rev_unfold a b in
  List.fold_left
    (fun acc date ->
      match from_date date with
      | Workday _ -> { acc with workdays = succ acc.workdays }
      | Weekend (_, Date.Sat) -> { acc with saturdays = succ acc.saturdays }
      | Weekend (_, _) -> { acc with sundays = succ acc.sundays }
      | Reason (date, reason) ->
          { acc with leaves = (date, reason) :: acc.leaves })
    { workdays = 0; saturdays = 0; sundays = 0; leaves = [] }
    list
