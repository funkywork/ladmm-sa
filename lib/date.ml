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

type month = Sigs.month =
  | Jan
  | Feb
  | Mar
  | Apr
  | May
  | Jun
  | Jul
  | Aug
  | Sep
  | Oct
  | Nov
  | Dec

type day_of_week = Sigs.day_of_week = Mon | Tue | Wed | Thu | Fri | Sat | Sun
type t = { year : int; month : month; day : int }

let year_of { year; _ } = year
let month_of { month; _ } = month
let day_of { day; _ } = day

let is_leap_year year =
  if year mod 100 = 0 then year mod 400 = 0 else year mod 4 = 0

let in_leap_year d = is_leap_year (year_of d)

let days_in_month year month =
  match month with
  | Jan | Mar | May | Jul | Aug | Oct | Dec -> 31
  | Feb -> if is_leap_year year then 29 else 28
  | _ -> 30

let validate_year x =
  if x < 1970 then Error (`Year_too_small x)
  else if x > 3000 then Error (`Year_too_big x)
  else Ok x

let validate_day y m d =
  if d < 1 then Error (`Day_negative_or_null d)
  else
    let dim = days_in_month y m in
    if d > dim then Error (`Day_invalid_for ((y, m, dim), d)) else Ok d

let month_to_int = function
  | Jan -> 0
  | Feb -> 1
  | Mar -> 2
  | Apr -> 3
  | May -> 4
  | Jun -> 5
  | Jul -> 6
  | Aug -> 7
  | Sep -> 8
  | Oct -> 9
  | Nov -> 10
  | Dec -> 11

let month_from_int = function
  | 1 -> Ok Jan
  | 2 -> Ok Feb
  | 3 -> Ok Mar
  | 4 -> Ok Apr
  | 5 -> Ok May
  | 6 -> Ok Jun
  | 7 -> Ok Jul
  | 8 -> Ok Aug
  | 9 -> Ok Sep
  | 10 -> Ok Oct
  | 11 -> Ok Nov
  | 12 -> Ok Dec
  | x -> Error (`Month_invalid x)

let pp_month ppf m =
  Format.fprintf ppf "%s"
    (match m with
    | Jan -> "Janvier"
    | Feb -> "Février"
    | Mar -> "Mars"
    | Apr -> "Avril"
    | May -> "Mai"
    | Jun -> "Juin"
    | Jul -> "Juillet"
    | Aug -> "Août"
    | Sep -> "Septembre"
    | Oct -> "Octobre"
    | Nov -> "Novembre"
    | Dec -> "Décembre")

let month_to_string = Format.asprintf "%a" pp_month

let month_value = function
  | Jan -> 0
  | Feb -> 3
  | Mar -> 3
  | Apr -> 6
  | May -> 1
  | Jun -> 4
  | Jul -> 6
  | Aug -> 2
  | Sep -> 5
  | Oct -> 0
  | Nov -> 3
  | Dec -> 5

let day_of_week y m d =
  let yy = y mod 100 in
  let cc = (y - yy) / 100 in
  let c_code = [| 6; 4; 2; 0 |].(cc mod 4) in
  let y_code = (yy + (yy / 4)) mod 7 in
  let m_code =
    let v = month_value m in
    if is_leap_year y && (m = Jan || m = Feb) then v - 1 else v
  in
  let index = (c_code + y_code + m_code + d) mod 7 in
  [| Sun; Mon; Tue; Wed; Thu; Fri; Sat |].(index)

let make ~year:y ~month ~day:d =
  let open Util.Result in
  let* year = validate_year y in
  let+ day = validate_day year month d in

  { year; month; day }

let weekday_of { year; month; day } = day_of_week year month day

let pp ppf { year; month; day; _ } =
  Format.fprintf ppf "%02d/%02d/%04d" day (month_to_int month + 1) year

let to_string d = Format.asprintf "%a" pp d

let from_string str =
  let san = String.(lowercase_ascii @@ trim str) in
  match String.split_on_char '/' san with
  | [ d; m; y ] ->
      let opt =
        let open Util.Option in
        let* d = int_of_string_opt d in
        let* m = int_of_string_opt m in
        let+ y = int_of_string_opt y in
        (d, m, y)
      in
      let open Util.Result in
      let* day, m, year = Option.to_result ~none:(`Date_invalid san) opt in
      let* month = month_from_int m in
      make ~year ~month ~day
  | _ -> Error (`Date_invalid san)

let pp_error ppf err =
  Format.fprintf ppf "%s"
    (match err with
    | `Year_too_small x -> Format.asprintf "L'année [%d] est trop petite" x
    | `Year_too_big x -> Format.asprintf "L'année [%d] est trop grande" x
    | `Day_negative_or_null x ->
        Format.asprintf "Le jour [%d] est inférieur à 1" x
    | `Day_invalid_for ((y, m, dim), d) ->
        Format.asprintf
          "Le jour [%d] est invalide pour le mois de %s %04d, il n'a que %d \
           jours, "
          d (month_to_string m) y dim
    | `Month_invalid x ->
        Format.asprintf
          "Le mois [%d] doit-être compris entre 1 (Janvier) et 12 (Décembre)" x
    | `Date_invalid x ->
        Format.asprintf
          "La date [%s] est invaide, elle doit avoir le format \
           [jour/mois/année]"
          x)

let equal_error a b =
  match (a, b) with
  | `Year_too_small a, `Year_too_small b -> Int.equal a b
  | `Year_too_big a, `Year_too_big b -> Int.equal a b
  | `Day_negative_or_null a, `Day_negative_or_null b -> Int.equal a b
  | `Day_invalid_for ((xa, xb, xc), xd), `Day_invalid_for ((ya, yb, yc), yd) ->
      Int.equal xa ya
      && Int.equal (month_to_int xb) (month_to_int yb)
      && Int.equal xc yc
      && Int.equal xd yd
  | `Month_invalid a, `Month_invalid b -> Int.equal a b
  | `Date_invalid a, `Date_invalid b -> String.equal a b
  | _ -> false

let first_day_of_month d = { d with day = 1 }

let last_day_of_month d =
  let dim = days_in_month d.year d.month in
  { d with day = dim }

let month_range d = (first_day_of_month d, last_day_of_month d)

let pp_day_of_week ppf x =
  Format.fprintf ppf "%s"
    (match x with
    | Mon -> "Lundi"
    | Tue -> "Mardi"
    | Wed -> "Mercredi"
    | Thu -> "Jeudi"
    | Fri -> "Vendredi"
    | Sat -> "Samedi"
    | Sun -> "Dimanche")

let equal_day_of_week a b =
  match (a, b) with
  | Mon, Mon -> true
  | Tue, Tue -> true
  | Wed, Wed -> true
  | Thu, Thu -> true
  | Fri, Fri -> true
  | Sat, Sat -> true
  | Sun, Sun -> true
  | _ -> false

let add_month date value =
  let current_year = year_of date in
  let current_day = day_of date in
  let current_month = month_of date in
  let month_i = month_to_int current_month in
  let year_offset = (month_i + value) / 12 in
  let month =
    match Util.modulo (month_i + value) 12 + 1 |> month_from_int with
    | Ok x -> x
    | _ -> failwith "unreachable_case"
  in
  let year = current_year + year_offset + if value >= 0 then 0 else 1 in
  let day = Int.max (days_in_month year month) current_day in
  { year; month; day }

let make_matrix ~date ~duration ~steps =
  let arr = Array.make steps (date, date) in
  let rec aux start_date x =
    if x >= steps then ()
    else
      let end_date = add_month start_date duration |> last_day_of_month in
      let () = Array.set arr x (start_date, end_date) in
      aux (add_month end_date 1 |> first_day_of_month) (succ x)
  in
  let () = aux (first_day_of_month date) 0 in
  arr

let quarters date n = make_matrix ~date ~duration:2 ~steps:n

let next_month = function
  | Jan -> Feb
  | Feb -> Mar
  | Mar -> Apr
  | Apr -> May
  | May -> Jun
  | Jun -> Jul
  | Jul -> Aug
  | Aug -> Sep
  | Sep -> Oct
  | Oct -> Nov
  | Nov -> Dec
  | Dec -> Jan

let prev_month = function
  | Jan -> Dec
  | Feb -> Jan
  | Mar -> Feb
  | Apr -> Mar
  | May -> Apr
  | Jun -> May
  | Jul -> Jun
  | Aug -> Jul
  | Sep -> Aug
  | Oct -> Sep
  | Nov -> Oct
  | Dec -> Nov

let next { year; month; day } =
  let dim = days_in_month year month in
  let new_year, new_month, new_day =
    if day = dim then
      ((year + match month with Dec -> 1 | _ -> 0), next_month month, 1)
    else (year, month, day + 1)
  in
  { year = new_year; month = new_month; day = new_day }

let prev { year; month; day } =
  let new_year, new_month, new_day =
    if day = 1 then
      let new_year = match month with Jan -> year - 1 | _ -> year in
      let new_month = prev_month month in
      let new_day = days_in_month new_year new_month in
      (new_year, new_month, new_day)
    else (year, month, day - 1)
  in
  { year = new_year; month = new_month; day = new_day }

let compare a b =
  let f { year; month; day } =
    (year * 10000) + ((month_to_int month + 1) * 100) + day
  in
  Int.compare (f a) (f b)

let equal a b = Int.equal 0 (compare a b)
let ( = ) = equal
let ( <> ) x y = not (equal x y)
let ( > ) x y = compare x y > 0
let ( >= ) x y = compare x y >= 0
let ( < ) x y = compare x y < 0
let ( <= ) x y = compare x y <= 0
let ( + ) d x = add_month d x
let ( - ) d x = add_month d (-x)
let min a b = if b > a then a else b
let max a b = if a < b then a else b
let sort_range a b = if a < b then (a, b) else (b, a)

let unfold start_date end_date =
  let a, b = sort_range start_date end_date in
  let rec aux acc curr =
    if curr < a then acc else aux (curr :: acc) (prev curr)
  in
  aux [] b

let rev_unfold start_date end_date =
  let a, b = sort_range start_date end_date in
  let rec aux acc curr =
    if curr > b then acc else aux (curr :: acc) (next curr)
  in
  aux [] a
