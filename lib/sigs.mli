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

type month =
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

type day_of_week = Mon | Tue | Wed | Thu | Fri | Sat | Sun

type date_make_error =
  [ `Year_too_small of int
  | `Year_too_big of int
  | `Day_negative_or_null of int
  | `Day_invalid_for of (int * month * int) * int
  | `Month_invalid of int ]

type date_error = [ date_make_error | `Date_invalid of string ]
type quarters_by_date_error = [ `No_quarter of string ]

type quarters_by_range_error =
  [ quarters_by_date_error
  | `Range_overlapping of string
  | `Range_invalid of string ]

type quarters_error = [ quarters_by_date_error | quarters_by_range_error ]
