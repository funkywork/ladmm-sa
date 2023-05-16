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

type t =
  | Close_pan
  | Not_open_fill_ident of string
  | Not_open_fill_date of string
  | Create_case
  | Delete_case of string
  | Open_case of string
  | Close_case
  | Write_by_duration
  | Write_by_fee
  | Synthesis
  | Fill_duration_start of string
  | Fill_duration_end of string
  | Fill_days of string
  | Check_contract of bool
  | Check_c4 of bool
  | Check_is_artistic of bool
  | Save_duration_entry
  | Save_fee_entry
  | Recheck_c4 of string * bool
  | Recheck_contract of string * bool
  | Delete_entry of string
  | Fill_fee_date of string
  | Fill_fee_amount of string
  | Fill_ref_salary of string
  | Check_tva of bool
  | Fill_secretary of string
  | Nop
