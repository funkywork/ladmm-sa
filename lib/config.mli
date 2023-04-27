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

(** Defines the constants of the application. To be modified according to
    governmental updates.

    As the application must be deployable on a static server, this data is
    hardcoded but this will change if a server is set up. Moreover, as the whole
    logic is client driven, it would be possible to imagine a Wordpress plugin
    serving an API consumed by this client. Currently, this data is located in
    the configuration. *)

val daily_reference_salary : Num.t Temporal_db.t
(** The reference daily wage used to calculate the number of days worked from a
    gross wage. *)

val volatil_annual_leave : string Temporal_db.t
(** Holidays that cannot be calculated statically (Easter and Ascension, the
    latter depending on the calculation of Easter). *)

val known_annual_leave : string Constant_db.t
(** List of static holidays (which do not change from year to year). *)

val social_secretary : Percent.t Smap.t
