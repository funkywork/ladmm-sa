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

(** Represents a date in the form [dd/mm/yyyy]. *)

type t

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

(** {1 Creation} *)

val make :
  year:int -> month:month -> day:int -> (t, [> Sigs.date_error ]) result

val from_string : string -> (t, [> Sigs.date_error ]) result

(** {1 Accessors} *)

val year_of : t -> int
val month_of : t -> month
val day_of : t -> int
val weekday_of : t -> day_of_week
val in_leap_year : t -> bool

(** {1 Comparisons} *)

val equal : t -> t -> bool
val compare : t -> t -> int
val ( = ) : t -> t -> bool
val ( <> ) : t -> t -> bool
val ( > ) : t -> t -> bool
val ( < ) : t -> t -> bool
val ( >= ) : t -> t -> bool
val ( <= ) : t -> t -> bool
val min : t -> t -> t
val max : t -> t -> t

(** {1 Date control} *)

val first_day_of_month : t -> t
val last_day_of_month : t -> t
val month_range : t -> t * t
val next : t -> t
val prev : t -> t
val add_month : t -> int -> t
val ( + ) : t -> int -> t
val ( - ) : t -> int -> t
val make_matrix : date:t -> duration:int -> steps:int -> (t * t) array
val quarters : t -> int -> (t * t) array
val unfold : t -> t -> t list
val rev_unfold : t -> t -> t list

(** {1 Util} *)

val pp : Format.formatter -> t -> unit
val to_string : t -> string
val pp_day_of_week : Format.formatter -> day_of_week -> unit
val pp_month : Format.formatter -> month -> unit
val equal_day_of_week : day_of_week -> day_of_week -> bool
val month_to_int : month -> int

(** {1 Error util} *)

val pp_error : Format.formatter -> [> Sigs.date_error ] -> unit
val equal_error : [> Sigs.date_error ] -> [> Sigs.date_error ] -> bool
