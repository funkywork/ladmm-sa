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

(** A key value storage indexed by the couple [month/day]. *)

type 'a t

(** {1 Creation} *)

val from_list : ((Date.month * int) * 'a) list -> 'a t

(** {1 Find operation} *)

val find : 'a t -> Date.t -> 'a option

(** {1 Util} *)

val v : Date.month -> int -> 'a -> (Date.month * int) * 'a
val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
