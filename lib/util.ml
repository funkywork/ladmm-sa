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

module Result = struct
  type ('a, 'b) t = ('a, 'b) Stdlib.Result.t = Ok of 'a | Error of 'b

  let map f = function Ok x -> Ok (f x) | Error x -> Error x
  let bind f = function Ok x -> f x | Error x -> Error x
  let ( >|= ) x f = map f x
  let ( >>= ) x f = bind f x
  let ( let+ ) x f = x >|= f
  let ( let* ) x f = x >>= f
end

module Option = struct
  type 'a t = 'a option = None | Some of 'a

  let map f = function Some x -> Some (f x) | None -> None
  let bind f = function Some x -> f x | None -> None
  let ( >|= ) x f = map f x
  let ( >>= ) x f = bind f x
  let ( let+ ) x f = x >|= f
  let ( let* ) x f = x >>= f
end

let modulo x y =
  let r = x mod y in
  if r >= 0 then r else r + y
