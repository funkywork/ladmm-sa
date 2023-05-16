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

open Js_of_ocaml

module type STORAGE_REQ = sig
  val storage : Dom_html.storage Js.t Js.optdef
end

module Storage (R : STORAGE_REQ) = struct
  exception Not_supported
  exception Not_found

  module Storage_map = Map.Make (String)

  type key = string
  type value = string

  let is_supported () =
    Js.Optdef.case R.storage (fun () -> false) (fun _ -> true)

  let storage =
    Js.Optdef.case R.storage (fun () -> raise Not_supported) (fun x -> x)

  let length () = storage##.length

  let get key =
    storage##getItem (Js.string key)
    |> Js.Opt.to_option
    |> Option.map Js.to_string

  let set key value = storage##setItem (Js.string key) (Js.string value)
  let remove key = storage##removeItem (Js.string key)

  let update f key =
    let value = get key in
    let final_value = f value in
    let () =
      match final_value with None -> remove key | Some x -> set key x
    in
    final_value

  let clear () = storage##clear
  let key i = storage##key i |> Js.Opt.to_option |> Option.map Js.to_string

  let at i =
    match key i with
    | None -> None
    | Some k -> Option.map (fun value -> (k, value)) (get k)

  let filter predicate =
    let len = length () in
    let map = Storage_map.empty in
    let rec aux acc i =
      if i < len then
        match at i with
        | None -> raise Not_found
        | Some (key, value) ->
            let new_map =
              if predicate key value then Storage_map.add key value acc else acc
            in
            aux new_map (i + 1)
      else acc
    in
    aux map 0

  let to_map () = filter (fun _ _ -> true)
end

include Storage (struct
  let storage = Dom_html.window##.localStorage
end)
