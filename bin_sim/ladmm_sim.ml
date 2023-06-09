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

let () =
  let open Js_browser in
  match Document.(get_element_by_id document "application") with
  | None -> Firebug.console##error (Js.string "Unable to find root node")
  | Some root ->
      let init = Model.init () and update = Model.update and view = View.view in
      let app = Vdom.simple_app ~init ~view ~update () in
      let () = Element.remove_all_children root in
      Vdom_blit.run app |> Vdom_blit.dom |> Element.append_child root
