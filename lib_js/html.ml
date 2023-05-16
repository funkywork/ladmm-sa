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

include Vdom

let txt = text
let span ?key ?a l = elt "span" ?key ?a l
let button ?key ?a l = elt "button" ?key ?a l
let label ?key ?a l = elt "label" ?key ?a l
let placeholder x = Property ("placeholder", String x)
let name x = Property ("name", String x)
let id x = Property ("id", String x)
let for_ x = Attribute ("for", x)
let style k v = Style (k, v)
let checked = Property ("checked", String "checked")
let selected = Property ("selected", String "selected")
let fieldset ?key ?a l = elt "fieldset" ?key ?a l
let legend ?key ?a l = elt "legend" ?key ?a l

let checkbox ?key ?(a = []) is_checked () =
  let args =
    a @ [ type_ "checkbox" ] @ if is_checked then [ checked ] else []
  in
  input ~a:args ?key []

let select ?key ?a l = elt "select" ?key ?a l
let option ?key ?a l = elt "option" ?key ?a l

let optional ?key ?a l =
  select ?key ?a
    (List.map
       (fun (key, v, is_checked) ->
         option
           ~a:([ value key ] @ if is_checked then [ selected ] else [])
           [ txt v ])
       l)

let optional_from_map ?key ?a opt smap =
  let k =
    smap
    |> Ladmm_lib.Smap.bindings
    |> List.map (fun (k, _) ->
           (k, k, Option.fold ~none:false ~some:(fun x -> String.equal x k) opt))
  in
  optional ?key ?a (("nop", "Aucun", Option.is_none opt) :: k)

let h1 ?key ?a l = elt "h1" ?key ?a l
let h2 ?key ?a l = elt "h2" ?key ?a l
let h3 ?key ?a l = elt "h3" ?key ?a l

let fieldset_with ?key ?a title l =
  fieldset ?key ?a @@ (legend [ txt title ] :: [ div l ])

let p ?key ?a l = elt "p" ?key ?a l
