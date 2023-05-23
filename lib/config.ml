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

let refute = function Ok x -> x | _ -> failwith "refuted"

let daily_reference_salary =
  Temporal_db.from_list
    ([
       ("01/03/2020", 62.53)
     ; ("01/09/2021", 63.78)
     ; ("01/01/2022", 65.05)
     ; ("01/03/2022", 66.35)
     ; ("01/04/2022", 69.47)
     ; ("01/05/2022", 70.86)
     ; ("01/08/2022", 72.27)
     ; ("01/11/2022", 73.72)
     ; ("01/12/2022", 75.19)
     ]
    |> List.map (fun (d, v) -> (refute @@ Date.from_string d, Num.from_float v))
    )

let volatil_annual_leave =
  Temporal_db.from_list
    ([
       ("17/04/2017", "25/05/2017")
     ; ("02/04/2018", "10/05/2018")
     ; ("22/04/2019", "30/05/2019")
     ; ("13/04/2020", "21/05/2020")
     ; ("05/04/2021", "13/05/2021")
     ; ("18/04/2022", "26/05/2022")
     ; ("10/04/2023", "18/05/2023")
     ; ("01/04/2024", "09/05/2024")
     ; ("21/04/2025", "29/05/2025")
     ; ("06/04/2026", "14/05/2026")
     ; ("29/03/2027", "06/05/2027")
     ]
    |> List.concat_map (fun (p, a) ->
           [
             (refute @@ Date.from_string p, "Lundi de Pâques")
           ; (refute @@ Date.from_string a, "Ascension")
           ]))

let known_annual_leave =
  Constant_db.(
    from_list
      Date.
        [
          v Jan 1 "Jour de l'an"
        ; v May 1 "Fête du travail"
        ; v Jul 21 "Fête nationale"
        ; v Aug 15 "Assomption"
        ; v Nov 1 "Toussaint"
        ; v Nov 11 "Armistice de 1918"
        ; v Dec 25 "Noël"
        ])

let social_secretary =
  [
    ("Smart", Percent.from_float 6.50)
  ; ("Merveille", Percent.from_float 6.50)
  ; ("Amplo", Percent.from_float 6.50)
  ; ("Tentoo", Percent.from_float 6.50)
  ]
  |> List.to_seq
  |> Smap.of_seq
