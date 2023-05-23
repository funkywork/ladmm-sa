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

open Ladmm_lib
open Ladmm_js

let compute_error = function
  | None | Some (Ok _) -> []
  | Some (Error str) -> Html.[ txt str ]

let htd_compute_result = function
  | None -> []
  | Some x ->
      Html.
        [
          div
            [
              span
                ~a:[ class_ "verbatim" ]
                [
                  txt
                  @@ (Format.asprintf "%a" Num.pp x
                     |> Util.replace_char ~from:'.' ~by:',')
                ]
            ; span [ txt "jours calculés" ]
            ]
        ]

let htd_simulation Model.{ htd_hours; htd_avg; htd_result } =
  let open Html in
  fieldset_with
    ~a:[ class_ "simulator" ]
    "Conversion d'heure en journées travaillées"
    [
      p
        [
          txt
            "Ce simulateur permet la conversion rapide d'un nombre heures \
             prestées en un nombre de jours prestés. (En semaine de 5 jours). "
        ]
    ; div
        ~a:[ class_ "enum" ]
        [
          div
            [
              div [ label ~a:[ for_ "htd-hours" ] [ txt "Nombre d'heures" ] ]
            ; div
                [
                  input
                    ~a:
                      [
                        type_ "text"
                      ; placeholder "Nombre d'heures prestées"
                      ; value htd_hours.str
                      ; name "htd-hours"
                      ; id "htd-hours"
                      ; oninput (fun x -> Message.Htd_fill_hours x)
                      ]
                    []
                ]
            ; div (compute_error htd_hours.repr)
            ]
        ; div
            [
              div
                [
                  label ~a:[ for_ "htd-avg" ] [ txt "Heures pour une journée" ]
                ]
            ; div
                [
                  input
                    ~a:
                      [
                        type_ "text"
                      ; placeholder "Nombre d'heures dans une journée"
                      ; value htd_avg.str
                      ; name "htd-avg"
                      ; id "htd-avg"
                      ; oninput (fun x -> Message.Htd_fill_avg x)
                      ]
                    []
                ]
            ; div (compute_error htd_avg.repr)
            ]
        ; div
            [
              div [ txt "Journées prestées" ]
            ; div (htd_compute_result htd_result)
            ; div []
            ]
        ]
    ]

let eur x =
  Html.
    [
      span
        ~a:[ class_ "verbatim" ]
        [
          txt
          @@ (Format.asprintf "%a" Num.pp x
             |> Util.replace_char ~from:'.' ~by:',')
        ]
    ; span [ txt "euros" ]
    ]

let days x =
  Html.
    [
      span
        ~a:[ class_ "verbatim" ]
        [
          txt
          @@ (Format.asprintf "%a" Num.pp x
             |> Util.replace_char ~from:'.' ~by:',')
        ]
    ; span [ txt "jours" ]
    ]

let compute_nod_result = function
  | None -> []
  | Some Model.{ r_nod_brut; r_nod_tva; r_nod_social; r_nod_cost; r_nod_cachet }
    ->
      Html.
        [
          div [ div [ txt "Brut" ]; div @@ eur r_nod_brut; div [] ]
        ; div [ div [ txt "TVA" ]; div @@ eur r_nod_tva; div [] ]
        ; div [ div [ txt "Bureau social" ]; div @@ eur r_nod_social; div [] ]
        ; div
            [ div [ txt "Charges patronales" ]; div @@ eur r_nod_cost; div [] ]
        ; div [ div [ txt "Cachet" ]; div @@ eur r_nod_cachet; div [] ]
        ]

let compute_gross_result = function
  | None -> []
  | Some
      Model.{ r_gross_days; r_gross; r_gross_tva; r_gross_social; r_gross_cost }
    ->
      Html.
        [
          div
            [
              div [ txt "Nombre de jours éligibles" ]
            ; div @@ days r_gross_days
            ; div []
            ]
        ; div [ div [ txt "Montant brut" ]; div @@ eur r_gross_cost; div [] ]
        ; div [ div [ txt "TVA" ]; div @@ eur r_gross_tva; div [] ]
        ; div [ div [ txt "Bureau social" ]; div @@ eur r_gross_social; div [] ]
        ; div [ div [ txt "Charges patronales" ]; div @@ eur r_gross; div [] ]
        ]

let gross_simulation
    Model.
      {
        gross_amount
      ; gross_social
      ; gross_social_percent
      ; gross_salary_ref
      ; gross_tva
      ; gross_result
      ; _
      } =
  let open Html in
  fieldset_with
    ~a:[ class_ "simulator" ]
    "Calcul des données intermédiaires en fonction du montant brut-brut"
    [
      p
        [
          txt
            "Ce simulateur permet d'approximativement le nombre de jours \
             éligibles à partir d'un montant brut-brut (avec possibilité \
             d'inclure, ou non, la TVA et l'implication d'un bureau social)"
        ]
    ; div
        ~a:[ class_ "enum" ]
        ([
           div
             [
               div
                 [
                   label
                     ~a:[ for_ "gross-amount" ]
                     [ txt "Montant du brut-brut" ]
                 ]
             ; div
                 [
                   input
                     ~a:
                       [
                         type_ "text"
                       ; placeholder "Montant du brut brut"
                       ; value gross_amount.str
                       ; name "gross-amount"
                       ; id "gross-amount"
                       ; oninput (fun x -> Message.Gross_fill_amount x)
                       ]
                     []
                 ]
             ; div (compute_error gross_amount.repr)
             ]
         ; div
             [
               div [ label ~a:[ for_ "gross-tva" ] [ txt "TVA incluse ?" ] ]
             ; div
                 [
                   checkbox
                     ~a:
                       [
                         id "gross-tva"
                       ; onchange_checked (fun x -> Message.Gross_change_tva x)
                       ]
                     gross_tva ()
                 ]
             ; div []
             ]
         ; div
             [
               div [ label ~a:[ for_ "gross-social" ] [ txt "Bureau social" ] ]
             ; div
                 [
                   optional_from_map
                     ~a:
                       [
                         name "gross-social"
                       ; id "gross-social"
                       ; oninput (fun x -> Message.Gross_change_social x)
                       ]
                     gross_social Config.social_secretary
                 ]
             ; div []
             ]
         ; div
             [
               div
                 [
                   label
                     ~a:[ for_ "gross-percent" ]
                     [ txt "Pourcentage perçu par le bureau social" ]
                 ]
             ; div
                 [
                   input
                     ~a:
                       [
                         type_ "text"
                       ; placeholder "6.5"
                       ; value gross_social_percent.str
                       ; name "gross-percent"
                       ; id "gross-percent"
                       ; oninput (fun x ->
                             Message.Gross_change_social_percent x)
                       ]
                     []
                 ]
             ; div (compute_error gross_social_percent.repr)
             ]
         ; div
             [
               div
                 [
                   label
                     ~a:[ for_ "gross-ref" ]
                     [ txt "Salaire journalier de référence" ]
                 ]
             ; div
                 [
                   input
                     ~a:
                       [
                         type_ "text"
                       ; placeholder "Entrez le salaire journalier de référence"
                       ; value gross_salary_ref.str
                       ; name "nod-ref"
                       ; id "nod-ref"
                       ; oninput (fun x -> Message.Gross_change_salary_ref x)
                       ]
                     []
                 ]
             ; div (compute_error gross_salary_ref.repr)
             ]
         ]
        @ compute_gross_result gross_result)
    ]

let nod_simulation
    Model.
      {
        nod_days
      ; nod_social
      ; nod_social_percent
      ; nod_salary_ref
      ; nod_result
      ; _
      } =
  let open Html in
  fieldset_with
    ~a:[ class_ "simulator" ]
    "Conversion de jours prestés en cachet"
    [
      p
        [
          txt
            "Ce simulateur permet d'approximativement définir un cachet en \
             fonction d'un nombre de jours à prester. "
        ]
    ; div
        ~a:[ class_ "enum" ]
        ([
           div
             [
               div [ label ~a:[ for_ "nod-days" ] [ txt "Nombre de jours" ] ]
             ; div
                 [
                   input
                     ~a:
                       [
                         type_ "text"
                       ; placeholder "Nombre de jours à prester"
                       ; value nod_days.str
                       ; name "nod-days"
                       ; id "nod-days"
                       ; oninput (fun x -> Message.Nod_fill_days x)
                       ]
                     []
                 ]
             ; div (compute_error nod_days.repr)
             ]
         ; div
             [
               div [ label ~a:[ for_ "nod-social" ] [ txt "Bureau social" ] ]
             ; div
                 [
                   optional_from_map
                     ~a:
                       [
                         name "nod-social"
                       ; id "nod-social"
                       ; oninput (fun x -> Message.Nod_change_social x)
                       ]
                     nod_social Config.social_secretary
                 ]
             ; div []
             ]
         ; div
             [
               div
                 [
                   label
                     ~a:[ for_ "nod-percent" ]
                     [ txt "Pourcentage perçu par le bureau social" ]
                 ]
             ; div
                 [
                   input
                     ~a:
                       [
                         type_ "text"
                       ; placeholder "6.5"
                       ; value nod_social_percent.str
                       ; name "nod-percent"
                       ; id "nod-percent"
                       ; oninput (fun x -> Message.Nod_change_social_percent x)
                       ]
                     []
                 ]
             ; div (compute_error nod_social_percent.repr)
             ]
         ; div
             [
               div
                 [
                   label
                     ~a:[ for_ "nod-ref" ]
                     [ txt "Salaire journalier de référence" ]
                 ]
             ; div
                 [
                   input
                     ~a:
                       [
                         type_ "text"
                       ; placeholder "Entrez le salaire journalier de référence"
                       ; value nod_salary_ref.str
                       ; name "nod-ref"
                       ; id "nod-ref"
                       ; oninput (fun x -> Message.Nod_change_salary_ref x)
                       ]
                     []
                 ]
             ; div (compute_error nod_salary_ref.repr)
             ]
         ]
        @ compute_nod_result nod_result)
    ]

let from_model Model.{ hours_to_days; from_nod; from_gross_gross; _ } =
  [
    htd_simulation hours_to_days
  ; nod_simulation from_nod
  ; gross_simulation from_gross_gross
  ]

let view model =
  let open Html in
  div ~a:[ class_ "simulators" ] @@ from_model model
