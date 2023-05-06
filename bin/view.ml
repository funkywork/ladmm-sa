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

let render_ident str = function
  | Ok _ -> []
  | Error _ ->
      if String.trim str |> String.equal "" then []
      else [ Html.txt "Le nom du dossier est trop court" ]

let render_period str = function
  | Ok _ -> []
  | Error err ->
      if String.trim str |> String.equal "" then []
      else [ Html.txt @@ Format.asprintf "%a" Date.pp_error err ]

let compute_case_button ident period =
  let open Util.Result in
  let res =
    let* ident = ident in
    let+ quarters = period in
    Data.Stored_ident.can_add ident quarters
  in
  match res with
  | Ok true -> Ok ()
  | Ok false -> Error `Name_already_taken
  | Error err -> Error err

let case_button ident period =
  let open Html in
  match compute_case_button ident period with
  | Ok () ->
      [
        button
          ~a:[ disabled false; onclick (fun _ -> Message.Create_case) ]
          [ txt "Créer le profil" ]
      ]
  | Error `Name_already_taken ->
      [
        button ~a:[ disabled true ] [ txt "Créer le profil" ]
      ; span
          ~a:[ class_ "error" ]
          [ txt "Le dossier, pour cette période est déjà créé" ]
      ]
  | Error _ -> [ button ~a:[ disabled true ] [ txt "Créer le profil" ] ]

let render_cases =
  let open Html in
  function
  | [] -> txt "Aucun dossier n'a été créé"
  | xs ->
      div
        ~a:[ class_ "cases" ]
        (List.map
           (fun line ->
             let is_complete = line.Data.Case_line.is_complete in
             let index = line.Data.Case_line.index in
             div
               ~a:
                 [
                   class_ (if is_complete then "completed" else "not-completed")
                 ]
               [
                 div ~a:[]
                   [
                     input
                       ~a:
                         ([ type_ "checkbox"; disabled true ]
                         @ if is_complete then [ checked ] else [])
                       []
                   ]
               ; div
                   ~a:[ class_ "case-interval" ]
                   [
                     span
                       [ txt @@ Date.to_string line.Data.Case_line.start_date ]
                   ; span [ txt @@ Date.to_string line.Data.Case_line.end_date ]
                   ]
               ; div
                   ~a:[ class_ "artist-name" ]
                   [ txt (Ident.to_string line.Data.Case_line.name) ]
               ; div
                   ~a:[ class_ "edit-box" ]
                   [
                     button
                       ~a:
                         [
                           class_ "delete"
                         ; onclick (fun _ -> Message.Delete_case index)
                         ]
                       [ txt "supprimer" ]
                   ; button
                       ~a:
                         [
                           class_ "open"
                         ; onclick (fun _ -> Message.Open_case index)
                         ]
                       [ txt "ouvrir" ]
                   ]
               ])
           xs)

let render_computed_period =
  let open Html in
  function
  | Error _ -> []
  | Ok quarters ->
      let q =
        quarters
        |> Quarters.to_representable_list
        |> List.map (fun (i, s, e) ->
               div
                 [
                   div [ txt @@ Format.asprintf "Trimestre %d" i ]
                 ; div [ txt @@ Date.to_string s ]
                 ; div [ txt @@ Date.to_string e ]
                 ])
      in
      [ fieldset [ legend [ txt "Période calculée" ]; div q ] ]

let render_not_opened (ident_str, ident) (period_str, period) cases =
  let open Html in
  [
    fieldset
      [
        legend [ txt "Créer un nouveau dossier" ]
      ; div
          ~a:[ class_ "form" ]
          [
            div
              [
                label ~a:[ for_ "casename" ] [ txt "Nom du dossier" ]
              ; input
                  ~a:
                    [
                      name "case_name"
                    ; id "case_name"
                    ; value ident_str
                    ; placeholder "Nom de l'artiste"
                    ; oninput (fun x -> Message.Not_open_fill_ident x)
                    ]
                  []
              ; div ~a:[ class_ "error" ] (render_ident ident_str ident)
              ]
          ; div
              [
                label
                  ~a:[ for_ "case_period" ]
                  [ txt "Date de début de la saisie" ]
              ; input
                  ~a:
                    [
                      name "case_period"
                    ; id "case_period"
                    ; value period_str
                    ; placeholder "dd/mm/yyyy"
                    ; oninput (fun x -> Message.Not_open_fill_date x)
                    ]
                  []
              ; div ~a:[ class_ "error" ] (render_period period_str period)
              ]
          ]
      ; div ~a:[ class_ "computed_period" ] (render_computed_period period)
      ; div
          [
            div
              (case_button
                 (ident
                   : (Ident.t, Sigs.ident_error) result
                   :> (Ident.t, [> Sigs.ident_error ]) result)
                 (period
                   : (Quarters.t, Sigs.date_error) result
                   :> (Quarters.t, [> Sigs.date_error ]) result))
          ]
      ]
  ; fieldset [ legend [ txt "Dossiers existants" ]; render_cases cases ]
  ]

let render_entry offset i =
  let open Html in
  function
  | Data.Entry.Fee
      { has_contract; has_c4; date; days; id; gross; gross_gross; _ } ->
      div
        [
          div
            [
              button
                ~a:[ onclick (fun _ -> Message.Delete_entry id) ]
                [ txt "x" ]
            ; txt (Format.asprintf "  %d" (offset + i + 1))
            ]
        ; div
            [
              checkbox has_contract
                ~a:
                  [
                    onchange_checked (fun value ->
                        Message.Recheck_contract (id, value))
                  ]
                ()
            ]
        ; div
            [
              checkbox
                ~a:
                  [
                    onchange_checked (fun value ->
                        Message.Recheck_c4 (id, value))
                  ]
                has_c4 ()
            ]
        ; div [ txt @@ Format.asprintf "%a" Date.pp date ]
        ; div [ txt @@ Format.asprintf "%a" Num.pp gross ]
        ; div [ txt @@ Format.asprintf "%a" Num.pp gross_gross ]
        ; div []
        ; div
            [
              txt
              @@ Format.asprintf "%a jour%s" Num.pp days
                   (if Num.(days >= Num.from_int 2) then "s" else "")
            ]
        ]
  | Data.Entry.Duration
      { has_contract; has_c4; range = s, e; days; raw_days; id; is_artistic; _ }
    ->
      div
        ~a:[ class_ (if is_artistic then "artistic" else "not-art") ]
        [
          div
            [
              button
                ~a:[ onclick (fun _ -> Message.Delete_entry id) ]
                [ txt "x" ]
            ; txt (Format.asprintf "  %d" (offset + i + 1))
            ]
        ; div
            [
              checkbox has_contract
                ~a:
                  [
                    onchange_checked (fun value ->
                        Message.Recheck_contract (id, value))
                  ]
                ()
            ]
        ; div
            [
              checkbox
                ~a:
                  [
                    onchange_checked (fun value ->
                        Message.Recheck_c4 (id, value))
                  ]
                has_c4 ()
            ]
        ; div [ txt @@ Format.asprintf "%a -> %a" Date.pp s Date.pp e ]
        ; div []
        ; div []
        ; div
            [
              txt
              @@ Format.asprintf "%a jour%s" Num.pp raw_days
                   (if Num.(days >= Num.from_int 2) then "s" else "")
            ]
        ; div
            [
              txt
              @@ Format.asprintf "%a jour%s" Num.pp days
                   (if Num.(days >= Num.from_int 2) then "s" else "")
            ]
        ]

let active_template case content =
  let quarters = Data.Stored_case.group_entries case in
  let case_name = case.Data.Stored_case.name |> Ident.to_string in
  let open Html in
  [
    div
      ~a:[ class_ "command-open" ]
      [
        div
          [
            button
              ~a:[ onclick (fun _ -> Message.Write_by_duration) ]
              [ txt "Saisir une prestation à la durée" ]
          ; button
              ~a:[ onclick (fun _ -> Message.Write_by_fee) ]
              [ txt "Saisir une prestation au cachet" ]
          ]
      ; div
          [
            button
              ~a:[ onclick (fun _ -> Message.Close_case) ]
              [ txt "Fermer le dossier" ]
          ]
      ]
  ; div ~a:[ class_ "action-pan" ] content
  ; fieldset
      ~a:[ class_ "case" ]
      [
        legend [ txt ("Dossier de " ^ case_name) ]
      ; div
          ~a:[ class_ "case-period" ]
          (List.mapi
             (fun i
                  Data.Stored_case.
                    {
                      quarter = s, e
                    ; total_na
                    ; total_quarter
                    ; total
                    ; offset
                    ; entries
                    } ->
               div
                 ~a:[ class_ "quarter" ]
                 [
                   div
                     ~a:[ class_ "quarter-title" ]
                     [
                       div [ txt (Format.asprintf "Trimestre %d" (succ i)) ]
                     ; div [ txt @@ Date.to_string s ]
                     ; div [ txt @@ Date.to_string e ]
                     ]
                 ; div
                     ~a:[ class_ "entries" ]
                     [
                       div
                         [
                           div [ txt "N° ordre" ]
                         ; div [ txt "Contrat joint" ]
                         ; div [ txt "C4 joint" ]
                         ; div [ txt "Période" ]
                         ; div [ txt "Montant du cachet" ]
                         ; div [ txt "Montant brut" ]
                         ; div [ txt "Jours saisis" ]
                         ; div [ txt "Jours éligibles" ]
                         ]
                     ; div (List.mapi (render_entry offset) entries)
                     ]
                 ; div
                     ~a:[ class_ "synthesis" ]
                     [
                       div
                         ~a:[ class_ "total" ]
                         [
                           span [ txt "trimestre:" ]
                         ; span [ txt (total_quarter |> Num.to_string) ]
                         ; span [ txt "/78" ]
                         ]
                     ; div
                         ~a:[ class_ "total" ]
                         [
                           span [ txt "non-artistique" ]
                         ; span [ txt (total_na |> Num.to_string) ]
                         ; span [ txt "/52" ]
                         ]
                     ; div
                         ~a:[ class_ "total" ]
                         [
                           span [ txt "général:" ]
                         ; span [ txt (total |> Num.to_string) ]
                         ; span [ txt "/156" ]
                         ]
                     ]
                 ])
             quarters)
      ]
  ]

let render_opened case = active_template case []

let render_range_label case days_5dw range is_non_artistic =
  let open Html in
  match range with
  | None -> []
  | Some (Ok (_, _, i)) ->
      let xs =
        match days_5dw with
        | None -> []
        | Some days_5dw ->
            let days_6dw = Model.five_to_six days_5dw in
            let suff = if Num.(days_6dw >= from_int 2) then "s" else "" in
            if
              is_non_artistic
              && Num.(
                   Data.Stored_case.total_days_non_art case + days_6dw
                   >= Num.from_int 52)
            then
              [
                div
                  ~a:[ class_ "error" ]
                  [
                    txt
                      "Vous ne pouvez pas dépasser 52 jours de travail \
                       non-artistique"
                  ]
              ]
            else if
              Num.(
                Data.Stored_case.total_days_by_quarter i case + days_6dw
                >= Num.from_int 78)
            then
              [
                div
                  ~a:[ class_ "error" ]
                  [
                    txt
                      "Vous ne pouvez pas dépasser 78 jours de travail en un \
                       semestre"
                  ]
              ]
            else
              [
                span [ txt @@ Format.asprintf "%a" Num.pp days_6dw ]
              ; span [ txt (" jour" ^ suff ^ " éligible" ^ suff) ]
              ]
      in
      [
        div
          ~a:[ class_ "storeable-days" ]
          ([ span [ txt @@ Format.asprintf "Pour le trimestre %d, " (succ i) ] ]
          @ xs)
      ]
  | Some (Error x) -> [ div ~a:[ class_ "error" ] [ txt x ] ]

let render_duration_button case range days_5dw is_non_artistic =
  let open Html in
  let args =
    match (range, days_5dw) with
    | Some (Ok (_, _, i)), Some d ->
        let days_6dw = Model.five_to_six d in
        if
          (is_non_artistic
          && Num.(
               Data.Stored_case.total_days_non_art case + days_6dw
               >= Num.from_int 52))
          || Num.(
               Data.Stored_case.total_days_by_quarter i case + days_6dw
               >= Num.from_int 78)
        then [ disabled true ]
        else [ disabled false; onclick (fun _ -> Message.Save_duration_entry) ]
    | _ -> [ disabled true ]
  in
  [ button ~a:args [ txt "Ajouter l'entrée" ] ]

let render_enter_by_duration case sd ed range days_5dw has_c4 has_contract
    is_non_artistic =
  let open Html in
  let non_art_check =
    let total = Data.Stored_case.total_days_non_art case in
    if Num.(total < from_int 52) then
      [
        checkbox
          ~a:[ onchange_checked (fun value -> Message.Check_is_artistic value) ]
          is_non_artistic ()
      ; label [ txt "Prestation non-artistique" ]
      ]
    else
      [
        checkbox ~a:[ disabled true ] false ()
      ; label
          ~a:[ class_ "disabled-label" ]
          [ txt "Prestation non-artistique (< 52 jours)" ]
      ]
  in
  active_template case
    [
      fieldset
        [
          legend [ txt "Saisir une prestation à la durée" ]
        ; div ~a:[]
            [
              div
                ~a:[ class_ "combo-date" ]
                [
                  label ~a:[ for_ "case_start" ] [ txt "Début: " ]
                ; input
                    ~a:
                      [
                        name "case_start"
                      ; id "case_start"
                      ; value sd
                      ; placeholder "dd/mm/yyyy"
                      ; oninput (fun s -> Message.Fill_duration_start s)
                      ]
                    []
                ; label ~a:[ for_ "case_end" ] [ txt "Fin: " ]
                ; input
                    ~a:
                      [
                        name "case_end"
                      ; id "case_end"
                      ; value ed
                      ; placeholder "dd/mm/yyyy"
                      ; oninput (fun s -> Message.Fill_duration_end s)
                      ]
                    []
                ; label ~a:[ for_ "case_days" ] [ txt "Jours prestés (5j): " ]
                ; input
                    ~a:
                      [
                        name "case_days"
                      ; id "case_days"
                      ; placeholder "En semaine de 5 jours"
                      ; oninput (fun x -> Message.Fill_days x)
                      ; type_ "number"
                      ; value
                          (Option.fold ~none:"" ~some:string_of_float days_5dw)
                      ]
                    []
                ]
            ; div
                ~a:[ class_ "joint-doc" ]
                [
                  div
                    [
                      checkbox
                        ~a:
                          [
                            onchange_checked (fun value ->
                                Message.Check_contract value)
                          ]
                        has_contract ()
                    ; label [ txt "Contrat de travail" ]
                    ]
                ; div
                    [
                      checkbox
                        ~a:
                          [
                            onchange_checked (fun value ->
                                Message.Check_c4 value)
                          ]
                        has_c4 ()
                    ; label [ txt "C4" ]
                    ]
                ; div non_art_check
                ]
            ; div (render_range_label case days_5dw range is_non_artistic)
            ; div (render_duration_button case range days_5dw is_non_artistic)
            ]
        ]
    ]

let render_fee_error Model.{ result; _ } =
  let open Html in
  match result with
  | None -> [ button ~a:[ disabled true ] [ txt "Ajouter l'entrée" ] ]
  | Some (Ok result) ->
      let suff = if Num.(result.eligible > Num.from_int 1) then "s" else "" in
      [
        div
          ~a:[ class_ "storeable-days" ]
          [
            div ~a:[]
              [
                div [ txt "Salaire journalier de référence" ]
              ; div [ txt "TVA Appliquée" ]
              ; div [ txt "Frais de secrétariat sociaux" ]
              ; div [ txt "Charges patronales" ]
              ; div [ txt "Montant brut" ]
              ; div [ txt "Jours éligibles" ]
              ]
          ; div
              [
                div
                  [
                    txt @@ Format.asprintf "%a €" Num.pp result.ref_daily_salary
                  ]
              ; div [ txt @@ Format.asprintf "%a €" Num.pp result.applied_tva ]
              ; div
                  [
                    txt @@ Format.asprintf "%a €" Num.pp result.secretariat_fee
                  ]
              ; div
                  [ txt @@ Format.asprintf "%a €" Num.pp result.employer_cost ]
              ; div [ txt @@ Format.asprintf "%a €" Num.pp result.gross ]
              ; div
                  [
                    txt
                    @@ Format.asprintf "%a jour%s" Num.pp result.eligible suff
                  ]
              ]
          ; div
              [
                div
                  [
                    button
                      ~a:
                        [
                          disabled false
                        ; onclick (fun _ -> Message.Save_fee_entry)
                        ]
                      [ txt "Ajouter l'entrée" ]
                  ]
              ]
          ]
      ]
  | Some (Error err) ->
      [
        div ~a:[ class_ "error" ] [ txt err ]
      ; button ~a:[ disabled true ] [ txt "Ajouter l'entrée" ]
      ]

let render_salary_ref date_str =
  let date_ref =
    let open Util.Option in
    let* d = Date.from_string date_str |> Result.to_option in
    let+ r = Temporal_db.find_maximal_after Config.daily_reference_salary d in
    fst r
  in
  let open Html in
  [
    div ~a:[ class_ "temporal-db" ] [ txt "Salaires journaliers de références" ]
  ; div
      ~a:[ class_ "temporal-db-salary" ]
      (Temporal_db.to_list Config.daily_reference_salary
      |> List.map (fun (date, num) ->
             let a =
               if Option.equal Date.equal date_ref (Some date) then
                 [ class_ "selected" ]
               else []
             in
             div ~a
               [
                 div [ txt @@ Format.asprintf "À partir du %a" Date.pp date ]
               ; div [ txt @@ Format.asprintf "%a €" Num.pp num ]
               ]))
  ]

let render_entry_by_fee
    Model.(
      {
        case
      ; date_str
      ; amount_gross_gross_str
      ; tva_included
      ; has_contract
      ; has_c4
      ; social_secretary
      ; _
      } as k) =
  let open Html in
  active_template case
    [
      fieldset
        [
          legend [ txt "Saisir une prestation au cachet" ]
        ; div ~a:[]
            [
              div
                [
                  div
                    [
                      div
                        ~a:[ class_ "combo-date" ]
                        [
                          label
                            ~a:[ for_ "case_start" ]
                            [ txt "Date de facturation: " ]
                        ; input
                            ~a:
                              [
                                name "case_start"
                              ; id "case_start"
                              ; value date_str
                              ; placeholder "dd/mm/yyyy"
                              ; oninput (fun x -> Message.Fill_fee_date x)
                              ]
                            []
                        ; label
                            ~a:[ for_ "case_amount" ]
                            [ txt "Montant (brut brut)" ]
                        ; input
                            ~a:
                              [
                                name "case_amount"
                              ; id "case_amount"
                              ; value amount_gross_gross_str
                              ; placeholder "Montant brut brut"
                              ; oninput (fun x -> Message.Fill_fee_amount x)
                              ]
                            []
                        ]
                    ; div
                        [
                          div
                            ~a:[ class_ "joint-doc" ]
                            [
                              div
                                [
                                  optional_from_map
                                    ~a:
                                      [
                                        oninput (fun x ->
                                            Message.Fill_secretary x)
                                      ]
                                    social_secretary Config.social_secretary
                                ; label [ txt "Secrétariat social" ]
                                ]
                            ; div
                                [
                                  checkbox
                                    ~a:
                                      [
                                        onchange_checked (fun value ->
                                            Message.Check_tva value)
                                      ]
                                    tva_included ()
                                ; label
                                    [ txt "TVA appliquée au montant du cachet" ]
                                ]
                            ; div
                                [
                                  checkbox
                                    ~a:
                                      [
                                        onchange_checked (fun value ->
                                            Message.Check_contract value)
                                      ]
                                    has_contract ()
                                ; label [ txt "Contrat de travail" ]
                                ]
                            ; div
                                [
                                  checkbox
                                    ~a:
                                      [
                                        onchange_checked (fun value ->
                                            Message.Check_c4 value)
                                      ]
                                    has_c4 ()
                                ; label [ txt "C4" ]
                                ]
                            ]
                        ]
                    ; div (render_fee_error k)
                    ]
                ]
            ; div (render_salary_ref date_str)
            ]
        ]
    ]

let from_model = function
  | Model.Opened { case } -> render_opened case
  | Model.Entry_by_duration
      {
        case
      ; start_date_str
      ; end_date_str
      ; range
      ; days_5dw
      ; has_c4
      ; has_contract
      ; is_non_artistic
      } ->
      render_enter_by_duration case start_date_str end_date_str range days_5dw
        has_c4 has_contract is_non_artistic
  | Model.Entry_by_fee k -> render_entry_by_fee k
  | Model.Not_opened { identifier; period; cases } ->
      render_not_opened identifier period cases

let view model = Html.div @@ from_model model