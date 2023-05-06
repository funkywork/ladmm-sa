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

let confirm message yes no =
  let open Js_of_ocaml in
  let x = Dom_html.window##confirm (Js.string message) in
  if Js.to_bool x then yes () else no ()

let alert message =
  let open Js_of_ocaml in
  Dom_html.window##alert (Js.string message)

let five_to_six days_5dw =
  let d5 = Num.from_float days_5dw in
  let five, six = (Num.from_int 5, Num.from_int 6) in
  Num.(d5 * six / five)

type fee_result = {
    applied_tva : Num.t
  ; secretariat_fee : Num.t
  ; employer_cost : Num.t
  ; gross : Num.t
  ; gross_gross : Num.t
  ; ref_daily_salary : Num.t
  ; eligible : Num.t
  ; quarter : int
  ; date : Date.t
}

type entry_by_fee = {
    case : Data.Stored_case.t
  ; date_str : string
  ; amount_gross_gross_str : string
  ; daily_salary_ref_str : string
  ; tva_included : bool
  ; social_secretary : string option
  ; has_contract : bool
  ; has_c4 : bool
  ; result : (fee_result, string) result option
}

type t =
  | Opened of { case : Data.Stored_case.t }
  | Entry_by_fee of entry_by_fee
  | Entry_by_duration of {
        case : Data.Stored_case.t
      ; start_date_str : string
      ; end_date_str : string
      ; range : (Date.t * Date.t * int, string) result option
      ; has_contract : bool
      ; has_c4 : bool
      ; is_non_artistic : bool
      ; days_5dw : float option
      ; days_5dw_str : string
    }
  | Not_opened of {
        identifier : string * (Ident.t, Sigs.ident_error) result
      ; period : string * (Quarters.t, Sigs.date_error) result
      ; cases : Data.Case_line.t list
    }

let init () =
  let cases = Data.Case_line.get_all () in
  let empty = "" in
  Not_opened
    {
      identifier = (empty, Ident.make empty)
    ; period = (empty, Util.Result.(Date.from_string empty >|= Quarters.init))
    ; cases
    }

let discard model = model

let update_fill_ident cases period value =
  let ident = Ident.make value in
  let identifier = (value, ident) in
  Not_opened { period; identifier; cases }

let update_fill_date cases identifier value =
  let quarters =
    let open Util.Result in
    let+ date = Date.from_string value in
    Quarters.init date
  in
  let period = (value, quarters) in
  Not_opened { identifier; period; cases }

let update_create_case (_, identifier) (_, period) model =
  let open Util.Result in
  let result =
    let* identifier = identifier in
    let+ period = period in
    (identifier, period)
  in
  match result with
  | Ok (identifier, period) ->
      let case = Data.Stored_case.init identifier period in
      let index = case.Data.Stored_case.index in
      let () = Data.Stored_ident.add index in
      let () = Data.Stored_case.save case in
      init ()
  | Error _ -> model

let update_delete_case index model =
  confirm "Êtes-vous sur de vouloir supprimer l'entrée ?"
    (fun () ->
      let () = Data.Stored_ident.delete index in
      let () = Data.Stored_case.delete index in
      init ())
    (fun () -> model)

let update_open_case index previous_model =
  match Data.Stored_case.get index with
  | None ->
      let () = alert ("Impossible de trouver le dossier " ^ index) in
      previous_model
  | Some case -> Opened { case }

let update_not_opened identifier period cases model =
  let identifier : string * (Ident.t, Sigs.ident_error) result :>
      string * (Ident.t, [> Sigs.ident_error ]) result =
    identifier
  and period : string * (Quarters.t, Sigs.date_error) result :>
      string * (Quarters.t, [> Sigs.date_error ]) result =
    period
  in
  function
  | Message.Not_open_fill_ident value -> update_fill_ident cases period value
  | Message.Not_open_fill_date value -> update_fill_date cases identifier value
  | Message.Create_case -> update_create_case identifier period model
  | Message.Delete_case index -> update_delete_case index model
  | Message.Open_case index -> update_open_case index model
  | _ -> discard model

let update_opened _case model = function
  | Message.Close_case -> init ()
  | _ -> discard model

let compute_range quarters a b =
  let res =
    let open Util.Result in
    let* s = Date.from_string a in
    let* e = Date.from_string b in
    let+ t = Quarters.get_by_range quarters s e in
    (s, e, t)
  in
  match res with
  | Error (#Sigs.date_error as e) ->
      Error (Format.asprintf "%a" Date.pp_error e)
  | Error (#Sigs.quarters_error as e) ->
      Error (Format.asprintf "%a" Quarters.pp_error e)
  | Ok x -> Ok x

let other_value result d =
  match result with
  | Ok x -> if String.equal "" (String.trim d) then x |> Date.to_string else d
  | _ -> d

let compute_days5 d5 range =
  match range with
  | Some (Ok (x, y, _)) ->
      let Workdays.{ workdays; _ } = Workdays.from_range x y in
      Some (workdays |> Float.of_int)
  | _ -> d5

let update_save_duration_entry case range days_5dw has_c4 has_contract
    is_non_artistic model =
  let res =
    let open Util.Option in
    let* days_5dw = days_5dw in
    let* range = range in
    let+ range = Result.to_option range in
    (Num.from_float days_5dw, five_to_six days_5dw, range)
  in
  match res with
  | None -> model
  | Some (raw_days, days, (s, e, quarter)) ->
      let id = Data.uniq_id () in
      let range = (s, e) in
      let is_artistic = not is_non_artistic in
      let entry =
        Data.Entry.duration ~id ~has_contract ~has_c4 ~is_artistic ~range
          ~quarter ~days ~raw_days
      in
      let new_case = Data.Stored_case.add_entry case entry in
      let () = Data.Stored_case.save new_case in
      Opened { case = new_case }

let update_enter_by_duration case s e range days_5dw days_5dw_str has_c4
    has_contract is_non_artistic model = function
  | Message.Fill_duration_start value ->
      let end_date_str = other_value (Date.from_string value) e in
      let range =
        compute_range case.Data.Stored_case.quarters value end_date_str
        |> Option.some
      in
      let days_5dw = compute_days5 days_5dw range in
      Entry_by_duration
        {
          case
        ; start_date_str = value
        ; end_date_str
        ; range
        ; days_5dw
        ; days_5dw_str =
            Option.fold ~none:days_5dw_str ~some:string_of_float days_5dw
        ; has_c4
        ; has_contract
        ; is_non_artistic
        }
  | Message.Fill_duration_end value ->
      let start_date_str = other_value (Date.from_string value) s in
      let range =
        compute_range case.Data.Stored_case.quarters start_date_str value
        |> Option.some
      in
      let days_5dw = compute_days5 days_5dw range in
      Entry_by_duration
        {
          case
        ; start_date_str
        ; end_date_str = value
        ; range
        ; days_5dw
        ; days_5dw_str =
            Option.fold ~none:days_5dw_str ~some:string_of_float days_5dw
        ; has_c4
        ; has_contract
        ; is_non_artistic
        }
  | Message.Fill_days x ->
      let days_5dw =
        match float_of_string_opt x with
        | Some x -> if x > 0.0 then Some x else None
        | None -> None
      in
      Entry_by_duration
        {
          case
        ; start_date_str = s
        ; end_date_str = e
        ; range
        ; days_5dw
        ; days_5dw_str = x
        ; has_c4
        ; has_contract
        ; is_non_artistic
        }
  | Message.Check_contract value ->
      Entry_by_duration
        {
          case
        ; start_date_str = s
        ; end_date_str = e
        ; range
        ; days_5dw
        ; days_5dw_str
        ; has_c4
        ; has_contract = value
        ; is_non_artistic
        }
  | Message.Check_c4 value ->
      Entry_by_duration
        {
          case
        ; start_date_str = s
        ; end_date_str = e
        ; range
        ; days_5dw
        ; has_c4 = value
        ; days_5dw_str
        ; has_contract
        ; is_non_artistic
        }
  | Message.Check_is_artistic value ->
      Entry_by_duration
        {
          case
        ; start_date_str = s
        ; end_date_str = e
        ; range
        ; days_5dw
        ; days_5dw_str
        ; has_c4
        ; has_contract
        ; is_non_artistic = value
        }
  | Message.Save_duration_entry ->
      update_save_duration_entry case range days_5dw has_c4 has_contract
        is_non_artistic model
  | Message.Close_case -> init ()
  | _ -> discard model

let compute_fee_result ?(update_ref_salary = false) k =
  let open Util.Result in
  let ref_s =
    let* date = Date.from_string k.date_str in
    let ref_daily_salary =
      match Temporal_db.find_for Config.daily_reference_salary date with
      | Found (_, x) -> x
      | _ -> Num.from_float 73.72
    in
    if update_ref_salary then Ok ref_daily_salary
    else Num.from_string k.daily_salary_ref_str
  in
  let res =
    let* date = Date.from_string k.date_str in
    let* qi = Quarters.get_by_date k.case.quarters date in
    let* ref_daily_salary = ref_s in
    let* gross_gross =
      match float_of_string_opt k.amount_gross_gross_str with
      | None -> Error (`Invalid_amount k.amount_gross_gross_str)
      | Some x ->
          if x < 0.0 then Error (`Invalid_amount k.amount_gross_gross_str)
          else Ok (Num.from_float x)
    in
    let pc_tva =
      if k.tva_included then Percent.from_int 6 else Percent.from_int 0
    in
    let applied_tva = Percent.apply pc_tva gross_gross in
    let secretariat_pc_opt =
      let open Util.Option in
      let* v = k.social_secretary in
      let+ v = Smap.find_first_opt (String.equal v) Config.social_secretary in
      snd v
    in
    let pc_secretariat =
      Option.value ~default:(Percent.from_int 0) secretariat_pc_opt
    in
    let secretariat_fee =
      Percent.apply pc_secretariat Num.(gross_gross - applied_tva)
    in
    let employer_cost =
      Percent.(apply (from_float 36.30))
        Num.(gross_gross - applied_tva - secretariat_fee)
    in
    let gross =
      Num.(gross_gross - applied_tva - secretariat_fee - employer_cost)
    in

    let eligible = Num.(gross / ref_daily_salary) in
    let num = Data.Stored_case.total_days_by_quarter qi k.case in
    let+ () =
      if Num.(num + eligible >= Num.from_int 78) then
        Error (`High (num, eligible))
      else Ok ()
    in

    {
      gross_gross
    ; applied_tva
    ; secretariat_fee
    ; employer_cost
    ; gross
    ; ref_daily_salary
    ; eligible
    ; quarter = qi
    ; date
    }
  in

  {
    k with
    daily_salary_ref_str =
      (if update_ref_salary then
         Result.fold
           ~error:(fun _ -> k.daily_salary_ref_str)
           ~ok:(fun ref_daily_salary ->
             Format.asprintf "%a" Num.pp ref_daily_salary)
           ref_s
       else k.daily_salary_ref_str)
  ; result =
      Some
        (match res with
        | Ok x -> Ok x
        | Error (`Invalid_amount s) ->
            Error (Format.asprintf "Montant [%s] invalide" s)
        | Error (#Sigs.quarters_error as e) ->
            Error (Format.asprintf "%a" Quarters.pp_error e)
        | Error (#Sigs.date_error as e) ->
            Error (Format.asprintf "%a" Date.pp_error e)
        | Error (`Num_invalid_string s) ->
            Error
              (Format.asprintf "Salaire journalier de référence: [%s] invalide"
                 s)
        | Error (`High (num, el)) ->
            Error
              (Format.asprintf
                 "Vous ne pouvez pas dépasser 78 jours de travail en un \
                  semestre (%a + %a)"
                 Num.pp num Num.pp el))
  }

let update_fee k model = function
  | Message.Fill_ref_salary s ->
      let r = { k with daily_salary_ref_str = s } in
      Entry_by_fee (compute_fee_result r)
  | Message.Fill_fee_date s ->
      let r = { k with date_str = s } in
      Entry_by_fee (compute_fee_result ~update_ref_salary:true r)
  | Message.Fill_fee_amount s ->
      let r = { k with amount_gross_gross_str = s } in
      Entry_by_fee (compute_fee_result r)
  | Message.Check_c4 s ->
      let r = { k with has_c4 = s } in
      Entry_by_fee (compute_fee_result r)
  | Message.Check_contract s ->
      let r = { k with has_contract = s } in
      Entry_by_fee (compute_fee_result r)
  | Message.Check_tva s ->
      let r = { k with tva_included = s } in
      Entry_by_fee (compute_fee_result r)
  | Message.Fill_secretary s ->
      let v =
        let open Util.Option in
        Smap.find_first_opt (String.equal s) Config.social_secretary >|= fst
      in
      let r = { k with social_secretary = v } in
      Entry_by_fee (compute_fee_result r)
  | Message.Save_fee_entry -> (
      match k.result with
      | Some (Ok x) ->
          let entry =
            let id = Data.uniq_id () in
            Data.Entry.fee ~id ~has_contract:k.has_contract ~has_c4:k.has_c4
              ~quarter:x.quarter ~days:x.eligible ~gross:x.gross
              ~gross_gross:x.gross_gross ~date:x.date
              ~ref_daily_salary:x.ref_daily_salary
          in
          let new_case = Data.Stored_case.add_entry k.case entry in
          let () = Data.Stored_case.save new_case in
          Opened { case = new_case }
      | _ -> model)
  | Message.Close_case -> init ()
  | _ -> model

let update model message =
  match (message, model) with
  | ( Message.Write_by_duration
    , ( Opened { case }
      | Entry_by_duration { case; _ }
      | Entry_by_fee { case; _ } ) ) ->
      Entry_by_duration
        {
          case
        ; start_date_str = ""
        ; end_date_str = ""
        ; range = None
        ; days_5dw = None
        ; days_5dw_str = ""
        ; has_c4 = false
        ; has_contract = false
        ; is_non_artistic = false
        }
  | ( Message.Write_by_fee
    , ( Opened { case }
      | Entry_by_duration { case; _ }
      | Entry_by_fee { case; _ } ) ) ->
      Entry_by_fee
        {
          case
        ; date_str = ""
        ; amount_gross_gross_str = ""
        ; daily_salary_ref_str = ""
        ; tva_included = false
        ; social_secretary = None
        ; has_c4 = false
        ; has_contract = false
        ; result = None
        }
  | Message.Recheck_c4 (key, value), Opened { case } ->
      let new_case = Data.Stored_case.check_c4 case key value in
      let () = Data.Stored_case.save new_case in
      Opened { case = new_case }
  | Message.Recheck_contract (key, value), Opened { case } ->
      let new_case = Data.Stored_case.check_contract case key value in
      let () = Data.Stored_case.save new_case in
      Opened { case = new_case }
  | Message.Recheck_c4 (key, value), Entry_by_duration ({ case; _ } as k) ->
      let new_case = Data.Stored_case.check_c4 case key value in
      let () = Data.Stored_case.save new_case in
      Entry_by_duration { k with case = new_case }
  | Message.Recheck_contract (key, value), Entry_by_duration ({ case; _ } as k)
    ->
      let new_case = Data.Stored_case.check_contract case key value in
      let () = Data.Stored_case.save new_case in
      Entry_by_duration { k with case = new_case }
  | Message.Delete_entry key, Opened { case } ->
      let new_case = Data.Stored_case.delete_entry case key in
      let () = Data.Stored_case.save new_case in
      Opened { case = new_case }
  | Message.Delete_entry key, Entry_by_duration ({ case; _ } as k) ->
      let new_case = Data.Stored_case.delete_entry case key in
      let () = Data.Stored_case.save new_case in
      Entry_by_duration { k with case = new_case }
  | _, Opened { case } -> update_opened case model message
  | ( _
    , Entry_by_duration
        {
          case
        ; start_date_str
        ; end_date_str
        ; days_5dw
        ; range
        ; has_c4
        ; has_contract
        ; is_non_artistic
        ; days_5dw_str
        } ) ->
      update_enter_by_duration case start_date_str end_date_str range days_5dw
        days_5dw_str has_c4 has_contract is_non_artistic model message
  | _, Entry_by_fee k -> update_fee k model message
  | _, Not_opened { identifier; period; cases } ->
      update_not_opened identifier period cases model message
