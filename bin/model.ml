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
  let d5 = Num.from_int days_5dw in
  let five, six = (Num.from_int 5, Num.from_int 6) in
  Num.(d5 * six / five)

type t =
  | Opened of { case : Data.Stored_case.t }
  | Entry_by_duration of {
        case : Data.Stored_case.t
      ; start_date_str : string
      ; end_date_str : string
      ; range : (Date.t * Date.t * int, string) result option
      ; has_contract : bool
      ; has_c4 : bool
      ; is_non_artistic : bool
      ; days_5dw : int option
    }
  | Not_opened of {
        identifier : string * (Ident.t, Sigs.ident_error) result
      ; period : string * (Quarters.t, Sigs.date_error) result
      ; cases : Data.Case_line.t list
    }

let init () =
  let cases = Data.Case_line.get_all () in
  let () =
    Js_of_ocaml.(Firebug.console##log (Js.string "init", List.length cases))
  in
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

let update_opened case model = function
  | Message.Close_case -> init ()
  | Message.Write_by_duration ->
      Entry_by_duration
        {
          case
        ; start_date_str = ""
        ; end_date_str = ""
        ; range = None
        ; days_5dw = None
        ; has_c4 = false
        ; has_contract = false
        ; is_non_artistic = false
        }
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

let other_value flag result d =
  let f = if flag then Date.prev else Date.next in
  match result with
  | Ok x ->
      if String.equal "" (String.trim d) then x |> f |> Date.to_string else d
  | _ -> d

let compute_days5 d5 range =
  match range with
  | Some (Ok (x, y, _)) ->
      let Workdays.{ workdays; _ } = Workdays.from_range x y in
      Some workdays
  | _ -> d5

let update_save_duration_entry case range days_5dw has_c4 has_contract
    is_non_artistic model =
  let res =
    let open Util.Option in
    let* days_5dw = days_5dw in
    let* range = range in
    let+ range = Result.to_option range in
    (five_to_six days_5dw, range)
  in
  match res with
  | None -> model
  | Some (days, (s, e, quarter)) ->
      let range = (s, e) in
      let is_artistic = not is_non_artistic in
      let entry =
        Data.Entry.duration ~has_contract ~has_c4 ~is_artistic ~range ~quarter
          ~days
      in
      let new_case = Data.Stored_case.add_entry case entry in
      let () = Data.Stored_case.save new_case in
      Opened { case = new_case }

let update_enter_by_duration case s e range days_5dw has_c4 has_contract
    is_non_artistic model = function
  | Message.Fill_duration_start value ->
      let end_date_str = other_value false (Date.from_string value) e in
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
        ; has_c4
        ; has_contract
        ; is_non_artistic
        }
  | Message.Fill_duration_end value ->
      let start_date_str = other_value true (Date.from_string value) s in
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
        ; has_c4
        ; has_contract
        ; is_non_artistic
        }
  | Message.Fill_days x ->
      let days_5dw =
        match int_of_string_opt x with
        | Some x -> if x > 0 then Some x else None
        | None -> None
      in
      Entry_by_duration
        {
          case
        ; start_date_str = s
        ; end_date_str = e
        ; range
        ; days_5dw
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
        ; has_c4
        ; has_contract
        ; is_non_artistic = value
        }
  | Message.Save_duration_entry ->
      update_save_duration_entry case range days_5dw has_c4 has_contract
        is_non_artistic model
  | Message.Close_case -> init ()
  | _ -> discard model

let update model message =
  match model with
  | Opened { case } -> update_opened case model message
  | Entry_by_duration
      {
        case
      ; start_date_str
      ; end_date_str
      ; days_5dw
      ; range
      ; has_c4
      ; has_contract
      ; is_non_artistic
      } ->
      update_enter_by_duration case start_date_str end_date_str range days_5dw
        has_c4 has_contract is_non_artistic model message
  | Not_opened { identifier; period; cases } ->
      update_not_opened identifier period cases model message
