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

type 'a inputable = { str : string; repr : ('a, string) result option }

let make_empty_input () = { str = ""; repr = None }
let make_filled_input str repr = { str; repr }

type hours_to_days = {
    htd_hours : Num.t inputable
  ; htd_avg : Num.t inputable
  ; htd_result : Num.t option
}

type from_nod_result = {
    r_nod_brut : Num.t
  ; r_nod_tva : Num.t
  ; r_nod_social : Num.t
  ; r_nod_cost : Num.t
  ; r_nod_cachet : Num.t
}

type from_nod = {
    nod_days : Num.t inputable
  ; nod_social : string option
  ; nod_social_percent : Percent.t inputable
  ; nod_date_ref : Date.t option
  ; nod_salary_ref : Num.t inputable
  ; nod_result : from_nod_result option
}

type from_gross_result = {
    r_gross_days : Num.t
  ; r_gross : Num.t
  ; r_gross_tva : Num.t
  ; r_gross_social : Num.t
  ; r_gross_cost : Num.t
}

type from_gross_gross = {
    gross_amount : Num.t inputable
  ; gross_social : string option
  ; gross_social_percent : Percent.t inputable
  ; gross_salary_ref : Num.t inputable
  ; gross_tva : bool
  ; gross_result : from_gross_result option
}

type t = {
    hours_to_days : hours_to_days
  ; from_nod : from_nod
  ; from_gross_gross : from_gross_gross
  ; nop : unit
}

let init () =
  let salary = Temporal_db.find_last Config.daily_reference_salary in
  let nod_salary_ref =
    Option.fold ~none:(make_empty_input ())
      ~some:(fun (_, x) -> make_filled_input (Num.to_string x) (Some (Ok x)))
      salary
  in
  {
    hours_to_days =
      {
        htd_hours = make_empty_input ()
      ; htd_avg = make_filled_input "7,6" (Some (Ok (Num.from_float 7.6)))
      ; htd_result = None
      }
  ; from_nod =
      {
        nod_days = make_empty_input ()
      ; nod_social = Some "Smart"
      ; nod_social_percent =
          make_filled_input "6,5" (Some (Ok (Percent.from_float 6.5)))
      ; nod_date_ref = None
      ; nod_salary_ref
      ; nod_result = None
      }
  ; from_gross_gross =
      {
        gross_amount = make_empty_input ()
      ; gross_social = Some "Smart"
      ; gross_social_percent =
          make_filled_input "6,5" (Some (Ok (Percent.from_float 6.5)))
      ; gross_salary_ref = nod_salary_ref
      ; gross_tva = true
      ; gross_result = None
      }
  ; nop = ()
  }

let validate_num str =
  let repr =
    let open Util.Result in
    let* x = Num.from_string str in
    if Num.(x > from_int 0) then Ok x else Error (`Negative (Num.to_string x))
  in
  let repr =
    Result.map_error
      (function
        | `Num_invalid_string x -> Format.asprintf "[%s] n'est pas un nombre" x
        | `Negative x -> Format.asprintf "[%s] ne peut pas être négatif" x)
      repr
  in
  { str; repr = Some repr }

let validate_percent str =
  let repr =
    let open Util.Result in
    let* x =
      str
      |> Util.replace_char ~from:',' ~by:'.'
      |> Float.of_string_opt
      |> Option.to_result ~none:(`Invalid str)
    in
    if x > 0.0 then Ok (Percent.from_float x) else Error (`Negative str)
  in
  let repr =
    Result.map_error
      (function
        | `Invalid x -> Format.asprintf "[%s] n'est pas un pourcentage" x
        | `Negative x -> Format.asprintf "[%s] ne peut pas être négatif" x)
      repr
  in
  { str; repr = Some repr }

let compute_hours_to_days ({ htd_hours; htd_avg; _ } as model) =
  let htd_result =
    let open Util.Option in
    let* hours = htd_hours.repr >>= Result.to_option in
    let+ avg = htd_avg.repr >>= Result.to_option in
    Num.(hours / avg)
  in
  { model with htd_result }

let compute_nod ({ nod_days; nod_social_percent; nod_salary_ref; _ } as model) =
  let nod_result =
    let open Util.Option in
    let* days = nod_days.repr >>= Result.to_option in
    let* socp = nod_social_percent.repr >>= Result.to_option in
    let+ salr = nod_salary_ref.repr >>= Result.to_option in
    let r_nod_brut = Num.(days * salr) in
    let r_nod_tva = Percent.(apply @@ from_int 6) r_nod_brut in
    let r_nod_social = Percent.apply socp Num.(r_nod_brut - r_nod_tva) in
    let cost = Num.(r_nod_brut - r_nod_tva - r_nod_social) in
    let r_nod_cost = Num.(cost - Percent.(apply (from_float 36.3) cost)) in
    let r_nod_cachet =
      Num.(r_nod_brut + r_nod_social + r_nod_tva + r_nod_cost)
    in
    { r_nod_brut; r_nod_tva; r_nod_social; r_nod_cost; r_nod_cachet }
  in
  { model with nod_result }

let compute_gross
    ({ gross_amount; gross_social_percent; gross_salary_ref; gross_tva; _ } as
    model) =
  let gross_result =
    let open Util.Option in
    let* amount = gross_amount.repr >>= Result.to_option in
    let tva = if gross_tva then Percent.from_int 6 else Percent.from_int 0 in
    let* socp = gross_social_percent.repr >>= Result.to_option in
    let+ salr = gross_salary_ref.repr >>= Result.to_option in
    let r_gross_tva = Percent.(apply tva) amount in
    let r_gross_social = Percent.apply socp Num.(amount - r_gross_tva) in
    let cost = Num.(amount - r_gross_tva - r_gross_social) in
    let r_gross_cost = Num.(cost - Percent.(apply (from_float 36.3) cost)) in
    let r_gross = Num.(amount - r_gross_social - r_gross_tva - r_gross_cost) in
    let r_gross_days = Num.(r_gross_cost / salr) in
    { r_gross; r_gross_tva; r_gross_social; r_gross_cost; r_gross_days }
  in
  { model with gross_result }

let update_htd hours_to_days = function
  | Message.Htd_fill_hours value ->
      compute_hours_to_days
        { hours_to_days with htd_hours = validate_num value }
  | Message.Htd_fill_avg value ->
      compute_hours_to_days { hours_to_days with htd_avg = validate_num value }
  | _ -> hours_to_days

let update_nod from_nod = function
  | Message.Nod_fill_days value ->
      compute_nod { from_nod with nod_days = validate_num value }
  | Nod_change_social value ->
      let nod_social, nod_social_percent =
        Smap.find_opt value Config.social_secretary
        |> Option.fold
             ~none:
               (None, make_filled_input "0.0" (Some (Ok (Percent.from_int 0))))
             ~some:(fun x ->
               ( Some value
               , make_filled_input (Percent.to_string' x) (Some (Ok x)) ))
      in
      compute_nod { from_nod with nod_social; nod_social_percent }
  | Nod_change_social_percent value ->
      compute_nod
        {
          from_nod with
          nod_social_percent = validate_percent value
        ; nod_social = None
        }
  | Nod_change_date_ref value ->
      let nod_date_ref, nod_salary_ref =
        match Date.from_string value with
        | Error _ -> (None, from_nod.nod_salary_ref)
        | Ok date -> (
            match Temporal_db.find_for Config.daily_reference_salary date with
            | Temporal_db.Found (date, value) | Out_of_bound (date, value) ->
                ( Some date
                , make_filled_input (Num.to_string value) (Some (Ok value)) ))
      in
      compute_nod { from_nod with nod_date_ref; nod_salary_ref }
  | Nod_change_salary_ref value ->
      compute_nod
        {
          from_nod with
          nod_salary_ref = validate_num value
        ; nod_date_ref = None
        }
  | _ -> from_nod

let update_gross from_gross = function
  | Message.Gross_fill_amount value ->
      compute_gross { from_gross with gross_amount = validate_num value }
  | Gross_change_social value ->
      let gross_social, gross_social_percent =
        Smap.find_opt value Config.social_secretary
        |> Option.fold
             ~none:
               (None, make_filled_input "0.0" (Some (Ok (Percent.from_int 0))))
             ~some:(fun x ->
               ( Some value
               , make_filled_input (Percent.to_string' x) (Some (Ok x)) ))
      in
      compute_gross { from_gross with gross_social; gross_social_percent }
  | Gross_change_social_percent value ->
      compute_gross
        {
          from_gross with
          gross_social_percent = validate_percent value
        ; gross_social = None
        }
  | Gross_change_salary_ref value ->
      compute_gross { from_gross with gross_salary_ref = validate_num value }
  | Gross_change_tva value ->
      compute_gross { from_gross with gross_tva = value }
  | _ -> from_gross

let update ({ hours_to_days; from_nod; from_gross_gross; _ } as model) message =
  match message with
  | Message.Htd_fill_hours _ | Htd_fill_avg _ ->
      { model with hours_to_days = update_htd hours_to_days message }
  | Message.Nod_fill_days _ | Nod_change_social _ | Nod_change_social_percent _
  | Nod_change_date_ref _ | Nod_change_salary_ref _ ->
      { model with from_nod = update_nod from_nod message }
  | Message.Gross_fill_amount _ | Gross_change_social _
  | Gross_change_social_percent _ | Gross_change_tva _
  | Gross_change_salary_ref _ ->
      { model with from_gross_gross = update_gross from_gross_gross message }
  | _ -> model
