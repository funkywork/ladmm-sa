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

module Stored_ident = struct
  type t = string list

  let key = "ladmm-cases-ident"
  let to_json x = `List (List.map (fun x -> `String x) x)

  let from_json = function
    | `List r ->
        List.fold_left
          (fun acc -> function
            | `String x ->
                let open Util.Option in
                let+ acc = acc in
                x :: acc
            | _ -> None)
          (Some []) r
    | _ -> None

  let to_string x = x |> to_json |> Yojson.Safe.to_string

  let from_string x =
    (try Some (Yojson.Safe.from_string x)
     with e ->
       let () = Js_of_ocaml.Firebug.console##error e in
       None)
    |> Util.Option.bind from_json

  let exists name =
    let res =
      let open Util.Option in
      let* full = Storage.get key in
      let+ full = from_string full in
      List.exists (fun x -> String.equal x name) full
    in
    Option.value ~default:false res

  let make name quarters =
    let name = Ident.slugify name in
    let suff = Quarters.get_name quarters in
    Format.asprintf "%a_%s" Ident.pp name suff

  let add name =
    let res =
      let open Util.Option in
      let* full = Storage.get key in
      from_string full
    in
    let list =
      res
      |> Option.fold ~none:[ name ] ~some:(fun xs -> name :: xs)
      |> to_string
    in
    Storage.set key list

  let can_add name quarters =
    let k = make name quarters in
    not (exists k)

  let get () =
    Util.Option.(Storage.get key >>= from_string) |> Option.value ~default:[]

  let delete index =
    get ()
    |> List.filter (fun x -> not (String.equal x index))
    |> to_string
    |> Storage.set key
end

module Entry = struct
  type t =
    | Duration of {
          has_contract : bool
        ; has_c4 : bool
        ; is_artistic : bool
        ; range : Date.t * Date.t
        ; quarter : int
        ; days : Num.t
      }

  let duration ~has_contract ~has_c4 ~is_artistic ~range ~quarter ~days =
    Duration { has_contract; has_c4; is_artistic; range; quarter; days }

  let to_json = function
    | Duration
        {
          has_contract
        ; has_c4
        ; is_artistic
        ; range = start_date, end_date
        ; quarter
        ; days
        } ->
        `Variant
          ( "duration"
          , Some
              (`Assoc
                [
                  ("has_contract", `Bool has_contract)
                ; ("has_c4", `Bool has_c4)
                ; ("is_artistic", `Bool is_artistic)
                ; ("start_date", `String (Date.to_string start_date))
                ; ("end_date", `String (Date.to_string end_date))
                ; ("quarter", `Int quarter)
                ; ("days", `Float (Num.to_float days))
                ]) )

  let from_json = function
    | `Variant ("duration", Some (`Assoc assoc)) ->
        let open Util.Option in
        let open Yojson.Safe.Util in
        let* has_contract =
          List.assoc_opt "has_contract" assoc >>= to_bool_option
        in
        let* has_c4 = List.assoc_opt "has_c4" assoc >>= to_bool_option in
        let* is_artistic =
          List.assoc_opt "is_artistic" assoc >>= to_bool_option
        in
        let* start_date =
          List.assoc_opt "start_date" assoc >>= to_string_option
        in
        let* end_date = List.assoc_opt "end_date" assoc >>= to_string_option in
        let r =
          let open Util.Result in
          let* s = Date.from_string start_date in
          let+ e = Date.from_string end_date in
          (s, e)
        in
        let* range = Result.to_option r in
        let* quarter = List.assoc_opt "quarter" assoc >>= to_int_option in
        let+ days =
          List.assoc_opt "days" assoc >>= to_float_option >|= Num.from_float
        in
        Duration { has_contract; has_c4; is_artistic; range; quarter; days }
    | _ -> None

  let to_string x = x |> to_json |> Yojson.Safe.to_string

  let from_string x =
    (try Some (Yojson.Safe.from_string x)
     with e ->
       let () = Js_of_ocaml.Firebug.console##error e in
       None)
    |> Util.Option.bind from_json

  let days = function Duration { days; _ } -> days

  let is_complete = function
    | Duration { has_c4; has_contract; _ } -> has_c4 && has_contract
end

module Stored_case = struct
  type t = {
      index : string
    ; name : Ident.t
    ; quarters : Quarters.t
    ; entries : Entry.t list
  }

  let key k = "ladmm-case-list-" ^ k

  let to_json { index; name; quarters; entries } =
    `Assoc
      [
        ("index", `String index)
      ; ("name", `String (Ident.to_string name))
      ; ("quarters", `String (Quarters.first_date quarters |> Date.to_string))
      ; ("entries", `List (List.map Entry.to_json entries))
      ]

  let from_json = function
    | `Assoc assoc ->
        let open Util.Option in
        let open Yojson.Safe.Util in
        let* index = List.assoc_opt "index" assoc >>= to_string_option in
        let* name =
          List.assoc_opt "name" assoc >>= to_string_option >|= Ident.make'
        in
        let* date = List.assoc_opt "quarters" assoc >>= to_string_option in
        let* date = Date.from_string date |> Result.to_option in
        let quarters = Quarters.init date in
        let entries =
          List.assoc_opt "entries" assoc
          >|= to_list
          >|= List.fold_left
                (fun acc elt ->
                  let* acc = acc in
                  let+ entry = Entry.from_json elt in
                  entry :: acc)
                (Some [])
          |> Option.join
          |> Option.value ~default:[]
        in
        Some { index; name; quarters; entries }
    | _ -> None

  let to_string x = x |> to_json |> Yojson.Safe.to_string

  let from_string x =
    (try Some (Yojson.Safe.from_string x)
     with e ->
       let () = Js_of_ocaml.Firebug.console##error e in
       None)
    |> Util.Option.bind from_json

  let init name quarters =
    let index = Stored_ident.make name quarters in
    { index; name; quarters; entries = [] }

  let save case =
    let case_str = to_string case in
    let entry = key case.index in
    Storage.set entry case_str

  let get index =
    let open Util.Option in
    let* value = Storage.get @@ key index in
    from_string value

  let total_days { entries; _ } =
    List.fold_left
      (fun acc -> function Entry.Duration { days; _ } -> Num.(acc + days))
      (Num.from_int 0) entries

  let total_days_by_quarter q { entries; _ } =
    List.fold_left
      (fun acc -> function
        | Entry.Duration { days; quarter; _ } ->
            if Int.equal quarter q then Num.(acc + days) else acc)
      (Num.from_int 0) entries

  let total_days_non_art q { entries; _ } =
    List.fold_left
      (fun acc -> function
        | Entry.Duration { days; quarter; is_artistic; _ } ->
            if quarter <= q && not is_artistic then Num.(acc + days) else acc)
      (Num.from_int 0) entries

  let total_days_acc q { entries; _ } =
    List.fold_left
      (fun acc -> function
        | Entry.Duration { days; quarter; _ } ->
            if quarter <= q then Num.(acc + days) else acc)
      (Num.from_int 0) entries

  let is_complete case =
    Num.(total_days case >= Num.from_int 156)
    && List.for_all Entry.is_complete case.entries

  let delete index = Storage.remove (key index)
  let add_entry case entry = { case with entries = entry :: case.entries }
end

module Case_line = struct
  type t = {
      index : string
    ; name : Ident.t
    ; start_date : Date.t
    ; end_date : Date.t
    ; is_complete : bool
  }

  let from_index index =
    let open Util.Option in
    let+ case = Stored_case.get index in
    let start_date, end_date =
      Quarters.get_interval case.Stored_case.quarters
    in
    {
      index = case.Stored_case.index
    ; name = case.Stored_case.name
    ; start_date
    ; end_date
    ; is_complete = Stored_case.is_complete case
    }

  let get_all () =
    Stored_ident.get ()
    |> List.concat_map (fun index -> Option.to_list @@ from_index index)
    |> List.sort
         (fun
           { start_date = a; is_complete = a_c; _ }
           { start_date = b; is_complete = b_c; _ }
         ->
           let res = Bool.compare a_c b_c in
           if res = 0 then Date.compare a b else res)
end
