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

let () =
  Alcotest.run "Ladmm-lib"
    [ Num_test.cases; Percent_test.cases; Date_test.cases ]
