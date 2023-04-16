open Alcotest
open Ladmm_lib

let from_int_1 =
  test_case "from int 1" `Quick (fun () ->
      let expected = "33.00%" in
      let computed = Percent.(33 |> from_int |> to_string) in
      check string "should be equal" expected computed)

let from_float_1 =
  test_case "from float 1" `Quick (fun () ->
      let expected = "33.33%" in
      let computed = Percent.(33.3344556 |> from_float |> to_string) in
      check string "should be equal" expected computed)

let apply_1 =
  test_case "apply 1" `Quick (fun () ->
      let expected = "125.00" in
      let computed =
        Percent.(apply @@ from_int 50) (Num.from_int 250) |> Num.to_string
      in
      check string "should be equal" expected computed)

let apply_2 =
  test_case "apply 2" `Quick (fun () ->
      let expected = "333.30" in
      let computed =
        Percent.(apply @@ ~%:33.33) (Num.from_int 1000) |> Num.to_string
      in
      check string "should be equal" expected computed)

let cases = ("Percent", [ from_int_1; from_float_1; apply_1; apply_2 ])
