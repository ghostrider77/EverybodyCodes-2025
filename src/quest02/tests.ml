open OUnit2


let tests =
  "quest02" >::: [
    "should calculate the result by repeating the steps 3 times" >:: (fun _ ->
      let x = Quest.ComplexNumber.init 25 9 in
      let result = Quest.ComplexNumber.to_string @@ Quest.perform_cycle_k_times x 3 in
      let expected = "[357,862]" in
      assert_equal expected result ~printer:(Printf.sprintf "%S")
      );
  ]


let () =
  run_test_tt_main tests
