open OUnit2


let tests =
  "quest10" >::: [
    "should calculate number of sheep that a dragon can reach" >:: (fun _ ->
      let flock = [9; 1; 1; 4; 9; 6] in
      let nr_rounds = 10 in
      let result = Quest.flock_rearrangement flock nr_rounds in
      let expected = 109 in
      assert_equal expected result ~printer:(Printf.sprintf "%d")
      );
  ]


let () =
  run_test_tt_main tests
