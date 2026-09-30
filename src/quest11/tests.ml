open OUnit2


let tests =
  "quest10" >::: [
    "should perform a given amount of rounds in flock rearrangement" >:: (fun _ ->
      let flock = [9; 1; 1; 4; 9; 6] in
      let nr_rounds = 10 in
      let result = Quest.flock_rearrangement flock nr_rounds in
      let expected = 109 in
      assert_equal expected result ~printer:(Printf.sprintf "%d")
      );

    "should calculate number of rounds that balance the flock" >:: (fun _ ->
      let flock = [9; 1; 1; 4; 9; 6] in
      let result = Quest.get_nr_rounds_to_balance_the_flock flock in
      let expected = 11 in
      assert_equal expected result ~printer:(Printf.sprintf "%d")
      );

    "should calculate number of rounds that balance the flock using a larger example" >:: (fun _ ->
      let flock = [805; 706; 179; 48; 158; 150; 232; 885; 598; 524; 423] in
      let result = Quest.get_nr_rounds_to_balance_the_flock flock in
      let expected = 1579 in
      assert_equal expected result ~printer:(Printf.sprintf "%d")
      );
  ]


let () =
  run_test_tt_main tests
