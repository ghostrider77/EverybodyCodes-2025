open OUnit2


let tests =
  "quest03" >::: [
    "should calculate the number of full turns of the last wheel" >:: (fun _ ->
      let cogwheels = [102; 75; 50; 35; 13] in
      let result = Quest.calc_number_of_full_turns_of_last_gear cogwheels 2025 in
      let expected = 15888 in
      assert_equal expected result ~printer:(Printf.sprintf "%d")
      );

    "should calculate the number of full turns of the first wheels given the turns of the last one" >:: (fun _ ->
      let cogwheels = [102; 75; 50; 35; 13] in
      let result = Quest.calc_number_of_full_turns_of_first_gear cogwheels 10_000_000_000_000 in
      let expected = 1274509803922 in
      assert_equal expected result ~printer:(Printf.sprintf "%d")
      );

    "should calculate the number of full turns of the last wheel when wheels are in pair" >:: (fun _ ->
      let cogwheels = [(5, 5); (7, 21); (18, 36); (27, 27); (10, 50); (10, 50); (11, 11)] in
      let result = Quest.calc_number_of_full_turns_of_last_gear_for_paired_wheels cogwheels 100 in
      let expected = 6818 in
      assert_equal expected result ~printer:(Printf.sprintf "%d")
      );
  ]


let () =
  run_test_tt_main tests
