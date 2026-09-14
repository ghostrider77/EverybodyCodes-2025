open OUnit2


let tests =
  "quest03" >::: [
    "should calculate the sum of unique crates" >:: (fun _ ->
      let crates = [10; 5; 1; 10; 3; 8; 5; 2; 2] in
      let result = Quest.find_sum_of_unique_elements crates in
      let expected = 29 in
      assert_equal expected result ~printer:(Printf.sprintf "%d")
      );

    "should calculate the sum of smallest 20 unique crates" >:: (fun _ ->
      let crates = [
        4; 51; 13; 64; 57; 51; 82; 57; 16; 88; 89; 48; 32; 49; 49; 2; 84;
        65; 49; 43; 9; 13; 2; 3; 75; 72; 63; 48; 61; 14; 40; 77
        ] in
      let result = Quest.find_sum_of_smallest_set_of_given_size crates 20 in
      let expected = 781 in
      assert_equal expected result ~printer:(Printf.sprintf "%d")
      );
  ]


let () =
  run_test_tt_main tests
