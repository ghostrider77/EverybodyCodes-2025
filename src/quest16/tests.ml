open OUnit2


let tests =
  "quest16" >::: [
    "should calculate the number of blocks in the wall" >:: (fun _ ->
      let ns = [1; 2; 3; 5; 9] in
      let limit = 90 in
      let result = Quest.calc_nr_of_blocks ns limit in
      let expected = 193 in
      assert_equal expected result ~printer:(Printf.sprintf "%d")
      );

    "should calculate the product of numbers that produces a given block structure" >:: (fun _ ->
      let blocks = [1; 2; 2; 2; 2; 3; 1; 2; 3; 3; 1; 3; 1; 2; 3; 2; 1; 4; 1; 3; 2; 2; 1; 3; 2; 2] in
      let result = Quest.calc_product_of_recreated_numbers blocks in
      let expected = 270 in
      assert_equal expected result ~printer:(Printf.sprintf "%d")
      );

    "should calculate maximal length of the wall when the number of available bricks are given" >:: (fun _ ->
      let blocks = [1; 2; 2; 2; 2; 3; 1; 2; 3; 3; 1; 3; 1; 2; 3; 2; 1; 4; 1; 3; 2; 2; 1; 3; 2; 2] in
      let nr_available_blocks = 202520252025000 in
      let result = Quest.get_wall_length blocks nr_available_blocks in
      let expected = 94439495762954 in
      assert_equal expected result ~printer:(Printf.sprintf "%d")
      );
  ]


let () =
  run_test_tt_main tests
