open OUnit2


let tests =
  "quest03" >::: [
    "should calculate the sum of unique crates" >:: (fun _ ->
      let crates = [10; 5; 1; 10; 3; 8; 5; 2; 2] in
      let result = Quest.find_sum_of_unique_elements crates in
      let expected = 29 in
      assert_equal expected result ~printer:(Printf.sprintf "%d")
      );
  ]


let () =
  run_test_tt_main tests
