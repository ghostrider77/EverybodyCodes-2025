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
  ]


let () =
  run_test_tt_main tests
