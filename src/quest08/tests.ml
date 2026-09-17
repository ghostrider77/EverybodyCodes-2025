open OUnit2


let tests =
  "quest08" >::: [
    "should calculate the number of times a thread goes through the center of the circle" >:: (fun _ ->
      let nr_nails = 8 in
      let nails = [1; 5; 2; 6; 8; 4; 1; 7; 3] in
      let expected = 4 in
      let result = Quest.calc_nr_times_thread_passes_through_center nr_nails nails in
      assert_equal expected result ~printer:(Printf.sprintf "%d")
      );
  ]


let () =
  run_test_tt_main tests
