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

    "should calculate the number of times a thread intersects other threads" >:: (fun _ ->
      let nails = [1; 5; 2; 6; 8; 4; 1; 7; 3; 5; 7; 8; 2] in
      let expected = 21 in
      let result = Quest.calc_total_nr_intersections nails in
      assert_equal expected result ~printer:(Printf.sprintf "%d")
      );

    "should calculate the cut with the maximum number of crossings" >:: (fun _ ->
      let nails = [1; 5; 2; 6; 8; 4; 1; 7; 3; 6] in
      let nr_nails = 8 in
      let expected = 7 in
      let result = Quest.find_max_number_of_crossings nr_nails nails in
      assert_equal expected result ~printer:(Printf.sprintf "%d")
      );
  ]


let () =
  run_test_tt_main tests
