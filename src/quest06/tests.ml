open OUnit2


let tests =
  "quest06" >::: [
    "should calculate the mentor-novice pairs for swordfighters" >:: (fun _ ->
      let str = "ABabACacBCbca" in
      let heroes = Quest.parse_input str in
      let result = Quest.count_mentor_novice_pairs heroes Quest.SwordFighter in
      let expected = 5 in
      assert_equal expected result ~printer:(Printf.sprintf "%d")
      );

    "should calculate the mentor-novice pairs for a short input within a given radius without repeat" >:: (fun _ ->
      let str = "AABCBABCABCabcabcABCCBAACBCa" in
      let heroes = Quest.parse_input str in
      let repeat = 1 in
      let radius = 10 in
      let result = Quest.count_nearby_mentor_novice_pairs heroes repeat radius in
      let expected = 34 in
      assert_equal expected result ~printer:(Printf.sprintf "%d")
      );

    "should calculate the mentor-novice pairs for a short input within a given radius with repeat" >:: (fun _ ->
      let str = "AABCBABCABCabcabcABCCBAACBCa" in
      let heroes = Quest.parse_input str in
      let repeat = 2 in
      let radius = 10 in
      let result = Quest.count_nearby_mentor_novice_pairs heroes repeat radius in
      let expected = 72 in
      assert_equal expected result ~printer:(Printf.sprintf "%d")
      );
    "should calculate the mentor-novice pairs for a short input within a given radius with many repeats" >:: (fun _ ->
      let str = "AABCBABCABCabcabcABCCBAACBCa" in
      let heroes = Quest.parse_input str in
      let repeat = 1000 in
      let radius = 1000 in
      let result = Quest.count_nearby_mentor_novice_pairs heroes repeat radius in
      let expected = 3442321 in
      assert_equal expected result ~printer:(Printf.sprintf "%d")
      );
  ]


let () =
  run_test_tt_main tests
