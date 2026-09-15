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
  ]


let () =
  run_test_tt_main tests
