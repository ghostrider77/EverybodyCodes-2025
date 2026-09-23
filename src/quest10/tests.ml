open OUnit2


let tests =
  "quest10" >::: [
    "should calculate number of sheeps that a dragon can reach" >:: (fun _ ->
      let input_lines = [
        "...SSS.......";
        ".S......S.SS.";
        "..S....S...S.";
        "..........SS.";
        "..SSSS...S...";
        ".....SS..S..S";
        "SS....D.S....";
        "S.S..S..S....";
        "....S.......S";
        ".SSS..SS.....";
        ".........S...";
        ".......S....S";
        "SS.....S..S..";
      ] in
      let board, initial_position = Quest.parse_input input_lines in
      let nr_steps = 3 in
      let result = Quest.get_nr_sheeps_the_dragon_can_eat board initial_position nr_steps in
      let expected = 27 in
      assert_equal expected result ~printer:(Printf.sprintf "%d")
      );
  ]


let () =
  run_test_tt_main tests
