open OUnit2


let tests =
  "quest14" >::: [
    "should calculate the number of active tiles in each round" >:: (fun _ ->
      let input = [
        ".#.##.";
        "##..#.";
        "..##.#";
        ".#.##.";
        ".###..";
        "###.##";
      ] in
      let grid = Quest.parse_input input in
      let nr_rounds = 10 in
      let result = Quest.play_game grid nr_rounds in
      let expected = 200 in
      assert_equal expected result ~printer:(Printf.sprintf "%d")
      );
  ]


let () =
  run_test_tt_main tests
