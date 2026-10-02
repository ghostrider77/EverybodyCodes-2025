open OUnit2


let tests =
  "quest13" >::: [
    "should return the number after the dial is turned around" >:: (fun _ ->
      let ns = [72; 58; 47; 61; 67] in
      let dial = Quest.create_dial_from_numbers ns in
      let nr_turns = 2025 in
      let result = Quest.turn_dial dial nr_turns in
      let expected = 67 in
      assert_equal expected result ~printer:(Printf.sprintf "%d")
      );

    "should return the number after the dial is turned around when ranges were provided" >:: (fun _ ->
      let input = [
        "10-15";
        "12-13";
        "20-21";
        "19-23";
        "30-37";
        ] in
      let ranges = Quest.parse_ranges input in
      let dial = Quest.create_dial_from_ranges ranges in
      let nr_turns = 20252025 in
      let result = Quest.turn_dial dial nr_turns in
      let expected = 30 in
      assert_equal expected result ~printer:(Printf.sprintf "%d")
      );
  ]


let () =
  run_test_tt_main tests
