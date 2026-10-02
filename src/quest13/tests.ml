open OUnit2


let tests =
  "quest13" >::: [
    "should return the number after the dial is turned around" >:: (fun _ ->
      let ns = [72; 58; 47; 61; 67] in
      let nr_turns = 2025 in
      let result = Quest.turn_dial ns nr_turns in
      let expected = 67 in
      assert_equal expected result ~printer:(Printf.sprintf "%d")
      );
  ]


let () =
  run_test_tt_main tests
