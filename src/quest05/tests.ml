open OUnit2


let tests =
  "quest05" >::: [
    "should calculate the quality of the sword" >:: (fun _ ->
      let numbers = [5; 3; 7; 8; 9; 10; 4; 5; 7; 8; 8] in
      let result = Quest.get_sword_quality numbers in
      let expected = 581078 in
      assert_equal expected result ~printer:(Printf.sprintf "%d")
      );
  ]


let () =
  run_test_tt_main tests
