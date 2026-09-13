open OUnit2


let tests =
  "quest01" >::: [
    "should find the name following the instructions without wrapping around the names" >:: (fun _ ->
      let lines = [
        "Vyrdax,Drakzyph,Fyrryn,Elarzris";
        "";
        "R3,L2,R3,L1";
        ] in
      let names, instructions = Quest.parse_input lines in
      let expected = "Fyrryn" in
      let result = Quest.follow_instructions names instructions in
      assert_equal expected result ~printer:(Printf.sprintf "%S")
      );
    "should find the name following the circular instructions" >:: (fun _ ->
      let lines = [
        "Vyrdax,Drakzyph,Fyrryn,Elarzris";
        "";
        "R3,L2,R3,L1";
        ] in
      let names, instructions = Quest.parse_input lines in
      let expected = "Elarzris" in
      let result = Quest.follow_circular_instructions names instructions in
      assert_equal expected result ~printer:(Printf.sprintf "%S")
      );
    "should return the first name following the circular swap instructions" >:: (fun _ ->
      let lines = [
        "Vyrdax,Drakzyph,Fyrryn,Elarzris";
        "";
        "R3,L2,R3,L3";
        ] in
      let names, instructions = Quest.parse_input lines in
      let expected = "Drakzyph" in
      let result = Quest.follow_circular_instructions_with_swaps names instructions in
      assert_equal expected result ~printer:(Printf.sprintf "%S")
      );
  ]


let () =
  run_test_tt_main tests
