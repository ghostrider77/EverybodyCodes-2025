open OUnit2


let tests =
  "quest02" >::: [
    "should calculate the result by repeating the steps 3 times" >:: (fun _ ->
      let x = Quest.ComplexNumber.init 25 9 in
      let result = Quest.ComplexNumber.to_string @@ Quest.perform_cycle_k_times x 3 in
      let expected = "[357,862]" in
      assert_equal expected result ~printer:(Printf.sprintf "%S")
      );
    "should calculate the result by repeating the steps 100 times on a reduced grid" >:: (fun _ ->
      let top_left = Quest.ComplexNumber.init 35300 (-64910) in
      let coord_grid = Quest.create_grid top_left in
      let grid_numbers = Seq.map (fun (x, y) -> Quest.ComplexNumber.init x y) coord_grid in
      let result =
        Seq.fold_left
          (fun acc n -> if Option.is_some (Quest.perform_conditional_cycles n 100) then acc + 1 else acc)
          0
          grid_numbers in
      let expected = 4076 in
      assert_equal expected result ~printer:(Printf.sprintf "%d")
      );
  ]


let () =
  run_test_tt_main tests
