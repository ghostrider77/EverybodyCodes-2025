open OUnit2


let tests =
  "quest17" >::: [
    "should calculate the sum of values within a given radius" >:: (fun _ ->
      let input = [
        "189482189843433862719";
        "279415473483436249988";
        "432746714658787816631";
        "428219317375373724944";
        "938163982835287292238";
        "627369424372196193484";
        "539825864246487765271";
        "517475755641128575965";
        "685934212385479112825";
        "815992793826881115341";
        "1737798467@7983146242";
        "867597735651751839244";
        "868364647534879928345";
        "519348954366296559425";
        "134425275832833829382";
        "764324337429656245499";
        "654662236199275446914";
        "317179356373398118618";
        "542673939694417586329";
        "987342622289291613318";
        "971977649141188759131";
      ] in
      let grid = Quest.parse_input input in
      let radius = 10.0 in
      let result = Quest.calc_sum_of_cells_within_radius grid radius in
      let expected = 1573 in
      assert_equal expected result ~printer:(Printf.sprintf "%d")
      );
  ]


let () =
  run_test_tt_main tests
