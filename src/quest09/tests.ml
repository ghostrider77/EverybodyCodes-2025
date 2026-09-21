open OUnit2


let tests =
  "quest09" >::: [
    "should calculate the product of child-mother and child-father DNA similarity" >:: (fun _ ->
      let input_lines = [
        "1:CAAGCGCTAAGTTCGCTGGATGTGTGCCCGCG";
        "2:CTTGAATTGGGCCGTTTACCTGGTTTAACCAT";
        "3:CTAGCGCTGAGCTGGCTGCCTGGTTGACCGCG";
      ] in
      let a, b, c = Quest.parse_input input_lines in
      let result = Quest.calc_degree_of_similarity a b c in
      let expected = 414 in
      assert_equal expected result ~printer:(Printf.sprintf "%d")
      );
  ]


let () =
  run_test_tt_main tests
