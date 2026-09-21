open OUnit2


let tests =
  "quest09" >::: [
    "should calculate the product of child-mother and child-father DNA similarity" >:: (fun _ ->
      let input_lines = [
        "1:CAAGCGCTAAGTTCGCTGGATGTGTGCCCGCG";
        "2:CTTGAATTGGGCCGTTTACCTGGTTTAACCAT";
        "3:CTAGCGCTGAGCTGGCTGCCTGGTTGACCGCG";
      ] in
      let dnas = Quest.parse_input input_lines in
      let result = Quest.calc_degree_of_similarity_sum dnas in
      let expected = 414 in
      assert_equal expected result ~printer:(Printf.sprintf "%d")
      );

    "should calculate the sum of the product of child-mother and child-father DNA similarities" >:: (fun _ ->
      let input_lines = [
        "1:GCAGGCGAGTATGATACCCGGCTAGCCACCCC";
        "2:TCTCGCGAGGATATTACTGGGCCAGACCCCCC";
        "3:GGTGGAACATTCGAAAGTTGCATAGGGTGGTG";
        "4:GCTCGCGAGTATATTACCGAACCAGCCCCTCA";
        "5:GCAGCTTAGTATGACCGCCAAATCGCGACTCA";
        "6:AGTGGAACCTTGGATAGTCTCATATAGCGGCA";
        "7:GGCGTAATAATCGGATGCTGCAGAGGCTGCTG";
      ] in
      let dnas = Quest.parse_input input_lines in
      let result = Quest.calc_degree_of_similarity_sum dnas in
      let expected = 1245 in
      assert_equal expected result ~printer:(Printf.sprintf "%d")
      );
  ]


let () =
  run_test_tt_main tests
