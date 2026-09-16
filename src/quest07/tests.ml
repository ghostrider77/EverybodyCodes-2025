open OUnit2


let tests =
  "quest07" >::: [
    "should find the only name that complies with the given rules" >:: (fun _ ->
      let names = ["Oronris"; "Urakris"; "Oroneth"; "Uraketh"] in
      let rules = [
        ('r', ['a'; 'i'; 'o']);
        ('i', ['p'; 'w']);
        ('n', ['e'; 'r']);
        ('o', ['n'; 'm']);
        ('k', ['f'; 'r']);
        ('a', ['k']);
        ('U', ['r']);
        ('e', ['t']);
        ('O', ['r']);
        ('t', ['h'])
        ] in
      let result = Quest.find_name_that_complies_to_the_rules names (Quest.CharMap.of_list rules) in
      let expected = "Oroneth" in
      assert_equal expected result ~printer:(Printf.sprintf "%S")
      );
  ]


let () =
  run_test_tt_main tests
