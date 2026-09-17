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
        ('t', ['h']);
        ] in
      let result = Quest.find_name_that_complies_to_the_rules names (Quest.CharMap.of_list rules) in
      let expected = "Oroneth" in
      assert_equal expected result ~printer:(Printf.sprintf "%S")
      );

    "should calculate the index of sum of all names that complies with the given rules" >:: (fun _ ->
      let names = ["Xanverax"; "Khargyth"; "Nexzeth"; "Helther"; "Braerex"; "Tirgryph"; "Kharverax"] in
      let rules = [
        ('r', ['v'; 'e'; 'a'; 'g'; 'y']);
        ('a', ['e'; 'v'; 'x'; 'r']);
        ('e', ['r'; 'x'; 'v'; 't']);
        ('h', ['a'; 'e'; 'v']);
        ('g', ['r'; 'y']);
        ('y', ['p'; 't']);
        ('i', ['v'; 'r']);
        ('K', ['h']);
        ('v', ['e']);
        ('B', ['r']);
        ('t', ['h']);
        ('N', ['e']);
        ('p', ['h']);
        ('H', ['e']);
        ('l', ['t']);
        ('z', ['e']);
        ('X', ['a']);
        ('n', ['v']);
        ('x', ['z']);
        ('T', ['i']);
        ] in
      let result = Quest.get_index_sum_of_all_allowed_names names (Quest.CharMap.of_list rules) in
      let expected = 23 in
      assert_equal expected result ~printer:(Printf.sprintf "%d")
      );

    "should calculate the number of possible names of length between 7 and 11 with a give prefix" >:: (fun _ ->
      let names = ["Xaryt"] in
      let rules = [
        ('X', ['a'; 'o']);
        ('a', ['r'; 't']);
        ('r', ['y'; 'e'; 'a']);
        ('h', ['a'; 'e'; 'v']);
        ('t', ['h']);
        ('v', ['e']);
        ('y', ['p'; 't']);
        ] in
      let result = Quest.calc_nr_of_unique_prefixes names (Quest.CharMap.of_list rules) 7 11 in
      let expected = 25 in
      assert_equal expected result ~printer:(Printf.sprintf "%d")
      );

    "should calculate the number of possible names of length between 7 and 11 with guve prefixes" >:: (fun _ ->
      let names = ["Khara"; "Xaryt"; "Noxer"; "Kharax"] in
      let rules = [
        ('r', ['v'; 'e'; 'a'; 'g'; 'y']);
        ('a', ['e'; 'v'; 'x'; 'r'; 'g']);
        ('e', ['r'; 'x'; 'v'; 't']);
        ('h', ['a'; 'e'; 'v']);
        ('g', ['r'; 'y']);
        ('y', ['p'; 't']);
        ('i', ['v'; 'r']);
        ('K', ['h']);
        ('v', ['e']);
        ('B', ['r']);
        ('t', ['h']);
        ('N', ['e']);
        ('p', ['h']);
        ('H', ['e']);
        ('l', ['t']);
        ('z', ['e']);
        ('X', ['a']);
        ('n', ['v']);
        ('x', ['z']);
        ('T', ['i']);
        ] in
      let result = Quest.calc_nr_of_unique_prefixes names (Quest.CharMap.of_list rules) 7 11 in
      let expected = 1154 in
      assert_equal expected result ~printer:(Printf.sprintf "%d")
      );
  ]


let () =
  run_test_tt_main tests
