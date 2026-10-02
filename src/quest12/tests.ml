open OUnit2


let tests =
  "quest12" >::: [
    "should return the number of ignited barrels if we start it from the top left" >:: (fun _ ->
      let input = [
        "989601";
        "857782";
        "746543";
        "766789";
      ] in
      let grid = Quest.parse_input input in
      let start_barrels : Quest.coord list = [{x = 0; y = 0}] in
      let result = Quest.find_ignited_barrels grid start_barrels in
      let expected = 16 in
      assert_equal expected result ~printer:(Printf.sprintf "%d")
      );

    "should return the number of ignited barrels if we start it from the opposite corners" >:: (fun _ ->
      let input = [
        "9589233445";
        "9679121695";
        "8469121876";
        "8352919876";
        "7342914327";
        "7234193437";
        "6789193538";
        "6781219648";
        "5691219769";
        "5443329859";
      ] in
      let ({nr_rows; nr_cols; _} as grid) : Quest.grid = Quest.parse_input input in
      let start_barrels : Quest.coord list = [{x = 0; y = 0}; {x = nr_rows - 1; y = nr_cols - 1}] in
      let result = Quest.find_ignited_barrels grid start_barrels in
      let expected = 58 in
      assert_equal expected result ~printer:(Printf.sprintf "%d")
      );

    "should return the number of ignited barrels if we ignite the one that causes the most damage" >:: (fun _ ->
      let input = [
        "5411";
        "3362";
        "5235";
        "3112";
      ] in
      let grid = Quest.parse_input input in
      let result = Quest.find_greedy_largest_components grid 3 in
      let expected = 14 in
      assert_equal expected result ~printer:(Printf.sprintf "%d")
      );

    "should return the number of ignited barrels if we ignite the one that causes the most damage pt. 2" >:: (fun _ ->
      let input = [
        "41951111131882511179";
        "32112222211518122215";
        "31223333322115122219";
        "31234444432147511128";
        "91223333322176121892";
        "61112222211166431583";
        "14661111166111111746";
        "11111119142122222177";
        "41222118881233333219";
        "71222127839122222196";
        "56111126279711111517";
      ] in
      let grid = Quest.parse_input input in
      let result = Quest.find_greedy_largest_components grid 3 in
      let expected = 136 in
      assert_equal expected result ~printer:(Printf.sprintf "%d")
      );
  ]


let () =
  run_test_tt_main tests
