open OUnit2
open A2.Voting

let tests =
  "test suite"
  >::: [
         ("a trivial test" >:: fun _ -> assert_equal 0 0);
         ( "Testing [load_candidates] works when given the proper file"
         >:: fun _ ->
           let expected = [ "Chocolate"; "Strawberry"; "Vanilla" ] in
           let actual = load_candidates "../data/ice_cream_candidates.csv" in
           assert_bool "Checking the candidates loaded correctly from the csv"
             (expected = actual) );
         ( "Testing [load_ballots] works when given the proper file" >:: fun _ ->
           let expected =
             [
               [ "Vanilla"; "Chocolate"; "Strawberry" ];
               [ "Chocolate"; "Vanilla"; "Strawberry" ];
               [ "Vanilla"; "Chocolate"; "Strawberry" ];
               [ "Vanilla"; "Chocolate"; "Strawberry" ];
               [ "Strawberry"; "Chocolate"; "Vanilla" ];
               [ "Strawberry"; "Vanilla"; "Chocolate" ];
               [ "Strawberry"; "Chocolate"; "Vanilla" ];
               [ "Strawberry"; "Vanilla"; "Chocolate" ];
               [ "Vanilla"; "Chocolate"; "Strawberry" ];
               [ "Vanilla"; "Chocolate"; "Strawberry" ];
               [ "Strawberry"; "Chocolate"; "Vanilla" ];
               [ "Chocolate"; "Vanilla"; "Strawberry" ];
               [ "Vanilla"; "Chocolate"; "Strawberry" ];
               [ "Strawberry"; "Chocolate"; "Vanilla" ];
               [ "Chocolate"; "Vanilla"; "Strawberry" ];
               [ "Vanilla"; "Chocolate"; "Strawberry" ];
               [ "Strawberry"; "Vanilla"; "Chocolate" ];
               [ "Strawberry"; "Chocolate"; "Vanilla" ];
               [ "Chocolate"; "Vanilla"; "Strawberry" ];
               [ "Vanilla"; "Chocolate"; "Strawberry" ];
             ]
           in
           let actual = load_ballots "../data/ice_cream_ballots.csv" in
           assert_bool "Checking the candidates loaded correctly from the csv"
             (expected = actual) );
         ( "Testing [compute_points] works as intended"
         >:: fun _ ->
          let expected = [("Vanilla",1);("Chocolate",2);("Strawberry",0)] in
          let ballot = ["Choloate";"Vanilla";"Strawberry"] in
          let tallies = compute_points (load_candidates "../data/ice_cream_candidates.csv") ballot in
          assert_bool "Borda points were awarded as expected" (tallies = expected)
          );
       ]

let _ = run_test_tt_main tests
