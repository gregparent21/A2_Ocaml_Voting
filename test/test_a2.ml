open OUnit2
open A2.Voting

(**[print_list] is a helper method I made for debugging *)
let rec print_list = function
  | [] -> ()
  | (k, v) :: t ->
      Printf.printf "Key: %s, Value: %d\n" k v;
      print_list t

let big_ice_cream_expected =
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
           let actual = load_ballots "../data/ice_cream_ballots.csv" in
           assert_bool "Checking the candidates loaded correctly from the csv"
             (big_ice_cream_expected = actual) );
         ( "Testing [compute_points] works as intended" >:: fun _ ->
           let expected =
             [ ("Chocolate", 2); ("Strawberry", 0); ("Vanilla", 1) ]
           in
           let ballot = [ "Chocolate"; "Vanilla"; "Strawberry" ] in
           let tallies =
             compute_points
               (load_candidates "../data/ice_cream_candidates.csv")
               ballot
           in
           assert_bool "Borda points were awarded as expected"
             (tallies = expected) );
         ( "Testing [tally] works as intended when given the ice cream test case"
         >:: fun _ ->
           let expected =
             [ ("Vanilla", 23); ("Strawberry", 16); ("Chocolate", 21) ]
           in
           let final_points =
             tally
               (load_candidates "../data/ice_cream_candidates.csv")
               (load_ballots "../data/ice_cream_ballots.csv")
           in
           assert_bool "Borda points were awarded as expected"
             (final_points = expected) );
         ( "Testing [winners] works as intended when there is only 1 winner"
         >:: fun _ ->
           let expected_winner = [ "Vanilla" ] in
           let final_points =
             tally
               (load_candidates "../data/ice_cream_candidates.csv")
               (load_ballots "../data/ice_cream_ballots.csv")
           in
           let winners = winners final_points in
           assert_bool "Winner is determined as expected"
             (winners = expected_winner) );
         ( "Testing [winners] works as intended when there is a tie winner"
         >:: fun _ ->
           let expected_winners = [ "Vanilla";"Chocolate" ] in
           let final_points =
             tally
               (load_candidates "../data/ice_cream_candidates.csv")
               (load_ballots "../data/tied_ballot.csv")
           in
           let winners = winners final_points in
           assert_bool "Winner is determined as expected"
             (winners = expected_winners) );
       ]

(*TODO: make a method that compares 2 association lists without mattering about
  order *)
let _ = run_test_tt_main tests
