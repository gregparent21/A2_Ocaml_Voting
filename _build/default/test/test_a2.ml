open OUnit2
open A2.Voting

(**[print_list] is a helper method I made for debugging *)
let rec print_list = function
  | [] -> ()
  | (k, v) :: t ->
      Printf.printf "Key: %s, Value: %d\n" k v;
      print_list t

(**[sort_tally tally_lst] sorts [tally_lst] first by the candidate name, then by
   points in the case of the same candidate name *)
let sort_tally tally_lst =
  List.sort
    (fun (c1, p1) (c2, p2) ->
      match String.compare c1 c2 with
      | 0 -> Int.compare p1 p2
      | k -> k)
    tally_lst

(**[cmp_tally a b] is true if [a] and [b] are equal once sorted *)
let cmp_tally a b = sort_tally a = sort_tally b

(**[cmp_winners a b] is true if a and b have the same elements, ignoring order
*)
let cmp_winners a b = List.sort String.compare a = List.sort String.compare b

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
             (cmp_tally tallies expected) );
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
             (cmp_tally final_points expected) );
         ( "Testing [winners] works as intended when there is only 1 winner"
         >:: fun _ ->
           let expected_winner = [ "Vanilla" ] in
           let candidates =
             load_candidates "../data/ice_cream_candidates.csv"
           in
           let ballots = load_ballots "../data/ice_cream_ballots.csv" in
           let winner_results = winners candidates ballots in
           assert_bool "Winner is determined as expected"
             (cmp_winners winner_results expected_winner) );
         ( "Testing [winners] works as intended when there is a tie winner"
         >:: fun _ ->
           let expected_winners = [ "Vanilla"; "Chocolate" ] in
           let winner_results =
             winners
               (load_candidates "../data/ice_cream_candidates.csv")
               (load_ballots "../data/tied_ballot.csv")
           in
           assert_bool "Winner is determined as expected"
             (cmp_winners winner_results expected_winners) );
         ( "Testing [winners] works as intended for a larger data set of 5 \
            candidates"
         >:: fun _ ->
           let expected_winners = [ "Breaking Bad" ] in
           let winner_results =
             winners
               (load_candidates "../data/tv_show_candidates.csv")
               (load_ballots "../data/tv_show_ballots.csv")
           in
           assert_bool "Winner is determined as expected for larger data set"
             (cmp_winners winner_results expected_winners) );
         ( "Testing [tally] works as intended for a larger data set of 5 \
            candidates"
         >:: fun _ ->
           let expected_tally =
             [
               ("The Wire", 61);
               ("Dexter", 66);
               ("The Sopranos", 67);
               ("Breaking Bad", 137);
               ("Game of Thrones", 69);
             ]
           in
           let final_points =
             tally
               (load_candidates "../data/tv_show_candidates.csv")
               (load_ballots "../data/tv_show_ballots.csv")
           in
           assert_bool
             "Final tally is determined as expected for a larger\n\
             \            dataset"
             (cmp_tally final_points expected_tally) );
       ]

let _ = run_test_tt_main tests
