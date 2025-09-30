open A2.Voting

(**[print_lst tally_lst num] prints all elements of [tally_lst] in order,
   starting at number [num] *)
let rec print_lst tally_lst num =
  match tally_lst with
  | [] -> ()
  | (c, p) :: t ->
      Printf.printf "%d. %s - %d votes\n%!" num c p;
      print_lst t (num + 1)

(**[print_tally tally_lst] prints the [tally_lst] by first sorting the elements
   by greatest points, and then envoking [print_lst] *)
let print_tally tally_lst =
  let sorted =
    List.sort
      (fun (c1, p1) (c2, p2) ->
        match compare (-1 * p1) (-1 * p2) with
        | 0 -> String.compare c1 c2
        | p -> p)
      tally_lst
  in
  print_lst sorted 1

(**[print_winners winner_lst] prints all elements of [winner_lst] on a new line*)
let rec print_winners winner_lst =
  match winner_lst with
  | [] -> print_endline "No Winners"
  | [ h ] -> print_endline h
  | h :: t ->
      print_endline h;
      print_winners t

(**Function that runs the Borda Voting System. Must be 3 arguments provided in
   the terminal, otherwise error *)
let () =
  if Array.length Sys.argv = 3 then
    let candidate_file = Sys.argv.(1) in
    let ballot_path = Sys.argv.(2) in

    try
      let candidates = load_candidates candidate_file in
      let ballots = load_ballots ballot_path in
      let tally_results = tally candidates ballots in
      let winner_results = winners candidates ballots in

      Printf.printf "Borda Tally:\n";
      print_tally tally_results;
      Printf.printf "Winner(s): \n";
      print_winners winner_results
    with
    | Csv.Failure (row, column, msg) ->
        Printf.printf "CSV error at row %d, columm %d : %s \n" row column msg;
        prerr_endline "Ensure that each candidate row has exactly one name";
        prerr_endline "Ensure that each ballot row is a full ranking"
    | Sys_error msg -> prerr_endline ("File error: " ^ msg)
    | Failure msg -> prerr_endline ("Error: " ^ msg)
    | e -> prerr_endline "Unexpected error"
  else print_endline "Error. Did not properly enter 2 file paths"
