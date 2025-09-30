type candidate = string
(**The [candidate] type synonym is the candidate's name of type string *)

type ballot = candidate list
(**The [ballot] type synonym represents the full ranking of the candidates. They
   are ranked starting at index 0 being the highest ranked item*)

type tally = (candidate * int) list
(**[tally] is a type synonym of an [(candidate * int)] association list,
   connecting a candidate key with a value of their total points *)

(* I used these type synonyms to define return types for the methods before
   fully developing the methods. This prevented compile time errors in my test
   cases *)

(**[lookup key] returns the value associated with [key] in an association list.
   if key is not in the list, then it returns None*)
let rec lookup key lst =
  match lst with
  | [] -> None
  | (k, v) :: t -> if key = k then Some v else lookup key t

(** [load_candidates file] is the list of candidate in [file]. Will fail if the
    file contains multiple entries per row *)
let load_candidates file =
  let rows = Csv.load file in
  List.map
    (function
      | [ name ] -> name
      | _ -> failwith "Invalid candidate file. More than one entry per row")
    rows

(**[check_rows row length] checks that all rows in [table] are of length
   [length] *)
let rec check_rows table length =
  match table with
  | [] -> ()
  | h :: t ->
      if List.length h <> length then
        failwith
          "Error: Not correct amount of candidates ranked in each CSV row"
      else check_rows t length

(**[load_ballots file] is the String String List loaded from [file]. Fails if
   the file is empty or if not every row has all the candidates ranked. Checks
   this by calling [check_rows row length] *)
let load_ballots file =
  let rows = Csv.load file in
  match rows with
  | [] -> failwith "Error: Ballot file is empty"
  | h :: t ->
      let length = List.length h in
      check_rows t length;
      rows

(**[compute_points candidates ballot] is an association list of each candidate’s
   Borda points assigned from one singular voter's ballot. *)
let compute_points candidates ballot =
  let n = List.length candidates in
  let indicies = List.mapi (fun i c -> (c, i)) ballot in
  List.map
    (fun c ->
      match lookup c indicies with
      | None ->
          failwith
            "Invalid ballot. Each candidate in candidates should be in the \
             ballot"
      | Some i -> (c, n - 1 - i))
    candidates

(**[combine_tallies combined tallies] computes the sum of the points from two
   separate tallies by adding all tallies from [tallies] into [combined]
   association list *)
let combine_tallies combined tallies =
  let add current (c, p) =
    let current_value = lookup c current in
    let prev =
      match current_value with
      | Some v -> v
      | None -> 0
    in
    (c, prev + p) :: List.remove_assoc c current
  in
  List.fold_left add combined tallies

(** [tally candidates ballots] is the full Borda tally from [candidates] and
    [ballots]. It starts by assigning every candidate in [candidates] as point
    of 0. It then calls [compute_points] for each ballot in [ballots] to compute
    them into [tally]s. [List.map computed ballots] forms a [tally list], one
    for each ballot, and uses [combine_tallies] to fold the points into a
    running total *)
let tally candidates ballots =
  let start_values = List.map (fun c -> (c, 0)) candidates in
  let computed = compute_points candidates in
  List.fold_left combine_tallies start_values (List.map computed ballots)

(** [compute_winners tallies] is all candidates tied for max points. First we
    find the max_points scored by using [List.fold_left] and then use
    [List.filter] to add all elements with the winning score to an accumulator.
    Then we use [List.map fst filter] to get the first element of each tuple,
    which is the name of the candidate *)
let compute_winners tallies =
  match tallies with
  | [] -> []
  | _ ->
      let max_points =
        List.fold_left (fun m (_, p) -> if p > m then p else m) (-1) tallies
      in
      let filter = List.filter (fun (_, p) -> p = max_points) tallies in
      List.map fst filter

(**[winners candidates ballots] is the list of final winners. It finds this by
   finding the final tally by calling [tally] and passes the values to
   [compute_winners] *)
let winners candidates ballots =
  let final_tally = tally candidates ballots in
  compute_winners final_tally
