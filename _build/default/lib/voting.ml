type candidate = string
(**The [candidate] type synonym is the candidate's name of type string *)

type ballot = candidate list
(**The [ballot] type synonym represents the full ranking of the candidates. They
   are ranked starting at index 0 being the highest ranked item*)

type tally = (candidate * int) list
(**[tally] is a type synonym of an [(candidate * int)] association list,
   connecting a candidate key with a value of their total points *)

(** [load_candidates file] is the list of candidate in [file]. Will fail if the
    file contains multiple entries per row *)
let load_candidates file =
  let rows = Csv.load file in
  List.map
    (function
      | [ name ] -> name
      | _ -> failwith "Invalid candidate file. More than one entry per row")
    rows

(**[load_ballots file] is the String String List loaded from [file] *)
let load_ballots file = Csv.load file

(**[compute_points candidates ballot] is an association list of each candidate’s Borda points
   from one singular voter's ballot. *)
let compute_points candidates ballot : (candidate * int) list=
  (* let n = List.length candidates in *)
  (* let points = [] in  *)
  failwith "incomplete"

(**[combine_tallies] fully computes the final tallies by computing the sum of
   all values in [ballots] for each candidate in [candidates] *)
let combine_tallies candidates ballots = failwith "Incomplete"

(** [tally candidates ballots] is the full Borda tally from [candidates] and
    [ballots]. *)
let tally candidiates ballots = failwith "Not implemented"

(** [winners tallies] is all candidates tied for max points. *)
let winners tallies = failwith "Not implemented"
