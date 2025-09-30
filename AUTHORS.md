Authors: Gregory Parent, gmp89

Collaborators: None

GenAI:
- I used ChatGPT to randomly generate 40 rows for my "/data/tv_show_ballots.csv" file. This just simplified the process of creating 40 random rows of all 5 candidates
- When making my [cmp_tally] method in test_a2, (and later when making [print_tally] in bin/main I resused the same logic) I had the idea to sort the lists to compare the equality of two lists with the same values but in different order. ChatGPT introduced to me the String.compare and Int.compare methods which helped me sort the lists (for either comparison or printing)
- For [combine_tallies] in lib/voting.ml, I consulted ChatGPT for this line of code [(c, prev + p) :: List.remove_assoc c current]. Originally, I jsut had the new tuple append to current like follows [(c,prev+p) :: current]. This worked for computing the total tallies, as when looking for the points of a candidate, the earliest tuple would come up first, so it wouldn't matter if there were old, redudant, tuples still in the association list. However, this created issues in my test case, as I couldn't just directly compare the lists using my [cmp_tally] helper method. Thus, in looking for solutions to remove old elements, Gen AI suggested using the [List.remove_assoc] method