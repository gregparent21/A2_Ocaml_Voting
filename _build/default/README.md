Welcome to A2: Voting!

To use:
1. First run [dune build] in the terminal
2. To run, run [dune exec bin/main.exe <candidate CSV file> <ballots CSV file>] in the terminal, where <candidate CSV file> and <ballots CSV file> are the file paths for the candidates and ballots, respectively. Both paths should begin with "data/" and end with ".csv"
    2a. Each row in <candidate CSV file> must have one name
    2b. Each row in <ballots CSV file> must have the full ranking of all the candidates
3. To run the sample provided ice cream election, run 
   [dune exec bin/main.exe data/ice_cream_candidates.csv data/ice_cream_ballots.csv] in the terminal
4. To run my additional sample election, of my top 5 favorite TV shows, run
   [dune exec bin/main.exe data/tv_show_candidates.csv data/tv_show_ballots.csv] in the terminal
5. If there is anything wrong with the files, an appropritate error message should be displayed