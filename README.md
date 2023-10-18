# colon-cancer
# For near optimum balance, run the following scripts in order (change n = 500 in initial_pairing.R first)
random_pairing.R -> exchange.R -> final_check.R
# exchange.R needs to be run multiple time unless 'integer(0)' appears for both effect and share pairs. 


# For generating big files with 7000 rows run the following script manually and follow the instructions within.
generate_manual_big_file.R

# At the end, run the following file for desired formatting in .csv
formatting.R
