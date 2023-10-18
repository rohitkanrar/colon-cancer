big_output <- numeric(0)
set.seed(349431)

# RUN {random_pairing.R -> exhange.R (multiple times) -> final_check.R} 10 times atleast
big_output <- c(big_output, final_pairs) # after each successful run

# At the end
is_unique <- 0
for(i in 1:length(big_output)){
  a <- 0
  a <- sum(duplicated(as.numeric(big_output[[i]])))
  if(a > 0)
    print(i)
  is_unique <- is_unique + a
}
saveRDS(big_output,
        "Research/RA/colon-cancer/pair_assignment_big_68_balanced_after_check_first.rds")



big_output <- numeric(0)
set.seed(761357)

# RUN {random_pairing.R -> exhange.R (multiple times) -> final_check.R} 10 times atleast
big_output <- c(big_output, final_pairs) # after each successful run

# At the end
is_unique <- 0
for(i in 1:length(big_output)){
  a <- 0
  a <- sum(duplicated(as.numeric(big_output[[i]])))
  if(a > 0)
    print(i)
  is_unique <- is_unique + a
}
saveRDS(big_output,
        "Research/RA/colon-cancer/pair_assignment_big_68_balanced_after_check_second.rds")
