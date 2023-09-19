source("Research/RA/colon-cancer/func.R")
final_pairs <-readRDS("Research/RA/colon-cancer/pair_assignment_68_balanced_after_check_second.rds")
n <- length(final_pairs)
n_pair <- nrow(final_pairs[[1]]) / 2
main_output <- matrix(0, n, 4*n_pair)
col_name <- character(0)

for(i in 1:(2*n_pair)){
  col_name <- c(col_name, paste("P_PAIR", i, "A", sep = ""),
                paste("P_PAIR", i, "B", sep = ""))
}

for(i in 1:n){
  vec_ <- mat_to_vec(final_pairs[[i]])
  main_output[i, ] <- vec_
}

colnames(main_output) <- col_name
write.csv(main_output, "Research/RA/colon-cancer/pair_assignments_second_68_balanced.csv")