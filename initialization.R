# frequency (balanced per type)
share_pairs <- matrix(0, m, m)
repl <- ceiling(n_pair * n / n_total_pairs)
share_pairs <- rep(repl, n_total_pairs)
effect_pairs <- share_pairs 


# placing all pairs in an upper triangular array
all_pairs <- matrix(0, m, m)
all_pairs[upper.tri(all_pairs)] <- 1:n_total_pairs

# initialization
output <- vector(mode = "list", length = n)
msg_asgnd_effect <- vector(mode = "list", length = n)
msg_asgnd_share <- vector(mode = "list", length = n)
for(i in 1:n){
  output[[i]] <- matrix(0, 2 * n_pair, 2)
  msg_asgnd_effect[[i]] <- numeric(0)
  msg_asgnd_share[[i]] <- numeric(0)
}