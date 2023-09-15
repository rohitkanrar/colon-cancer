final_pairs <-readRDS("Research/RA/colon-cancer/pair_assignment_final_after_check.rds")
# final_pairs <- output
effect_final_freq <- numeric(n_total_pairs)
share_final_freq <- numeric(n_total_pairs)

for(i in 1:n){
  pairs_ <- final_pairs[[i]]
  if(sum(duplicated(as.numeric(pairs_))) > 0)
    print(paste("pair", i, "has duplicated messages", sep = " "))
  for(j in 1:n_pair){
    pair_ <- pairs_[j, ]
    ind_ <- array_to_pair(pair_[1], pair_[2])
    effect_final_freq[ind_] <- effect_final_freq[ind_] + 1
  }
  for(j in (n_pair+1):(2*n_pair)){
    pair_ <- pairs_[j, ]
    ind_ <- array_to_pair(pair_[1], pair_[2])
    share_final_freq[ind_] <- share_final_freq[ind_] + 1
  }
}

count(effect_final_freq)
count(share_final_freq)
# saveRDS(final_pairs, 
#         "Research/RA/colon-cancer/pair_assignment_final_after_check.rds")