m <- 72
n_total_pairs <- choose(m, 2)
n <- 500
n_pair <- 15

share_pairs <- matrix(0, m, m)

repl <- floor(n_pair * n / n_total_pairs)
n_bigs <- n * n_pair - repl * n_total_pairs
n_smalls <- n_total_pairs - n_bigs

# x+y=2556
# 3x+2y=7500

share_pairs[upper.tri(share_pairs)] <- sample(c(rep(repl, n_smalls),
                                 rep(repl+1, n_bigs)), n_total_pairs)


# repl <- ceiling(n_pair * n / n_total_pairs)
# share_pairs[upper.tri(share_pairs)] <- sample(c(rep(repl, 2000),
#                                                 rep(repl+1, 556)), 
#                                               n_total_pairs)
effect_pairs <- share_pairs 
# randomizing this will lead to unbalanced sample for each pair of messages
share <- share_pairs
effect <- effect_pairs


all_pairs <- matrix(0, m, m)
all_pairs[upper.tri(all_pairs)] <- 1:n_total_pairs



output <- vector(mode = "list", length = n)

for(i in 1:n){
  print(i)
  share_pairs_ <- share_pairs
  effect_pairs_ <- effect_pairs
  output[[i]] <- matrix(0, 2 * n_pair, 2)
  if(i == 500){
    break
  }
  else{
    for(j in 1:n_pair){
      count_ <- 0
      while(count_ == 0){
        prob_ <- share_pairs_[upper.tri(share_pairs_)]
        prob_ <- prob_ / sum(prob_)
        pair_ <- sample(1:n_total_pairs, 1, prob = prob_)
        msg_ <- which(all_pairs == pair_, arr.ind = TRUE)
        count_ <- share_pairs[msg_[1], msg_[2]]
      }
      output[[i]][j, ] <- c(msg_[1], msg_[2])
      count_ <- count_ - 1
      share_pairs[msg_[1], msg_[2]] <- count_
      
      share_pairs_[msg_[1], ] <- 0
      share_pairs_[, msg_[2]] <- 0
      
      effect_pairs_[msg_[1], ] <- 0
      effect_pairs_[, msg_[2]] <- 0
    }
    
    for(j in 1:n_pair){
      count_ <- 0
      while(count_ == 0){
        prob_ <- effect_pairs_[upper.tri(effect_pairs_)]
        prob_ <- prob_ / sum(prob_)
        pair_ <- sample(1:n_total_pairs, 1, prob = prob_)
        msg_ <- which(all_pairs == pair_, arr.ind = TRUE)
        count_ <- share_pairs[msg_[1], msg_[2]]
      }
      output[[i]][(j+n_pair), ] <- c(msg_[1], msg_[2])
      count_ <- count_ - 1
      effect_pairs[msg_[1], msg_[2]] <- count_
      
      effect_pairs_[msg_[1], ] <- 0
      effect_pairs_[, msg_[2]] <- 0
    }
  }
}