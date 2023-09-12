# no of messages
m <- 72 
# total number of pairs
n_total_pairs <- choose(m, 2)
# no of participants (in each group)
n <- 500
# no of pairs each participants will see (type: effect and share separately)
n_pair <- 15


# frequency (balanced per type)
share_pairs <- matrix(0, m, m)
repl <- ceiling(n_pair * n / n_total_pairs)
share_pairs[upper.tri(share_pairs)] <- sample(rep(repl, n_total_pairs), 
                                              n_total_pairs)
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

# initial assignment

for(p in 1:n_pair){
  print(p)
  for(i in 1:n){
    # effect block
    assgd_pairs <- c(msg_asgnd_effect[[i]], msg_asgnd_share[[i]])
    if(length(assgd_pairs) == 0){
      avl_pairs_ <- 1:n_total_pairs
    }
    else{
      avl_pairs_ <- (1:n_total_pairs)[-assgd_pairs]
    }
    pair_ <- sample(avl_pairs_, 1)
    ind_ <- pair_to_array(pair_)
    n_filled_ <- length(msg_asgnd_effect[[i]])
    effect_count_ <- effect_pairs[ind_[1], ind_[2]]
    is_ok_ <- FALSE
    
    if(effect_count_ > 0){
      if(n_filled_ == 0){
        is_ok_ <- TRUE
        output[[i]][1, ] <- ind_
      }
      else{
        is_ok_ <- check_unique(output[[i]], ind_)
        if(is_ok_){
          filled_ <- length(msg_asgnd_effect[[i]])
          output[[i]][n_filled_/2+1, ] <- ind_
        }
      }
      if(is_ok_){
        effect_pairs[ind_[1], ind_[2]] <- effect_count_ - 1
        msg_asgnd_effect[[i]] <- c(msg_asgnd_effect[[i]], ind_)
      }
    }
    # else continue
    
    # share_block
    assgd_pairs <- c(msg_asgnd_effect[[i]], msg_asgnd_share[[i]])
    if(length(assgd_pairs) == 0){
      avl_pairs_ <- 1:n_total_pairs
    }
    else{
      avl_pairs_ <- (1:n_total_pairs)[-assgd_pairs]
    }
    pair_ <- sample(avl_pairs_, 1)
    ind_ <- pair_to_array(pair_)
    n_filled_ <- length(msg_asgnd_share[[i]])
    share_count_ <- share_pairs[ind_[1], ind_[2]]
    is_ok_ <- FALSE
    
    if(share_count_ > 0){
      if(n_filled_ == 0){
        is_ok_ <- TRUE
        output[[i]][n_pair+1, ] <- ind_
      }
      else{
        is_ok_ <- check_unique(output[[i]], ind_)
        if(is_ok_){
          filled_ <- length(msg_asgnd_share[[i]])
          output[[i]][n_pair+n_filled_/2+1, ] <- ind_
        }
      }
      if(is_ok_){
        share_pairs[ind_[1], ind_[2]] <- share_count_ - 1
        msg_asgnd_share[[i]] <- c(msg_asgnd_share[[i]], ind_)
      }
      
    }
    # else continue
    
  }
}