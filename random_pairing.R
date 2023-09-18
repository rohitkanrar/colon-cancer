# setwd("Research/RA/colon-cancer/")
source("Research/RA/colon-cancer/func.R")
source("Research/RA/colon-cancer/initial_pairing.R")

# allowing +1 pairs after initial pairing
share_pairs <- share_pairs + 1
effect_pairs <- effect_pairs + 1
share_messages <- share_messages + (m-1)
effect_messages <- effect_messages + (m-1)

effect_pairs_dist <- get_dist_of_pairs(effect_messages)
share_pairs_dist <- get_dist_of_pairs(share_messages)

# expecting "output", "msg_asgnd_effect" and "msg_asgnd_share" objects
diff <- sum(rep(repl, n_total_pairs)) - n * n_pair
while((sum(share_pairs) > diff) || (sum(effect_pairs) > diff)){
  print(sum(share_pairs))
  print(sum(effect_pairs))
  
  if((sum(share_pairs) + sum(effect_pairs) - 2 * diff) %% 100 == 0){
    effect_pairs_dist <- get_dist_of_pairs(effect_messages)
    share_pairs_dist <- get_dist_of_pairs(share_messages)
  }
  
  if(((sum(share_pairs) - 100) < diff) && ((sum(effect_pairs)- 100) < diff)){
    pair_ <- sample(1:n_total_pairs, 1,
                    prob = (share_pairs + effect_pairs) /
                      sum((share_pairs + effect_pairs)))
  }
  else{
    pair_ <- sample(1:n_total_pairs, 1,
                    prob = (share_pairs_dist + effect_pairs_dist) /
                      sum((share_pairs_dist + effect_pairs_dist)))
  }
  ind_ <- pair_to_array(pair_)
  effect_count_ <- effect_pairs[pair_]
  share_count_ <- share_pairs[pair_]
  
  for(i in 1:n){
    if(effect_count_ > 0){
      n_filled_ <- length(msg_asgnd_effect[[i]])
      if(n_filled_/2 < n_pair){
        is_ok_ <- check_unique(output[[i]], ind_)
        if(is_ok_){
          output[[i]][n_filled_/2+1, ] <- ind_
          effect_pairs[pair_] <- effect_pairs[pair_] - 1
          msg_asgnd_effect[[i]] <- c(msg_asgnd_effect[[i]], ind_)
          effect_messages[ind_[1]] <- effect_messages[ind_[1]] - 1
          effect_messages[ind_[2]] <- effect_messages[ind_[2]] - 1
          break
        }
      }
    }
    
    
    if(share_count_ > 0){
      n_filled_ <- length(msg_asgnd_share[[i]])
      if(n_filled_/2 < n_pair){
        is_ok_ <- check_unique(output[[i]], ind_)
        if(is_ok_){
          output[[i]][n_pair+n_filled_/2+1, ] <- ind_
          share_pairs[pair_] <- share_pairs[pair_] - 1
          msg_asgnd_share[[i]] <- c(msg_asgnd_share[[i]], ind_)
          share_messages[ind_[1]] <- share_messages[ind_[1]] - 1
          share_messages[ind_[2]] <- share_messages[ind_[2]] - 1
          break
        }
      }
    }
  }
}


is_unique <- 0
for(i in 1:n){
  a <- 0
  a <- sum(duplicated(as.numeric(output[[i]])))
  if(a > 0)
    print(i)
  is_unique <- is_unique + a
}
