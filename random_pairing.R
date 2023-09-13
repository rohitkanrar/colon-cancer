# setwd("Research/RA/colon-cancer/")
source("Research/RA/colon-cancer/func.R")
source("Research/RA/colon-cancer/initial_pairing.R")


# expecting "output", "msg_asgnd_effect" and "msg_asgnd_share" objects
diff <- sum(rep(repl, n_total_pairs)) - n * n_pair
while((sum(share_pairs) > diff) || (sum(effect_pairs) > diff)){
  print(sum(share_pairs))
  print(sum(effect_pairs))
  pair_ <- sample(1:n_total_pairs, 1, 
                  prob = (share_pairs + effect_pairs) / 
                    sum((share_pairs + effect_pairs)))
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
