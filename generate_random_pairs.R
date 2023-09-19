# specify root directory
wkdir <- "Research/RA/colon-cancer/"
# run supplementary functions
source(paste(wkdir, "func.R", sep = ""))
# no of messages
m <- 72 
# total number of pairs
n_total_pairs <- choose(m, 2)
# no of participants (in each group)
n <- 500
# no of pairs each participants will see (type: effect and share separately)
n_pair <- 15


first_time_run <- TRUE

# initialization
source(paste(wkdir, "initialization.R", sep = ""))

# initial pairing phase
source(paste(wkdir, "initial_pairing.R", sep = ""))

# random pairing phase
# expecting "output", "msg_asgnd_effect" and "msg_asgnd_share" objects
diff <- sum(rep(repl, n_total_pairs)) - n * n_pair
loop_count <- 1

while((sum(share_pairs) > diff) || (sum(effect_pairs) > diff)){
  # random pairing
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
  loop_count <- loop_count + 1
}

# exchange phase
# source(paste(wkdir, "exchange.R", sep = ""))
