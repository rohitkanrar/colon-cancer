#browser()

for(p in which(effect_pairs == (repl - 1))){
  ind_ <- pair_to_array(p)
  status <- FALSE
  i <- 1
  while(!status && (i <= n)){
    # print(i)
    if(check_unique(output[[i]], ind_)){
      j <- 1
      while(!status && (j <= n_pair)){
        ind1_ <- as.numeric(output[[i]][j, ])
        p_ <- array_to_pair(ind1_[1], ind1_[2])
        if(effect_pairs[p_] < (repl - 2)){
          status <- TRUE
          output[[i]][j, ] <- ind_
          effect_pairs[p_] <- effect_pairs[p_] + 1
          effect_pairs[p] <- effect_pairs[p] - 1
        }
      }
      j <- j + 1
    }
    i <- i + 1
  }
}


for(p in which(share_pairs == (repl - 1))){
  ind_ <- pair_to_array(p)
  status <- FALSE
  i <- 1
  while(!status && (i <= n)){
    # print(i)
    if(check_unique(output[[i]], ind_)){
      j <- 1
      while(!status && (j <= n_pair)){
        ind1_ <- as.numeric(output[[i]][j, ])
        p_ <- array_to_pair(ind1_[1], ind1_[2])
        if(share_pairs[p_] < (repl - 2)){
          status <- TRUE
          output[[i]][j, ] <- ind_
          share_pairs[p_] <- share_pairs[p_] + 1
          share_pairs[p] <- share_pairs[p] - 1
        }
      }
      j <- j + 1
    }
    i <- i + 1
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
