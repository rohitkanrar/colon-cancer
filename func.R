check_unique <- function(pairs, new_pair){
  new_pair <- as.numeric(new_pair)
  pairs <- as.numeric(pairs)
  unq <- sum(pairs == new_pair[1]) + sum(pairs == new_pair[2])
  if(unq == 0)
    unq <- TRUE
  else
    unq <- FALSE
  return(unq)
}

array_to_pair <- function(i, j) i + (j - 1) * (j - 2) / 2
pair_to_array <- function(pair) as.numeric(which(all_pairs == pair, arr.ind = T))


# check_exchange <- function(pairs, new_pair){
#   k <- 0
#   for(i in 1:nrow(pairs)){
#     k <- k + as.numeric(check_unique(pairs[-i, ], new_pair))
#     if(k > 0)
#       break
#   }
#   if(k > 0)
#     return(FALSE)
#   else
#     return(TRUE)
# }

mat_to_vec <- function(pairs){
  n_row_ <- nrow(pairs)
  vec_ <- numeric(0)
  for(i in 1:n_row_){
    vec_ <- c(vec_, pairs[i, ])
  }
  vec_
}

shuffle_pairs <- function(output){
  n <- length(output)
  n_pair <- nrow(output[[1]]) / 2
  ind <- sample(1:n, n, replace = FALSE)
  output <- output[ind]
  for(i in 1:n){
    ind_ <- c(sample(1:n_pair, n_pair, replace = FALSE), 
              sample((n_pair+1):(2*n_pair), n_pair, replace = FALSE))
    output[[i]] <- output[[i]][ind_, ]
  }
  return(output)
}

library(plyr)

get_freq_of_messages <- function(output){
  effect_messages <- numeric(0)
  share_messages <- numeric(0)
  n <- length(output)
  n_pair <- nrow(output[[1]]) / 2
  for(i in 1:n){
    effect_messages <- c(effect_messages, as.numeric(output[[i]][1:n_pair, ]))
    share_messages <- c(share_messages, 
                        as.numeric(output[[i]][(n_pair+1):(2*n_pair), ]))
  }
  return(list(count(effect_messages), count(share_messages)))
}

get_dist_of_pairs <- function(messages_count){
  prob_m <- messages_count / sum(messages_count)
  prob_pairs <- numeric(n_total_pairs)
  for(i in 1:n_total_pairs){
    ind_ <- pair_to_array(i)
    prob_pairs[i] <- 2 * prob_m[ind_[1]] * prob_m[ind_[2]]
  }
  # prob_pairs
  prob_pairs <- prob_pairs / sum(prob_pairs)
  return(prob_pairs)
}
