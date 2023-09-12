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


