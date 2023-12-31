M <- 12 # no of messages
L <- 5 # no of questions per participants
N <- 50 # no of white participants

# Pair Assignments
pairs_white <- numeric(0)
for(i in 1:N){
  pairs_white <- rbind(pairs_white, 
                       matrix(sample(1:M, 2*L), ncol = 2))
}

pairs_black <- numeric(0)
for(i in 1:N){
  pairs_black <- rbind(pairs_black, 
                       matrix(sample(1:M, 2*L), ncol = 2))
}

# Parameter Initialization
th_mu <- c(-4, 0.01, 10)
theta <- numeric(M)
for(i in 1:(M-1)){
  cl <- sample(1:length(th_mu), 1)
  theta[i] <- rnorm(1, th_mu[cl], 0.1)
}
theta[M] <- -sum(theta)

delta <- matrix(rnorm(2*(M-1)), 2, M-1)
delta <- cbind(delta, apply(delta, 1, sum))
mu <- rnorm(1)
mu <- c(mu, -mu)
gamma <- matrix(rnorm(N-1), 1, N-1)
gamma <- rbind(gamma, -gamma)
gamma <- cbind(gamma, c(-sum(gamma[1, ]), -sum(gamma[2, ])))

prob <- array(0, c(2, M, M, N))
y <- array(0, c(2, N, L))

# Calculating the probabilities
for(r in 1:2){
  for(i in 1:M){
    for(j in 1:M){
      for(k in 1:N){
        prob[r, i, j, k] <- pnorm(
          theta[i] - theta[j] + delta[r, i] - delta[r, j] +
            mu[r] + gamma[r, k]
        )
      }
    }
  }
}

# Generating responses
for(r in 1:2){
  for(k in 1:N){
    for(l in 1:L){
      if(r == 1){
        i <- pairs_white[L * (k-1) + l, 1]
        j <- pairs_white[L * (k-1) + l, 2]
      } else{
        i <- pairs_black[L * (k-1) + l, 1]
        j <- pairs_black[L * (k-1) + l, 2]
      }
      y[r, k, l] <- rbinom(1, 1, prob[r, i, j, k])
    }
  }
}