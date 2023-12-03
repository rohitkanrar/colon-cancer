p1 <- array(0, c(2, N, L))
p2 <- p1
for(r in 1:2){
  for(n in 1:N){
    for(l in 1:L){
      if(r == 1){
        p1[r,n,l] <- pairs_white[L*(n-1)+l, 1]
        p2[r,n,l] <- pairs_white[L*(n-1)+l, 2]
      } else{
        p1[r,n,l] <- pairs_black[L*(n-1)+l, 1]
        p2[r,n,l] <- pairs_black[L*(n-1)+l, 2]
      }
    }
  }
}


Theta <- matrix(0, 2 * N * L, (M - 1))
Delta <- matrix(0, 2 * N * L, 2 * (M - 1))
Mu <- numeric(2 * N * L)
Gamma <- matrix(0, 2 * N * L, 2*N)
ind <- expand.grid(l = 1:L, n = 1:N, r = 1:2)
res <- numeric(2 * N * L)

for(i in 1:nrow(Theta)){
  ind_ <- ind[i, ]
  r_ <- ind_$r
  n_ <- ind_$n
  l_ <- ind_$l
  a <- p1[r_, n_, l_]
  b <- p2[r_, n_, l_]
  res[i] <- y[r_, n_, l_]
  if(a != M && b != M){
    Theta[i, a] <- 1
    Theta[i, b] <- -1
  } else if (a == M) {
    Theta[i, ] <- -1
    Theta[i, b] <- -2
  } else{
    Theta[i, ] <- 1
    Theta[i, a] <- 2
  }
  tmp <- numeric(N)
  tmp[n_] <- 1
  if(r_ == 1){
    Delta[i, ] <- c(Theta[i, ], rep(0, (M-1)))
    Mu[i] <- 1
    Gamma[i, ] <- c(tmp, numeric(N))
  } else{
    Delta[i, ] <- c(rep(0, (M-1)), -Theta[i, ])
    Mu[i] <- -1
    Gamma[i, ] <- c(numeric(N), tmp)
  }
}

