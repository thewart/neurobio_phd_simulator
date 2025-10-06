n_units <- 100
n_animals <- 3

animal <- rep(1:n_animals, each = n_units)
lambda <- rgamma(n_units * n_animals, shape = 5, rate = 1)

