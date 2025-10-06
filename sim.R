library(data.table)

n_units <- 100
n_animals <- 3
n_trials <- rnbinom(n_units * n_animals, mu = 50, size = 5)

unit <- 1:(n_units*n_animals)
animal <- rep(1:n_animals, each = n_units)
stim <- rbinom(sum(n_trials), 1, 0.5)

eps <- rnorm(n_units * n_animals, mean = 2, sd = 0.5)
mu <- rnorm(n_animals-1, 0, 0.5)
beta <- rt(n_units * n_animals, df = 10, ncp = 0.25) * 0.1

Z <- model.matrix(~ 0 + factor(unit) + factor(animal), data.table(unit, animal))
lambda <- exp(Z %*% c(eps, mu))
