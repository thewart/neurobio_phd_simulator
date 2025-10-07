library(data.table)

n_units <- 100
n_animals <- 3
n_trials <- rnbinom(n_units * n_animals, mu = 50, size = 5)

X <- data.table(
  stim = rbinom(sum(n_trials), 1, 0.5),
  animal = rep(1:n_animals, 
               times = matrix(n_trials, ncol = n_animals) |> colSums()),
  unit = apply(matrix(n_trials, ncol = n_animals), 2, \(x) rep(1:n_units, times = x)) |> unlist()
)

eps <- rnorm(n_units * n_animals, sd = 0.5)
mu <- rnorm(n_animals, 2, 0.5)
beta <- rt(n_units * n_animals, df = 10, ncp = 0) * 0.1

X_wide <- cbind(model.matrix(~ 0 + factor(animal) + stim:factor(unit):factor(animal), X), 
                model.matrix(~ 0 + factor(unit):factor(animal), X))
X[, y := rpois(.N, exp(X_wide %*% c(mu, beta, eps)))]

# insert errors
X[, unit_id := paste(animal, unit, sep = ":")]

dead_cell <- X[, mean(y) > 5 & .N > 50, by = unit_id][V1 == T, unit_id] |> sample(1)
X[unit_id == dead_cell, y := replace(y, ((.N %/% 2) + sample(-20:10, 1)):.N, 0)]

bad_measure <- X[, sample(unit_id, 1)]
X[unit_id == bad_measure, y:= runif(.N, 0, 1e6)]


### analysis ####
X <- X[!(unit_id %in% c(dead_cell, bad_measure))]

X[, mean(y[stim==T] - mean(y[stim==F])), by = unit_id][, mean(V1)]

h0 <- matrix(nrow = X[, uniqueN(unit_id)], ncol = 1e3)
for (i in 1:1e3) h0[, i] <- X[, .(y, stim = sample(stim)), by = unit_id][
  , mean(y[stim==T] - mean(y[stim==F])), by = unit_id][, V1]

quantile(colMeans(h0), c(0.025, 0.975))


