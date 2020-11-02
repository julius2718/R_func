
# With Noise on Path Coefficients
with_path_noise <- function(
  N = 10000, path_pi = list(), path_lam = list(), seed = 0
) {
  set.seed(seed)
  # Exogenous Variables
  f1 <- rnorm(N, 0, 1)
  ## Observation Errors
  for (i in 1:9) {
    assign(sprintf("e%d", i), rnorm(N, 0, 1))
  }
  ## Latent Errors
  for (i in 2:3) {
    assign(sprintf("d%d", i), rnorm(N, 0, 1))
  }
  # Path Coefficients
  ## Structural Equations
  pi_21 <- rnorm(N, path_pi$pi_21, 0.5)
  pi_32 <- rnorm(N, path_pi$pi_32, 0.5)
  pi_31 <- rnorm(N, path_pi$pi_31, 0.5)
  ## Observation Equations
  no <- 1
  for (i in 1:3) {
    for (j in 1:3) {
      assign(
        sprintf("lam_%d%d", no, i),
        rnorm(N, path_lam[sprintf("lam_%d%d", no, i)][[1]], 0.5)
      )
      no <- no + 1
    }
  }
  no <- 1
  # Endogenous Variables
  ## Latent Variables
  f2 <- f1 * pi_21 + d2
  f3 <- f1 * pi_31 + f2 * pi_32 + d3
  ## Observed Variables
  dat_list <- list()
  for (i in 1:3) {
    for (j in 1:3) {
      from <- get(sprintf("f%d", i))
      lam <- get(sprintf("lam_%d%d", no, i))
      e <- get(sprintf("e%d", no))
      assign(sprintf("obs%d", no), (3.5 + from * lam + e))
      dat_list[sprintf("obs%d", no)] <- list(get(sprintf("obs%d", no)))
      no <- no + 1
    }
  }
  return(as.data.frame(dat_list))
}

two_groups <- function(
  total = 10000, ratio = 0.5,
  g1_path_pi = list(), g1_path_lam = list(),
  g2_path_pi = list(), g2_path_lam = list()
) {
  g1 <- with_path_noise(
    N = total * ratio,
    path_pi = g1_path_pi, path_lam = g1_path_lam, seed = 0
  )
  g2 <- with_path_noise(
    N = total * (1 - ratio),
    path_pi = g2_path_pi, path_lam = g2_path_lam, seed = 0
  )
  return(rbind(g1, g2))
}
