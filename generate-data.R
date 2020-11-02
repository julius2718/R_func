
# Functions for Data Generation

# Data Generation
generate_data <- function(seed = NULL,
                          obs_coef = list(),
                          obs_to_lat = NULL,
                          lat_coef = list(),
                          obs_mean = list(),
                          cov_d = NULL, cov_ed = NULL, cov_e = NULL,
                          n = 300, n_obs = 1, n_lat = 1,
                          sig_ex_d = 1, sig_ex_e = 1,
                          save_as_csv = FALSE, file = "data.csv") {
  set.seed(seed)

  # Parameter Matrix
  ## Path coef. of observation eq.
  mat_obs <- matrix(0, nrow = n_obs, ncol = n_lat)

  for (i in seq_len(length(obs_coef))) {
    row <- as.integer(strsplit(names(obs_coef[i]), "_")[[1]][1])
    col <- as.integer(strsplit(names(obs_coef[i]), "_")[[1]][2])
    val <- obs_coef[[i]][1]
    mat_obs[row, col] <- val
  }

  ## Path coef. between latent variables
  mat_lat <- matrix(0, nrow = n_lat, ncol = n_lat)

  if (is.null(lat_coef) == FALSE) {
    for (i in seq_len(length(lat_coef))) {
      row <- as.integer(strsplit(names(lat_coef[i]), "_")[[1]][1])
      col <- as.integer(strsplit(names(lat_coef[i]), "_")[[1]][2])
      val <- lat_coef[[i]][1]
      mat_lat[row, col] <- val
    }
  }


  ## Path coef. from obs. to lat.
  mat_ob_la <- matrix(0, nrow = n_lat, ncol = n_obs)

  if (is.null(obs_to_lat) == FALSE) {
    for (i in seq_len(length(obs_to_lat))) {
      row <- as.integer(strsplit(names(obs_to_lat[i]), "_")[[1]][1])
      col <- as.integer(strsplit(names(obs_to_lat[i]), "_")[[1]][2])
      val <- obs_to_lat[[i]][1]
      mat_ob_la[row, col] <- val
    }
  }

  ## Path coef. between obs.
  mat_ob_ob <- matrix(0, nrow = n_obs, ncol = n_obs)

  parameter_matrix <- cbind(rbind(mat_lat, mat_obs),
                            rbind(mat_ob_la, mat_ob_ob))


  # Selection Matrix
  large_g <- cbind(matrix(0, nrow = n_obs, ncol = n_lat),
                     diag(1, nrow = n_obs, ncol = n_obs))

  # Residual Matrix
  res_d <- diag(sig_ex_d, nrow = n_lat, ncol = n_lat)

  if (is.null(cov_d) == FALSE) {
    for (i in seq_len(length(cov_d))) {
      row <- as.integer(strsplit(names(cov_d[i]), "_")[[1]][1])
      col <- as.integer(strsplit(names(cov_d[i]), "_")[[1]][2])
      res_d[row, col] <- cov_d[[i]][1]
      res_d[col, row] <- cov_d[[i]][1]
    }
  }

  res_ed <- matrix(0, nrow = n_obs, ncol = n_lat)
  res_de <- matrix(0, nrow = n_lat, ncol = n_obs)

  if (is.null(cov_ed) == FALSE) {
    for (i in seq_len(length(cov_ed))) {
      row <- as.integer(strsplit(names(cov_ed[i]), "_")[[1]][1])
      col <- as.integer(strsplit(names(cov_ed[i]), "_")[[1]][2])
      res_ed[row, col] <- cov_ed[[i]][1]
      res_de[col, row] <- cov_ed[[i]][1]
    }
  }

  res_e <- diag(sig_ex_e, nrow = n_obs, ncol = n_obs)

  if (is.null(cov_e) == FALSE) {
    for (i in seq_len(length(cov_e))) {
      row <- as.integer(strsplit(names(cov_e[i]), "_")[[1]][1])
      col <- as.integer(strsplit(names(cov_e[i]), "_")[[1]][2])
      res_e[row, col] <- cov_e[[i]][1]
      res_e[col, row] <- cov_e[[i]][1]
    }
  }

  res_mat <- rbind(cbind(res_d, res_de),
                   cbind(res_ed, res_e))

  # Calculation
  i_minus_a <- diag(1, nrow = nrow(parameter_matrix),
                    ncol = ncol(parameter_matrix)) - parameter_matrix
  large_t <- solve(i_minus_a)

  cov_structure <- large_g %*% large_t %*% res_mat %*% t(large_t) %*% t(large_g)

  mean <- as.vector(unlist(obs_mean))

  generated_data <- mvtnorm::rmvnorm(n = n, mean = mean, sigma = cov_structure)
  colnames(generated_data) <- sapply(c(1:n_obs), function(i) {
    paste("obs", as.character(i), sep = "")
  })

  generated_data <- data.frame(generated_data)

  if (save_as_csv == TRUE) {
    write.csv(generated_data, file = file)
  }

  return(generated_data)
}

# Extract Data-----------------------------------------------------------------
extract_data <- function(data, sizes = c(10), n_dataset = 10, seed = 0) {
  set.seed(seed)
  ex_datasets <- list()
  ex_datasets <- lapply(sizes, FUN = function(x) {
    .temp <- list()
    for (i in 1:n_dataset) {
      .temp <- c(.temp, list(dplyr::sample_n(tbl = data, size = x)))
    }
    return(.temp)
  })
  return(ex_datasets)
}
