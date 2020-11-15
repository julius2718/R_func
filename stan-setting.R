
# Functions for Stan

# Stan Setting-----------------------------------------------------------------
stan_setting <- function(parallel = FALSE) {
  rstan::rstan_options(auto_write = TRUE)
  if (parallel == T) {
    options(mc.cores = parallel::detectCores())
  }
}

# Detect RStudio---------------------------------------------------------------
## Return FALSE if run on RStudio on MacOS.
## This is for avoiding bugs related to R4.0 and RStudio.
should_parallel <- function() {
  is_darwin <- Sys.info()[["sysname"]] == "Darwin"
  is_rstudio <- .Platform$GUI == "RStudio"
  return(
    ifelse(
      is_darwin && is_rstudio,
      FALSE, TRUE
    )
  )
}

# Plot MCMC Chain--------------------------------------------------------------
stan_generate_pdf <- function(file, obj, pars, height, width) {
  pdf(file = file, height = height, width = width)
  trace <- rstan::stan_trace(obj, pars = pars, inc_warmup = T) # trace plot
  dens <- rstan::stan_dens(obj, pars = pars, separate_chains = T) # density plot
  ac <- rstan::stan_ac(obj, pars = pars) # auto-correlation
  print(trace)
  print(dens)
  print(ac)
  dev.off()
}

# Extract Necessary Parameters from fit----------------------------------------
extract_summary_from_list <- function(data = list(), pars = c()) {
  extracted <- lapply(
    X = data,
    FUN = function(x) {
      return(
        subset(
          x,
          grepl(paste(pars, collapse = "|"), rownames(x))
        )
      )
    }
  )
  return(extracted)
}

extract_summary_from_df <- function(data = data.frame(), pars = c()) {
  return(
    subset(
      data,
      grepl(paste(pars, collapse = "|"), rownames(data))
    )
  )
}

# Save stanfit Objects---------------------------------------------------------
save_stanfit <- function(obj, should_save = "y", file = "fit.rds") {
  if (should_save == "y") {
    saveRDS(obj, file = file)
  } else if (should_save != "n") {
    cat("Unexpected input detected. Defauling to 'n'.")
  }
}
