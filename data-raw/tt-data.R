### t-test data (to test replication-wise confidence intervals for coverage)
set.seed(2398756) # random seed
obs <- 90 # total number of observations
gr_ratio <- 2 # using 1 gives equal number of obs per group
B <- 500 # Replications
# Object with results
tt.df <- vector(mode = "list", length = B)
# Loop (for ftw)
for (i in seq(B)) {
  # Simulate data
  group <- seq(obs) > obs / ((gr_ratio + 1) / gr_ratio)
  # Normal data
  ynormeq <- c(
    rnorm(obs - sum(group), mean = 5, sd = 5),
    rnorm(sum(group), mean = 6, sd = 5)
  )
  ynormuneq <- c(
    ynormeq[group == FALSE],
    rnorm(sum(group), mean = 6, sd = 10)
  )
  # Here we add Gamma-distributed data.
  # We add a constant to match normal mean, var and diff
  ygammeq <- c(
    rgamma(obs - sum(group), shape = 1, scale = 5),
    rgamma(sum(group), shape = 1, scale = 5) + 1
  )
  ygammuneq <- c(
    ygammeq[group == FALSE],
    rgamma(sum(group), shape = 1, scale = 10) - 4
  )

  # Make a list to store single outputs
  out <- vector(mode = "list", length = 8)

  # t-test for normal data with equal variance (DGM = 1), t-test with pooled variance (method = 1)
  tt <- t.test(ynormeq ~ group, paired = FALSE, var.equal = TRUE)
  out[[1]] <- data.frame(
    diff = with(tt, estimate[1] - estimate[2]),
    se = tt$stderr,
    lower = tt$conf.int[1],
    upper = tt$conf.int[2],
    df = tt$parameter,
    repno = i,
    dgm = 1,
    method = 1,
    row.names = NULL
  )
  # t-test for normal data with unequal variance (DGM = 2), t-test with pooled variance (method = 1)
  tt <- t.test(ynormuneq ~ group, paired = FALSE, var.equal = TRUE)
  out[[2]] <- data.frame(
    diff = with(tt, estimate[1] - estimate[2]),
    se = tt$stderr,
    lower = tt$conf.int[1],
    upper = tt$conf.int[2],
    df = tt$parameter,
    repno = i,
    dgm = 2,
    method = 1,
    row.names = NULL
  )
  # t-test for normal data with equal variance (DGM = 1), t-test without pooled variance (method = 2)
  tt <- t.test(ynormeq ~ group, paired = FALSE, var.equal = FALSE)
  out[[3]] <- data.frame(
    diff = with(tt, estimate[1] - estimate[2]),
    se = tt$stderr,
    lower = tt$conf.int[1],
    upper = tt$conf.int[2],
    df = tt$parameter,
    repno = i,
    dgm = 1,
    method = 2,
    row.names = NULL
  )
  # t-test for normal data with unequal variance (DGM = 2), t-test without pooled variance (method = 2)
  tt <- t.test(ynormuneq ~ group, paired = FALSE, var.equal = FALSE)
  out[[4]] <- data.frame(
    diff = with(tt, estimate[1] - estimate[2]),
    se = tt$stderr,
    lower = tt$conf.int[1],
    upper = tt$conf.int[2],
    df = tt$parameter,
    repno = i,
    dgm = 2,
    method = 2,
    row.names = NULL
  )
  # t-test for gamma data with equal variance (DGM = 3), t-test with pooled variance (method = 1)
  tt <- t.test(ygammeq ~ group, paired = FALSE, var.equal = TRUE)
  out[[5]] <- data.frame(
    diff = with(tt, estimate[1] - estimate[2]),
    se = tt$stderr,
    lower = tt$conf.int[1],
    upper = tt$conf.int[2],
    df = tt$parameter,
    repno = i,
    dgm = 3,
    method = 1,
    row.names = NULL
  )
  # t-test for gamma data with unequal variance (DGM = 4), t-test with pooled variance (method = 1)
  tt <- t.test(ygammuneq ~ group, paired = FALSE, var.equal = TRUE)
  out[[6]] <- data.frame(
    diff = with(tt, estimate[1] - estimate[2]),
    se = tt$stderr,
    lower = tt$conf.int[1],
    upper = tt$conf.int[2],
    df = tt$parameter,
    repno = i,
    dgm = 4,
    method = 1,
    row.names = NULL
  )
  # t-test for gamma data with equal variance (DGM = 3), t-test without pooled variance (method = 2)
  tt <- t.test(ygammeq ~ group, paired = FALSE, var.equal = FALSE)
  out[[7]] <- data.frame(
    diff = with(tt, estimate[1] - estimate[2]),
    se = tt$stderr,
    lower = tt$conf.int[1],
    upper = tt$conf.int[2],
    df = tt$parameter,
    repno = i,
    dgm = 3,
    method = 2,
    row.names = NULL
  )
  # t-test for gamma data with unequal variance (DGM = 4), t-test without pooled variance (method = 2)
  tt <- t.test(ygammuneq ~ group, paired = FALSE, var.equal = FALSE)
  out[[8]] <- data.frame(
    diff = with(tt, estimate[1] - estimate[2]),
    se = tt$stderr,
    lower = tt$conf.int[1],
    upper = tt$conf.int[2],
    df = tt$parameter,
    repno = i,
    dgm = 4,
    method = 2,
    row.names = NULL
  )

  # Make results to return
  out <- do.call(rbind.data.frame, out)
  tt.df[[i]] <- out
}
tt <- do.call(rbind.data.frame, tt.df)

### Export for use in the package
usethis::use_data(tt, overwrite = TRUE)
