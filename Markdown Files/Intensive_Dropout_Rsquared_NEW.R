## ============================================================================
## INTENSIVE DROPOUT WITH R²-BASED MNAR MISSINGNESS - HAZARD CALIBRATION
## ============================================================================

library(mvtnorm)
library(ggplot2)

## ============================================================================
## MISSINGNESS MODEL R² TARGETS - EDIT THESE TO CHANGE MNAR STRENGTH
## ============================================================================
## NOTE: Level-2 missingness uses RESIDUALS (u_b0i, u_logvi) not totals
##       This gives orthogonal, non-overlapping R² contributions:
##       - r2_group:  Direct group effect on missingness
##       - r2_b0i:   Random intercept deviations (person-specific baseline y)
##       - r2_logvi:  Log residual variance deviations (person-specific variability)
##
## DROPOUT: All predictors are multiplied by switch dummy d (0 for days 0-6, 1 thereafter)
##          This ensures zero missingness for the first 7 days, then dropout thereafter
##          No random intercept (u_0j) - can't estimate with no within-person variation in 1s
##          days_prelim uses 0:19 (NOT 0:99) to correctly reflect p_d1 = 13/20 in Sigma_L1
##
## HAZARDS: Specify conditional dropout probabilities for days 7-19 (not cumulative!)
##          These are P(dropout at day t | still in study at day t)
##          Days 0-6 have zero dropout by design (d = 0 forces intercept to -Inf)
## ============================================================================

ICC_target <- 0.35        # ICC for full model
R2_between <- 0.50        # Total R² at Level-2
R2_within  <- 0.50        # Total R² at Level-1

# Level-2 variance partitioning (as proportions of total between variance)
# These should sum to R2_between - ORTHOGONAL (no overlap)
r2_group  <- 0.05         # Group effect (direct)
r2_b0i   <- 0.20         # Random intercept residuals (u_b0i)
r2_logvi  <- 0.25         # Log variance residuals (u_logvi)
# Remaining: sigma2_u0 = 0 for dropout (no random intercept)

# Level-1 variance partitioning (as proportions of total within variance)
# These should sum to R2_within
r2_y    <- 0.40           # (y - alpha): deviation from person intercept b0i
r2_lagy <- 0.10           # (y.lag - alpha): lagged deviation from person intercept b0i
# Remaining: (1 - R2_within) = residual e_ij (fixed at 1 in fitted model)

# Target CONDITIONAL DROPOUT HAZARDS for days 7-19
# P(dropout at day t | still in study at day t)
# ~5.7% per day yields ~53% cumulative dropout by day 19 (13 at-risk days)
hazard_target <- setNames(rep(0.057, 13), as.character(7:19))

## ============================================================================
## DATA GENERATION PARAMETERS
## ============================================================================

N          <- 125          # Number of clusters (participants)
N_prelim   <- 50000        # For stable covariance estimates
seed       <- 12345
days       <- 0:19         # 20 observations per cluster
days_prelim <- 0:19        # Use same 20-day structure (NOT 0:99 - need correct p_d1 = 13/20)

## Fixed effects for random intercepts and slopes
b0icept <- 4.308
b0group <- -0.381
b1icept <- -0.082
b1group <- -0.15

## Variance-covariance matrix for (b0i, b1i, logvi)
sigma_b0b1logvi <- matrix(c(
  0.676,   0.0228,  -0.217,
  0.0228,  0.03,    -0.0288,
  -0.217,  -0.0288,   0.457
), nrow = 3, byrow = TRUE)

# x generation parameters (ICC = 0.35)
x_between_var <- 0.582
x_within_var  <- 1.080
x_grand_mean  <- 4.048

## ============================================================================
## COMPUTE VARIANCE COMPONENTS FROM R² TARGETS
## ============================================================================

cat("\n")
cat(rep("=", 80), "\n", sep="")
cat("R²-BASED MNAR MISSINGNESS MODEL - INTENSIVE DROPOUT WITH HAZARD CALIBRATION\n")
cat(rep("=", 80), "\n\n", sep="")

# Derived quantities
W         <- 1 / (1 - R2_within)
B         <- ICC_target * W / (1 - ICC_target)
sigma2_u0 <- (1 - R2_between) * B
sigma2_u0 <- 0   # OVERRIDE for dropout: no random intercept (no within-person variation in 1s)
sigma2_e  <- 1.0

cat("Target Configuration:\n")
cat(sprintf("  ICC: %.3f  |  R²_between: %.3f  |  R²_within: %.3f\n",
            ICC_target, R2_between, R2_within))
cat(sprintf("  Between variance: %.4f  |  Within variance: %.4f\n", B, W))
cat(sprintf("  Group probability: %.2f\n", 0.36))
cat("  NOTE: sigma2_u0 = 0 for dropout (no random intercept)\n\n")

## ============================================================================
## STEP 1: COMPUTE LEVEL-2 COVARIANCE MATRIX (ANALYTICAL)
## ============================================================================

cat("Step 1: Computing Level-2 covariance matrix...\n")

# Proportion in group = 1
p_group  <- 0.36
var_group <- p_group * (1 - p_group)

# Variances of residuals from sigma_b0b1logvi (u_b1i NOT used in missingness model)
var_u_b0i        <- sigma_b0b1logvi[1, 1]
var_u_logvi      <- sigma_b0b1logvi[3, 3]
cov_u_b0i_u_logvi <- sigma_b0b1logvi[1, 3]

# For DROPOUT: Need covariance of d*predictors
# d = 0 for days 0-6, d = 1 for days 7-19 => p_d1 = 13/20 = 0.65
p_d1 <- 13/20

# Var(d * X) for binary d: p_d1 * Var(X) + p_d1*(1-p_d1) * E(X)^2
# For binary group: E(group) = p_group, so add quadratic term
var_d_group        <- p_d1 * var_group + p_d1 * (1 - p_d1) * p_group^2
# For zero-mean random effects: E(u_b0i) = E(u_logvi) = 0, so no quadratic term
var_d_u_b0i        <- p_d1 * var_u_b0i
var_d_u_logvi      <- p_d1 * var_u_logvi
cov_d_u_b0i_d_u_logvi <- p_d1 * cov_u_b0i_u_logvi

# Group is independent of random effects (cov = 0)
Sigma_L2 <- matrix(c(
  var_d_group,           0,                      0,
  0,                     var_d_u_b0i,            cov_d_u_b0i_d_u_logvi,
  0,                     cov_d_u_b0i_d_u_logvi,  var_d_u_logvi
), nrow = 3, byrow = TRUE)

cat("  Done. Using RESIDUALS with DROPOUT switch (d*predictors). p_d1 =", p_d1, "\n")
cat("  Correlation(u_b0i, u_logvi):",
    round(cov_u_b0i_u_logvi / sqrt(var_u_b0i * var_u_logvi), 3), "\n\n")

## ============================================================================
## STEP 2: GENERATE PRELIMINARY DATA FOR LEVEL-1 COVARIANCE
## ============================================================================

cat("Step 2: Generating preliminary data (N =", N_prelim, ")...\n")

set.seed(seed)

# Joint distribution: male x group (independent, each 50/50 split within)
prob_joint <- t(matrix(c(
  0.64 * 0.5, 0.36 * 0.5,   # male = 0
  0.64 * 0.5, 0.36 * 0.5),  # male = 1
  ncol = 2))
rownames(prob_joint) <- c(0, 1)
colnames(prob_joint) <- c(0, 1)

combos <- as.data.frame(as.table(prob_joint))
sim <- combos[sample(seq_len(nrow(combos)), size = N_prelim, replace = TRUE,
                     prob = combos$Freq), c("Var1", "Var2")]
names(sim) <- c("male", "group")
sim$male  <- as.integer(as.character(sim$male))
sim$group <- as.integer(as.character(sim$group))

# Fixed effects
b0i_hat <- b0icept + b0group * sim$group
b1i_hat <- b1icept + b1group * sim$group

# Random effects: (b0i, b1i, logvi) ~ MVN(0, sigma_b0b1logvi)
u <- rmvnorm(N_prelim, c(0, 0, 0), sigma_b0b1logvi)
sim$b0i   <- b0i_hat + u[, 1]
sim$b1i   <- b1i_hat + u[, 2]
sim$logvi <- -1.150 + u[, 3]
sim$id <- 1:N_prelim

# Expand to long format using 20-day structure (days_prelim = 0:19)
sim_long <- sim[rep(seq_len(nrow(sim)), each = length(days_prelim)), ]
sim_long$day <- rep(days_prelim, times = nrow(sim))
sim_long <- sim_long[order(sim_long$id, sim_long$day), ]

# Generate x with ICC = 0.35
x_mean_j  <- rnorm(N_prelim, mean = x_grand_mean, sd = sqrt(x_between_var))
x_mean_ij <- rep(x_mean_j, each = length(days_prelim))
sim_long$x <- x_mean_ij + rnorm(nrow(sim_long), mean = 0, sd = sqrt(x_within_var))

# Within-cluster centering
sim_long$x_pm <- ave(sim_long$x, sim_long$id, FUN = mean)
sim_long$x_wc <- sim_long$x - sim_long$x_pm

# Heterogeneous residual variance: sigma^2_i = exp(logvi)
sim_long$var_cluster <- exp(sim_long$logvi)

# Generate outcome
yhat <- sim_long$b0i + sim_long$x_wc * sim_long$b1i
sim_long$res <- rnorm(nrow(sim_long), 0, sqrt(sim_long$var_cluster))
sim_long$y   <- yhat + sim_long$res

# MNAR deviation scores: (y - alpha) and lag(y - alpha)
# alpha = person-specific random intercept b0i
sim_long$y_dev_alpha <- sim_long$y - sim_long$b0i
sim_long$y_dev_alpha_lag <- ave(sim_long$y_dev_alpha, sim_long$id,
                                FUN = function(x) c(0, x[-length(x)]))

# Switch dummy for dropout (0 for days 0-6, 1 for days 7-19)
sim_long$d <- ifelse(sim_long$day < 7, 0, 1)

# Level-1 predictors: d*y_dev_alpha and d*y_dev_alpha_lag
# Using 20-day preliminary data ensures correct p_d1 = 13/20 in Sigma_L1
sim_long$d_y_dev_alpha     <- sim_long$d * sim_long$y_dev_alpha
sim_long$d_y_dev_alpha_lag <- sim_long$d * sim_long$y_dev_alpha_lag

pred_vars   <- c("d_y_dev_alpha", "d_y_dev_alpha_lag")
X_L1_prelim <- as.matrix(sim_long[, pred_vars])
Sigma_L1    <- cov(X_L1_prelim)

cat("  Done. (days_prelim = 0:19, p_d1 = 13/20 =", p_d1, ")\n\n")

## ============================================================================
## STEP 3: SOLVE FOR MISSINGNESS MODEL COEFFICIENTS
## ============================================================================

cat("Step 3: Solving for coefficients...\n")

# Level-2 coefficients (for d*predictors)
target_var_group  <- r2_group * B
target_var_b0i   <- r2_b0i  * B
target_var_logvi  <- r2_logvi * B
target_vars_L2 <- c(target_var_group, target_var_b0i, target_var_logvi)

gamma_init   <- sqrt(target_vars_L2 / diag(Sigma_L2))
var_check    <- as.numeric(t(gamma_init) %*% Sigma_L2 %*% gamma_init)
scale_factor <- sqrt(sum(target_vars_L2) / var_check)
gamma_L2     <- gamma_init * scale_factor

# Level-1 coefficients (for d*y_dev_alpha and d*y_dev_alpha_lag)
target_vars_L1 <- c(r2_y * W, r2_lagy * W)

beta_init    <- sqrt(target_vars_L1 / diag(Sigma_L1))
var_check    <- as.numeric(t(beta_init) %*% Sigma_L1 %*% beta_init)
scale_factor <- sqrt(sum(target_vars_L1) / var_check)
beta_L1      <- beta_init * scale_factor

cat("  Level-2 coefficients: d*group =", round(gamma_L2[1], 3),
    "| d*u_b0i =", round(gamma_L2[2], 3),
    "| d*u_logvi =", round(gamma_L2[3], 3), "\n")
cat("  Level-1 coefficients: d*y_dev =", round(beta_L1[1], 3),
    "| d*y_dev_lag =", round(beta_L1[2], 3), "\n\n")

## ============================================================================
## STEP 4: GENERATE ACTUAL DATA
## ============================================================================

cat("Step 4: Generating actual data (N =", N, ")...\n")

set.seed(seed)

# Joint distribution
sim <- combos[sample(seq_len(nrow(combos)), size = N, replace = TRUE,
                     prob = combos$Freq), c("Var1", "Var2")]
names(sim) <- c("male", "group")
sim$male  <- as.integer(as.character(sim$male))
sim$group <- as.integer(as.character(sim$group))

# Fixed effects
b0i_hat <- b0icept + b0group * sim$group
b1i_hat <- b1icept + b1group * sim$group

# Random effects
u <- rmvnorm(N, c(0, 0, 0), sigma_b0b1logvi)
sim$b0i   <- b0i_hat + u[, 1]
sim$b1i   <- b1i_hat + u[, 2]
sim$logvi <- -1.150 + u[, 3]
sim$id <- 1:N

# Expand to long format
sim_long <- sim[rep(seq_len(nrow(sim)), each = length(days)), ]
sim_long$day <- rep(days, times = nrow(sim))
sim_long <- sim_long[order(sim_long$id, sim_long$day), ]

# Generate x
x_mean_j  <- rnorm(N, mean = x_grand_mean, sd = sqrt(x_between_var))
x_mean_ij <- rep(x_mean_j, each = length(days))
sim_long$x <- x_mean_ij + rnorm(nrow(sim_long), mean = 0, sd = sqrt(x_within_var))

# Within-cluster centering
sim_long$x_pm <- ave(sim_long$x, sim_long$id, FUN = mean)
sim_long$x_wc <- sim_long$x - sim_long$x_pm

# Heterogeneous residual variance
sim_long$var_cluster <- exp(sim_long$logvi)

# Generate outcome
yhat <- sim_long$b0i + sim_long$x_wc * sim_long$b1i
sim_long$res <- rnorm(nrow(sim_long), 0, sqrt(sim_long$var_cluster))
sim_long$y   <- yhat + sim_long$res

# MNAR deviation scores
sim_long$y_dev_alpha <- sim_long$y - sim_long$b0i
sim_long$y_dev_alpha_lag <- ave(sim_long$y_dev_alpha, sim_long$id,
                                FUN = function(x) c(0, x[-length(x)]))

# Switch dummy for dropout (0 for days 0-6, 1 for days 7-19)
sim_long$d <- ifelse(sim_long$day < 7, 0, 1)

# Extract random effect residuals for missingness model (orthogonal to group)
sim$u_b0i  <- sim$b0i  - (b0icept + b0group * sim$group)
sim$u_b1i  <- sim$b1i  - (b1icept + b1group * sim$group)
sim$u_logvi <- sim$logvi - (-1.150)
sim_long$u_b0i  <- rep(sim$u_b0i,  each = length(days))
sim_long$u_b1i  <- rep(sim$u_b1i,  each = length(days))
sim_long$u_logvi <- rep(sim$u_logvi, each = length(days))

long <- sim_long
cat("  Done.\n\n")

## ============================================================================
## STEP 5: GENERATE MISSINGNESS (DROPOUT WITH HAZARD CALIBRATION)
## ============================================================================

cat("Step 5: Generating missingness (dropout with hazard calibration)...\n")

# Build linear predictor using d*predictors (zero for days 0-6 by construction)
X_L2 <- cbind(
  d_group   = long$d * long$group,
  d_u_b0i  = long$d * long$u_b0i,
  d_u_logvi = long$d * long$u_logvi
)
X_L1 <- cbind(
  d_y_dev_alpha     = long$d * long$y_dev_alpha,
  d_y_dev_alpha_lag = long$d * long$y_dev_alpha_lag
)

# Compute linear predictors (without intercept)
eta_L2 <- X_L2 %*% gamma_L2
# No u_0j for dropout (cannot estimate random intercept with no within-person variation in 1s)
eta_L1 <- X_L1 %*% beta_L1
e_ij   <- rnorm(nrow(long), mean = 0, sd = sqrt(sigma2_e))

# Combine (without intercept)
eta_no_intercept <- eta_L2 + eta_L1 + e_ij

# Calibrate to CONDITIONAL HAZARDS for days 7-19
cat("  Calibrating day-specific intercepts to target HAZARDS...\n")

# Track dropout status across the sample
long$dropped <- FALSE
long <- long[order(long$id, long$day), ]

alpha_vec <- setNames(rep(-50, 7), as.character(0:6))  # Days 0-6 forced to ~0% dropout

set.seed(seed)
for (d_name in names(hazard_target)) {
  d <- as.integer(d_name)

  # Only calibrate among those NOT YET DROPPED at this day
  still_in_mask <- !long$dropped & long$day == d
  idx <- which(still_in_mask)

  if (length(idx) == 0) {
    warning(sprintf("Everyone dropped by day %s", d_name))
    next
  }

  targ  <- hazard_target[d_name]
  eta_d <- eta_no_intercept[idx]

  # Solve for intercept that gives mean(pnorm(alpha + eta)) = targ among those still in
  f <- function(a) mean(pnorm(a + eta_d)) - targ

  tryCatch({
    alpha_vec[d_name] <- uniroot(f, interval = c(-10, 10), extendInt = "yes")$root
  }, error = function(e) {
    alpha_vec[d_name] <- qnorm(targ) - mean(eta_d)
    warning(sprintf("Day %s: Using approximate intercept", d_name))
  })

  # Determine who drops at this day (among those still in)
  p_drop    <- pnorm(alpha_vec[d_name] + eta_d)
  drops_now <- runif(length(idx)) < p_drop

  # Mark as dropped and propagate all future days forward
  if (any(drops_now)) {
    ids_drop <- unique(long$id[idx[drops_now]])
    long$dropped[long$id %in% ids_drop & long$day >= d] <- TRUE
  }
}

# Set miss_flag: 0 = observed, 1 = dropout at that day, NA = already dropped
long$miss_flag <- 0L
for (i in unique(long$id)) {
  idx <- which(long$id == i)
  if (any(long$dropped[idx])) {
    first_drop <- min(long$day[idx][long$dropped[idx]])
    long$miss_flag[idx[long$day[idx] == first_drop]] <- 1L
    long$miss_flag[idx[long$day[idx] > first_drop]] <- NA
  }
}

long$missing <- long$miss_flag
long$y_mnar <- long$y
long$y_mnar[long$missing == 1L | is.na(long$missing)] <- NA

# Calculate cumulative dropout rates at each day
cumulative_dropout <- numeric(length(days))
names(cumulative_dropout) <- as.character(days)
for (d in days) {
  dropout_status <- tapply(long$miss_flag[long$day <= d],
                           long$id[long$day <= d],
                           function(x) any(x == 1 | is.na(x), na.rm = TRUE))
  cumulative_dropout[as.character(d)] <- mean(dropout_status, na.rm = TRUE) * 100
}

cat("\n  Cumulative Dropout Rates (days 7-19):\n")
cat("  ======================================\n")
cumulative_target_pct <- 0
for (d_name in names(hazard_target)) {
  d <- as.integer(d_name)
  cumulative_target_pct <- cumulative_target_pct +
    (1 - cumulative_target_pct) * hazard_target[d_name]
  cat(sprintf("  Day %2d: Cumulative Target = %5.1f%%, Achieved = %5.1f%%\n",
              d, cumulative_target_pct * 100, cumulative_dropout[d_name]))
}
cat(sprintf("\n  Overall dropout rate (by day 19): %.1f%%\n\n", cumulative_dropout["19"]))

## ============================================================================
## STEP 6: DIAGNOSTICS
## ============================================================================

cat("Step 6: Validating variance decomposition...\n")

var_eta_L2 <- var(eta_L2)
var_eta_L1 <- var(eta_L1)
var_eij    <- var(e_ij)

between_var <- var_eta_L2   # No random intercept for dropout
within_var  <- var_eta_L1 + var_eij
total_var   <- var(eta_no_intercept)

ICC_achieved        <- between_var / total_var
R2_between_achieved <- var_eta_L2 / between_var
R2_within_achieved  <- var_eta_L1 / within_var

cat(sprintf("  ICC:        Target = %.3f,  Achieved = %.3f  (%.1f%% error)\n",
            ICC_target, ICC_achieved,
            100*(ICC_achieved - ICC_target)/ICC_target))
cat(sprintf("  R²_between: Target = %.3f,  Achieved = %.3f  (%.1f%% error)\n",
            R2_between, R2_between_achieved,
            100*(R2_between_achieved - R2_between)/R2_between))
cat(sprintf("  R²_within:  Target = %.3f,  Achieved = %.3f  (%.1f%% error)\n\n",
            R2_within, R2_within_achieved,
            100*(R2_within_achieved - R2_within)/R2_within))

# Decompose Level-2 variance by predictor
var_group_component <- var(long$d * long$group   * gamma_L2[1])
var_b0i_component   <- var(long$d * long$u_b0i  * gamma_L2[2])
var_logvi_component <- var(long$d * long$u_logvi * gamma_L2[3])

cat("Level-2 Variance Decomposition:\n")
cat(sprintf("  d*Group contribution:   %.4f (%.1f%% of between_var)\n",
            var_group_component, 100*var_group_component/between_var))
cat(sprintf("  d*u_b0i contribution:   %.4f (%.1f%% of between_var)\n",
            var_b0i_component, 100*var_b0i_component/between_var))
cat(sprintf("  d*u_logvi contribution: %.4f (%.1f%% of between_var)\n\n",
            var_logvi_component, 100*var_logvi_component/between_var))

# Probit MNAR signal (post-day-4 observations only; NAs auto-excluded by glm)
long_post6 <- long[long$day > 6, ]
fit_time <- glm(I(missing == 1) ~ factor(day),
                data = long_post6, family = binomial(link = "probit"))
fit_full <- glm(I(missing == 1) ~ factor(day) + group + u_b0i + u_logvi +
                  y_dev_alpha + y_dev_alpha_lag,
                data = long_post6, family = binomial(link = "probit"))

LL_time  <- as.numeric(logLik(fit_time))
LL_full  <- as.numeric(logLik(fit_full))
delta_R2 <- 1 - LL_full / LL_time

cat("Step 7: MNAR signal strength (post-day-6 observations only)\n")
cat(sprintf("  McFadden ΔR² (MNAR vs. time-only): %.4f\n\n", delta_R2))

## ============================================================================
## PLOTS
## ============================================================================

mean_by_day <- aggregate(y ~ day + group, data = long, FUN = mean, na.rm = TRUE)

ggplot(mean_by_day, aes(x = day, y = y, color = factor(group), group = group)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("steelblue", "firebrick"),
                     labels = c("Group 0", "Group 1"), name = "Group") +
  labs(x = "Day", y = "Mean y",
       title = "Mean y by Day and Group (Intensive Dropout Pattern)") +
  theme_minimal(base_size = 14)

## ============================================================================
## SAVE OUTPUT
## ============================================================================

mnar <- long[, c('id', 'day', 'day', 'y_mnar', 'y', 'x', 'x', 'miss_flag', 'group')]
names(mnar) <- c('l2id', 'occasion', 'time', 'y', 'ycom', 'x', 'xcom', 'm', 'group')
mnar$x <- ifelse(is.na(mnar$y), NA, mnar$xcom)

fdir::set()
save(mnar, file = 'intensive-dropout.rds')
write.csv(mnar, file = 'intensive-dropout.csv', row.names = FALSE)

cat(rep("=", 80), "\n", sep="")
cat("COMPLETE - INTENSIVE DROPOUT PATTERN GENERATED WITH HAZARD CALIBRATION\n")
cat(sprintf("Days 0-6 complete (d=0), days 7-19 at risk of dropout (d=1), N=%d\n", N))
cat(sprintf("Group 1: %.1f%%, Group 0: %.1f%%\n",
            mean(long$group) * 100, (1 - mean(long$group)) * 100))
cat(rep("=", 80), "\n\n", sep="")
