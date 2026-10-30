## ============================================================================
## GROWTH DROPOUT WITH R²-BASED MNAR MISSINGNESS - HAZARD CALIBRATION
## ============================================================================

library(mvtnorm)
library(ggplot2)

## ============================================================================
## MISSINGNESS MODEL R² TARGETS - EDIT THESE TO CHANGE MNAR STRENGTH
## ============================================================================
## NOTE: Level-2 missingness uses RESIDUALS (u_b0i, u_b1i) not totals (b0i, b1i)
##       This gives orthogonal, non-overlapping R² contributions:
##       - r2_drug: Direct drug effect on missingness
##       - r2_b0i: Random intercept deviations (person-specific baseline)
##       - r2_b1i: Random slope deviations (person-specific trajectory)
##
## DROPOUT: All predictors are multiplied by switch dummy d (0 at baseline, 1 after)
##          This ensures zero missingness at baseline, then dropout thereafter
##          No random intercept (u_0j) - can't estimate with no within-person variation
##
## HAZARDS: Specify conditional dropout probabilities (not cumulative rates!)
##          These are the probabilities of dropping at time t given still in study
## ============================================================================

ICC_target <- 0.35        # ICC for full model
R2_between <- 0.50        # Total R² at Level-2
R2_within <- 0.50         # Total R² at Level-1

# Level-2 variance partitioning (as proportions of total between variance)
# These should sum to R2_between - NOW ORTHOGONAL (no overlap)
r2_drug <- 0.05           # Drug effect (direct) - increased to maintain drug disadvantage
r2_b0i <- 0.25            # Random intercept residuals (u_b0i)
r2_b1i <- 0.20            # Random slope residuals (u_b1i) - decreased to reduce selective attrition
# Remaining: (1 - R2_between) would be random intercept u_0j, but set to 0 for dropout

# Level-1 variance partitioning (as proportions of total within variance)
# These should sum to R2_within
r2_Y <- 0.40              # Y (growth-curve centered: Y - (b0i + b1i*time))
r2_lagY <- 0.10           # lag(Y) (growth-curve centered: lagY - (b0i + b1i*(time-1)))
# Remaining: (1 - R2_within) = residual ε_ij (fixed at 1 in fitted model)

# Target CONDITIONAL DROPOUT HAZARDS by week
# These are P(dropout at time t | still in study at time t)
# Week 0 is always 0 (baseline has no dropout)

hazard_target <- c(`1` = 0.15, `2` = 0.1854, `3` = 0.1854, `4` = 0.1854)
# hazard_target <- c(`1` = 0.10, `2` = 0.15, `3` = 0.20, `4` = 0.25)

## ============================================================================
## DATA GENERATION PARAMETERS
## ============================================================================

N <- 500
N_prelim <- 100000  # For stable covariance estimates
seed <- 12345
weeks <- 0:4

# Group assignment probability
p_drug <- 0.60            # Probability of being in drug group (balanced 50/50 split)

# Growth model parameters
b0icept <- 6.4165
b1icept <- -0.6695
b1drug <- -0.7656
sigma_b <- matrix(c(0.3005, 0.075, 0.075, 0.1217), nrow = 2)
quad <- 0
quaddrug <- 0
resvar <- 0.701

## ============================================================================
## COMPUTE VARIANCE COMPONENTS FROM R² TARGETS
## ============================================================================

cat("\n")
cat(rep("=", 80), "\n", sep="")
cat("R²-BASED MNAR MISSINGNESS MODEL - DROPOUT WITH HAZARD CALIBRATION\n")
cat(rep("=", 80), "\n\n", sep="")

# Derived quantities
W <- 1 / (1 - R2_within)
B <- ICC_target * W / (1 - ICC_target)
sigma2_u0 <- (1 - R2_between) * B
sigma2_u0 <- 0  # OVERRIDE for dropout: no random intercept (no within-person variation in 1s)
sigma2_e <- 1.0

cat("Target Configuration:\n")
cat(sprintf("  ICC: %.3f  |  R²_between: %.3f  |  R²_within: %.3f\n",
            ICC_target, R2_between, R2_within))
cat(sprintf("  Between variance: %.4f  |  Within variance: %.4f\n", B, W))
cat(sprintf("  Drug group probability: %.2f\n", p_drug))
cat("  NOTE: sigma2_u0 = 0 for dropout (no random intercept)\n\n")

## ============================================================================
## STEP 1: COMPUTE LEVEL-2 COVARIANCE MATRIX (ANALYTICAL)
## ============================================================================

cat("Step 1: Computing Level-2 covariance matrix...\n")

# Variance of drug (0/1 indicator)
var_drug <- p_drug * (1 - p_drug)

# Variances of residuals
var_u_b0i <- sigma_b[1, 1]
var_u_b1i <- sigma_b[2, 2]
cov_u_b0i_u_b1i <- sigma_b[1, 2]

# For DROPOUT: Need covariance of d*predictors where d = 0 at t=0, d = 1 at t>0
p_d1 <- 4/5  # Proportion where d = 1
var_d_drug <- p_d1 * var_drug + p_d1 * (1 - p_d1) * p_drug^2
var_d_u_b0i <- p_d1 * var_u_b0i
var_d_u_b1i <- p_d1 * var_u_b1i
cov_d_drug_d_u_b0i <- 0  # Drug and random effects are independent
cov_d_drug_d_u_b1i <- 0
cov_d_u_b0i_d_u_b1i <- p_d1 * cov_u_b0i_u_b1i

Sigma_L2 <- matrix(c(
  var_d_drug,          cov_d_drug_d_u_b0i, cov_d_drug_d_u_b1i,
  cov_d_drug_d_u_b0i,  var_d_u_b0i,        cov_d_u_b0i_d_u_b1i,
  cov_d_drug_d_u_b1i,  cov_d_u_b0i_d_u_b1i, var_d_u_b1i
), nrow = 3, byrow = TRUE)

cat("  Done. Using RESIDUALS with DROPOUT switch (d*predictors).\n")
cat("  Correlation(u_b0i, u_b1i):", round(cov_u_b0i_u_b1i / sqrt(var_u_b0i * var_u_b1i), 3), "\n\n")

## ============================================================================
## STEP 2: GENERATE PRELIMINARY DATA FOR LEVEL-1 COVARIANCE
## ============================================================================

cat("Step 2: Generating preliminary data (N =", N_prelim, ")...\n")

set.seed(seed)
sim <- data.frame(
  id = 1:N_prelim,
  drug = rbinom(N_prelim, 1, p_drug)
)

b0i_hat <- b0icept
b1i_hat <- b1icept + b1drug * sim$drug
u <- rmvnorm(N_prelim, c(0, 0), sigma_b)
sim$b0i <- b0i_hat + u[, 1]
sim$b1i <- b1i_hat + u[, 2]

sim_long <- sim[rep(seq_len(nrow(sim)), each = length(weeks)), ]
sim_long$week <- rep(weeks, times = nrow(sim))
sim_long <- sim_long[order(sim_long$id, sim_long$week), ]

yhat <- sim_long$b0i + sim_long$week * sim_long$b1i +
  (sim_long$week^2) * quad + sim_long$drug * (sim_long$week^2) * quaddrug
sim_long$res <- rnorm(length(yhat), 0, sqrt(resvar))
sim_long$severity <- yhat + sim_long$res

# Center Y around individual growth curve: Y - (b0i + b1i * time)
sim_long$severity_predicted <- sim_long$b0i + sim_long$b1i * sim_long$week
sim_long$severity_centered <- sim_long$severity - sim_long$severity_predicted

# Center lagY around individual growth curve: lagY - (b0i + b1i * (time - 1))
sim_long$lag_severity <- ave(sim_long$severity, sim_long$id,
                             FUN = function(x) c(NA, x[-length(x)]))
sim_long$lag_severity[is.na(sim_long$lag_severity)] <- 0
sim_long$lag_severity_predicted <- sim_long$b0i + sim_long$b1i * (sim_long$week - 1)
sim_long$lag_severity_centered <- sim_long$lag_severity - sim_long$lag_severity_predicted

# Extract residuals for Level-2 covariance
sim$u_b0i <- sim$b0i - b0icept
sim$u_b1i <- sim$b1i - (b1icept + b1drug * sim$drug)
sim_long$u_b0i <- rep(sim$u_b0i, each = length(weeks))
sim_long$u_b1i <- rep(sim$u_b1i, each = length(weeks))

# Create switch dummy for dropout (0 at baseline, 1 thereafter)
sim_long$d <- ifelse(sim_long$week == 0, 0, 1)

# Level-1 predictors: d*Y and d*lag(Y) for dropout
X_L1_prelim <- cbind(
  d_Y = sim_long$d * sim_long$severity_centered,
  d_lagY = sim_long$d * sim_long$lag_severity_centered
)
Sigma_L1 <- cov(X_L1_prelim)

cat("  Done.\n\n")

## ============================================================================
## STEP 3: SOLVE FOR MISSINGNESS MODEL COEFFICIENTS
## ============================================================================

cat("Step 3: Solving for coefficients...\n")

# Level-2 coefficients (for d*predictors)
target_var_drug <- r2_drug * B
target_var_b0i <- r2_b0i * B
target_var_b1i <- r2_b1i * B
target_vars_L2 <- c(target_var_drug, target_var_b0i, target_var_b1i)

gamma_init <- sqrt(target_vars_L2 / diag(Sigma_L2))
var_check <- as.numeric(t(gamma_init) %*% Sigma_L2 %*% gamma_init)
scale_factor <- sqrt(sum(target_vars_L2) / var_check)
gamma_L2 <- gamma_init * scale_factor

# Level-1 coefficients (for d*Y and d*lag(Y))
target_vars_L1 <- c(
  r2_Y * W,
  r2_lagY * W
)

beta_init <- sqrt(target_vars_L1 / diag(Sigma_L1))
var_check <- as.numeric(t(beta_init) %*% Sigma_L1 %*% beta_init)
scale_factor <- sqrt(sum(target_vars_L1) / var_check)
beta_L1 <- beta_init * scale_factor

cat("  Level-2 coefficients: d*drug =", round(gamma_L2[1], 3),
    "| d*u_b0i =", round(gamma_L2[2], 3), "| d*u_b1i =", round(gamma_L2[3], 3), "\n")
cat("  Level-1 coefficients: d*Y =", round(beta_L1[1], 3),
    "| d*lag(Y) =", round(beta_L1[2], 3), "\n\n")

## ============================================================================
## STEP 4: GENERATE ACTUAL DATA
## ============================================================================

cat("Step 4: Generating actual data (N =", N, ")...\n")

set.seed(seed)
sim <- data.frame(
  id = 1:N,
  drug = rbinom(N, 1, p_drug)
)

b0i_hat <- b0icept
b1i_hat <- b1icept + b1drug * sim$drug
u <- rmvnorm(N, c(0, 0), sigma_b)
sim$b0i <- b0i_hat + u[, 1]
sim$b1i <- b1i_hat + u[, 2]

sim_long <- sim[rep(seq_len(nrow(sim)), each = length(weeks)), ]
sim_long$week <- rep(weeks, times = nrow(sim))
sim_long <- sim_long[order(sim_long$id, sim_long$week), ]

yhat <- sim_long$b0i + sim_long$week * sim_long$b1i +
  (sim_long$week^2) * quad + sim_long$drug * (sim_long$week^2) * quaddrug
sim_long$res <- rnorm(length(yhat), 0, sqrt(resvar))
sim_long$severity <- yhat + sim_long$res

# Center Y around individual growth curve: Y - (b0i + b1i * time)
sim_long$severity_predicted <- sim_long$b0i + sim_long$b1i * sim_long$week
sim_long$severity_centered <- sim_long$severity - sim_long$severity_predicted

# Center lagY around individual growth curve: lagY - (b0i + b1i * (time - 1))
sim_long$lag_severity <- ave(sim_long$severity, sim_long$id,
                             FUN = function(x) c(NA, x[-length(x)]))
sim_long$lag_severity[is.na(sim_long$lag_severity)] <- 0
sim_long$lag_severity_predicted <- sim_long$b0i + sim_long$b1i * (sim_long$week - 1)
sim_long$lag_severity_centered <- sim_long$lag_severity - sim_long$lag_severity_predicted

# Extract residuals for missingness model (orthogonal to drug)
sim$u_b0i <- sim$b0i - b0icept
sim$u_b1i <- sim$b1i - (b1icept + b1drug * sim$drug)
sim_long$u_b0i <- rep(sim$u_b0i, each = length(weeks))
sim_long$u_b1i <- rep(sim$u_b1i, each = length(weeks))

# Create switch dummy for dropout (0 at baseline, 1 thereafter)
sim_long$d <- ifelse(sim_long$week == 0, 0, 1)

long <- sim_long
cat("  Done.\n\n")

## ============================================================================
## STEP 5: GENERATE MISSINGNESS (DROPOUT WITH HAZARD CALIBRATION)
## ============================================================================

cat("Step 5: Generating missingness (dropout with hazard calibration)...\n")

# Use d*predictors (turns off all effects at baseline)
X_L2 <- cbind(
  d_drug = long$d * long$drug,
  d_u_b0i = long$d * long$u_b0i,
  d_u_b1i = long$d * long$u_b1i
)
X_L1 <- cbind(
  d_Y = long$d * long$severity_centered,
  d_lagY = long$d * long$lag_severity_centered
)

# Compute linear predictors (without intercept)
eta_L2 <- X_L2 %*% gamma_L2
# No u_0j for dropout - can't estimate with no within-person variation in 1s
eta_L1 <- X_L1 %*% beta_L1
e_ij <- rnorm(nrow(long), mean = 0, sd = sqrt(sigma2_e))

# Combine (without intercept yet)
eta_no_intercept <- eta_L2 + eta_L1 + e_ij

# Calibrate to CONDITIONAL HAZARDS (not cumulative rates)
cat("  Calibrating time-specific intercepts to target HAZARDS...\n")
alpha_vec <- c(`0` = -50)  # Baseline forced to 0% dropout
names(alpha_vec) <- c("0")

# Track dropout status
long$dropped <- FALSE
long <- long[order(long$id, long$week), ]

set.seed(seed)
for (wk in names(hazard_target)) {
  w <- as.integer(wk)
  
  # Only calibrate among those NOT YET DROPPED
  still_in_mask <- !long$dropped & long$week == w
  idx <- which(still_in_mask)
  
  if (length(idx) == 0) {
    warning(sprintf("Everyone dropped by week %s", wk))
    alpha_vec[wk] <- alpha_vec[as.character(w-1)]
    next
  }
  
  targ <- hazard_target[wk]
  eta_wk <- eta_no_intercept[idx]
  
  # Solve for intercept that gives mean hazard = target (among those still in)
  f <- function(a) mean(pnorm(a + eta_wk)) - targ
  
  tryCatch({
    alpha_vec[wk] <- uniroot(f, interval = c(-10, 10), extendInt = "yes")$root
  }, error = function(e) {
    alpha_vec[wk] <- qnorm(targ) - mean(eta_wk)
    warning(sprintf("Week %s: Using approximate intercept", wk))
  })
  
  # Determine who drops at this time (among those still in)
  p_drop <- pnorm(alpha_vec[wk] + eta_wk)
  drops_now <- runif(length(idx)) < p_drop
  
  # Mark as dropped and propagate forward for each id
  if (any(drops_now)) {
    ids_drop <- unique(long$id[idx[drops_now]])
    long$dropped[long$id %in% ids_drop & long$week >= w] <- TRUE
  }
}

# Set all weeks for dropped individuals to missing
long$miss_flag <- 0
for (i in unique(long$id)) {
  idx <- which(long$id == i)
  if (any(long$dropped[idx])) {
    # Find first dropout time
    first_drop <- min(long$week[idx][long$dropped[idx]])
    # Set ONLY the first dropout to 1
    long$miss_flag[idx[long$week[idx] == first_drop]] <- 1
    # Set all subsequent times to NA
    long$miss_flag[idx[long$week[idx] > first_drop]] <- NA
  }
}

long$missing <- long$miss_flag
long$severity_mnar <- long$severity
long$severity_mnar[long$missing == 1L | is.na(long$missing)] <- NA

# Calculate ACTUAL cumulative dropout rates correctly
# For each person, check if they've dropped out by each time
cumulative_dropout <- numeric(5)
names(cumulative_dropout) <- 0:4
for (w in 0:4) {
  # At each time, count people who have ANY missing data (dropped out)
  dropout_status <- tapply(long$miss_flag[long$week <= w],
                           long$id[long$week <= w],
                           function(x) any(x == 1 | is.na(x)))
  cumulative_dropout[as.character(w)] <- mean(dropout_status, na.rm = TRUE) * 100
}

overall_miss <- cumulative_dropout["4"]  # Final dropout rate

cat("\n  Cumulative Dropout Rates:\n")
cat("  ========================\n")
cumulative_target <- 0
for (wk in names(hazard_target)) {
  w <- as.integer(wk)
  hazard <- hazard_target[wk]
  cumulative_target <- cumulative_target + (1 - cumulative_target) * hazard
  cumulative_achieved <- cumulative_dropout[as.character(w)]

  cat(sprintf("  Time %d: Cumulative Target = %5.1f%%, Achieved = %5.1f%%\n",
              w, cumulative_target * 100, cumulative_achieved))
}
cat(sprintf("\n  Overall dropout rate: %.1f%%\n\n", overall_miss))

## ============================================================================
## STEP 6: DIAGNOSTICS
## ============================================================================

cat("Step 6: Validating variance decomposition...\n")

var_eta_L2 <- var(eta_L2)
var_eta_L1 <- var(eta_L1)
var_eij <- var(e_ij)

between_var <- var_eta_L2  # No random intercept for dropout
within_var <- var_eta_L1 + var_eij
total_var <- var(eta_no_intercept)

ICC_achieved <- between_var / total_var
R2_between_achieved <- var_eta_L2 / between_var
R2_within_achieved <- var_eta_L1 / within_var

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
var_drug_component <- var(long$d * long$drug * gamma_L2[1])
var_b0i_component <- var(long$d * long$u_b0i * gamma_L2[2])
var_b1i_component <- var(long$d * long$u_b1i * gamma_L2[3])

cat("Level-2 Variance Decomposition:\n")
cat(sprintf("  d*Drug contribution:  %.4f (%.1f%% of between_var)\n",
            var_drug_component, 100*var_drug_component/between_var))
cat(sprintf("  d*u_b0i contribution: %.4f (%.1f%% of between_var)\n",
            var_b0i_component, 100*var_b0i_component/between_var))
cat(sprintf("  d*u_b1i contribution: %.4f (%.1f%% of between_var)\n\n",
            var_b1i_component, 100*var_b1i_component/between_var))

# Probit models for MNAR signal (using residuals to match generation)
long_postbase <- long[long$week > 0, ]
fit_time <- glm(I(missing == 1) ~ factor(week),
                data = long_postbase, family = binomial(link = "probit"))
fit_full <- glm(I(missing == 1) ~ factor(week) + drug + u_b0i + u_b1i +
                  severity_centered + lag_severity_centered,
                data = long_postbase, family = binomial(link = "probit"))

LL_time <- as.numeric(logLik(fit_time))
LL_full <- as.numeric(logLik(fit_full))
delta_R2 <- 1 - LL_full/LL_time

cat("Step 7: MNAR signal strength (post-baseline observations only)\n")
cat(sprintf("  McFadden ΔR² (MNAR vs. time-only): %.4f\n\n", delta_R2))

## ============================================================================
## PLOTS
## ============================================================================

mean_by_week <- aggregate(severity ~ week + drug, data = long, FUN = mean)

ggplot(mean_by_week, aes(x = week, y = severity, color = factor(drug), group = drug)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("steelblue", "firebrick"),
                     labels = c("Control", "Drug"), name = "Group") +
  scale_y_continuous(limits = c(1, 7), breaks = 1:7) +
  labs(x = "Week", y = "Mean Severity",
       title = "Mean Severity by Week and Treatment Group (Dropout Pattern)") +
  theme_minimal(base_size = 14)

## ============================================================================
## SAVE OUTPUT
## ============================================================================

mnar <- long[, c('l2id', 'week', 'severity_mnar','severity', 'miss_flag', 'drug')]
names(mnar) <- c('l2id', 'time', 'y','ycom', 'm', 'group')

fdir::set()
save(mnar, file = 'growth-dropout.rds')
write.csv(mnar, file = 'growth-dropout.csv', row.names = FALSE)

cat(rep("=", 80), "\n", sep="")
cat("COMPLETE - DROPOUT PATTERN GENERATED WITH HAZARD CALIBRATION\n")
cat("Baseline missingness = 0%, hazards calibrated to targets\n")
cat(sprintf("Drug group: %.1f%%, Control group: %.1f%%\n",
            mean(long$drug) * 100, (1 - mean(long$drug)) * 100))
cat(rep("=", 80), "\n\n", sep="")
