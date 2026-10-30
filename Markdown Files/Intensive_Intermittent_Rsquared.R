## ============================================================================
## INTENSIVE INTERMITTENT WITH R²-BASED MNAR MISSINGNESS
## ============================================================================

library(mvtnorm)
library(ggplot2)

## ============================================================================
## MISSINGNESS MODEL R² TARGETS - EDIT THESE TO CHANGE MNAR STRENGTH
## ============================================================================
## NOTE: Level-2 missingness uses RESIDUALS (u_b0i, u_logvi) not totals
##       This gives orthogonal, non-overlapping R² contributions:
##       - r2_group: Direct group effect on missingness
##       - r2_b0i: Random intercept deviations (person-specific baseline y)
##       - r2_logvi: Log residual variance deviations (person-specific variability)
##
## INTERMITTENT: All occasions can have missingness (including day 0)
##               Time pattern controlled by miss_target (not included in R²)
## ============================================================================

ICC_target <- 0.35        # ICC for full model
R2_between <- 0.50        # Total R² at Level-2
R2_within <- 0.50         # Total R² at Level-1

# Level-2 variance partitioning (as proportions of total between variance)
# These should sum to R2_between - NOW ORTHOGONAL (no overlap)
r2_group <- 0.05          # Group effect (direct)
r2_b0i <- 0.20            # Random intercept residuals (u_b0i)
r2_logvi <- 0.25          # Log variance residuals (u_logvi)
# Remaining: (1 - R2_between) = random intercept u_0j

# Level-1 variance partitioning (as proportions of total within variance)
# These should sum to R2_within
# NOTE: Time pattern controlled by miss_target (not included in R² targets)
r2_y <- 0.40              # (y - alpha): deviation from person intercept b0i
r2_lagy <- 0.10           # (y.lag - alpha): lagged deviation from person intercept b0i
# Remaining: (1 - R2_within) = residual ε_ij (fixed at 1 in fitted model)

# Target marginal missingness rates by day (intermittent pattern)
# Replicates the growth model pattern across 20 days (4 days per growth model time point)
miss_target <- c(
  rep(0.10, 4),     # Days 0-3: 10%
  rep(0.0556, 4),   # Days 4-7: 5.56%
  rep(0.1854, 4),   # Days 8-11: 18.54%
  rep(0.1854, 4),   # Days 12-15: 18.54%
  rep(0.1854, 4)    # Days 16-19: 18.54%
)
names(miss_target) <- as.character(0:19)

## ============================================================================
## DATA GENERATION PARAMETERS
## ============================================================================

N <- 125                  # Number of clusters (participants)
N_prelim <- 50000         # For stable covariance estimates (especially for logvi)
seed <- 12345
days <- 0:19              # 20 observations per cluster
days_prelim <- 0:99       # 100 observations for preliminary data only

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
x_within_var <- 1.080
x_grand_mean <- 4.048

## ============================================================================
## COMPUTE VARIANCE COMPONENTS FROM R² TARGETS
## ============================================================================

cat("\n")
cat(rep("=", 80), "\n", sep="")
cat("R²-BASED MNAR MISSINGNESS MODEL - INTENSIVE INTERMITTENT\n")
cat(rep("=", 80), "\n\n", sep="")

# Derived quantities
W <- 1 / (1 - R2_within)
B <- ICC_target * W / (1 - ICC_target)
sigma2_u0 <- (1 - R2_between) * B
sigma2_e <- 1.0

cat("Target Configuration:\n")
cat(sprintf("  ICC: %.3f  |  R²_between: %.3f  |  R²_within: %.3f\n",
            ICC_target, R2_between, R2_within))
cat(sprintf("  Between variance: %.4f  |  Within variance: %.4f\n", B, W))
cat(sprintf("  Random intercept variance: %.4f\n\n", sigma2_u0))

## ============================================================================
## STEP 1: COMPUTE LEVEL-2 COVARIANCE MATRIX (ANALYTICAL)
## ============================================================================

cat("Step 1: Computing Level-2 covariance matrix...\n")

# Proportion in group = 1
p_group <- 0.36
var_group <- p_group * (1 - p_group)

# Variances of residuals from sigma_b0b1logvi
var_u_b0i <- sigma_b0b1logvi[1, 1]
var_u_logvi <- sigma_b0b1logvi[3, 3]

# Covariances (group independent of random effects)
cov_group_u_b0i <- 0
cov_group_u_logvi <- 0
cov_u_b0i_u_logvi <- sigma_b0b1logvi[1, 3]

Sigma_L2 <- matrix(c(
  var_group,         cov_group_u_b0i,   cov_group_u_logvi,
  cov_group_u_b0i,   var_u_b0i,         cov_u_b0i_u_logvi,
  cov_group_u_logvi, cov_u_b0i_u_logvi, var_u_logvi
), nrow = 3, byrow = TRUE)

cat("  Done. Using RESIDUALS (orthogonal to group). NOTE: u_b1i not used.\n")
cat("  Correlation(u_b0i, u_logvi):", round(cov_u_b0i_u_logvi / sqrt(var_u_b0i * var_u_logvi), 3), "\n\n")

## ============================================================================
## STEP 2: GENERATE PRELIMINARY DATA FOR LEVEL-1 COVARIANCE
## ============================================================================

cat("Step 2: Generating preliminary data (N =", N_prelim, ")...\n")

set.seed(seed)

# Joint distribution: male × group
prob_joint <- t(matrix(c(
  0.64 * 0.5, 0.36 * 0.5,  # male = 0
  0.64 * 0.5, 0.36 * 0.5), ncol = 2))  # male = 1
rownames(prob_joint) <- c(0, 1)
colnames(prob_joint) <- c(0, 1)

combos <- as.data.frame(as.table(prob_joint))
sim <- combos[sample(seq_len(nrow(combos)), size = N_prelim, replace = TRUE,
                     prob = combos$Freq), c("Var1", "Var2")]
names(sim) <- c("male", "group")
sim$male <- as.integer(as.character(sim$male))
sim$group <- as.integer(as.character(sim$group))

# Fixed effects
b0i_hat <- b0icept + b0group * sim$group
b1i_hat <- b1icept + b1group * sim$group

# Random effects
u <- rmvnorm(N_prelim, c(0, 0, 0), sigma_b0b1logvi)
sim$b0i <- b0i_hat + u[, 1]
sim$b1i <- b1i_hat + u[, 2]
sim$logvi <- -1.150 + u[, 3]
sim$id <- 1:N_prelim

# Expand to long format (use 100 obs per person for preliminary)
sim_long <- sim[rep(seq_len(nrow(sim)), each = length(days_prelim)), ]
sim_long$day <- rep(days_prelim, times = nrow(sim))
sim_long <- sim_long[order(sim_long$id, sim_long$day), ]

# Generate x with ICC = 0.35
x_mean_j <- rnorm(N_prelim, mean = x_grand_mean, sd = sqrt(x_between_var))
x_mean_ij <- rep(x_mean_j, each = length(days_prelim))
sim_long$x <- x_mean_ij + rnorm(nrow(sim_long), mean = 0, sd = sqrt(x_within_var))

# Separate x into within and between components
sim_long$x_pm <- ave(sim_long$x, sim_long$id, FUN = mean)
sim_long$x_wc <- sim_long$x - sim_long$x_pm

# Generate heterogeneous residual variance
sim_long$var_cluster <- exp(sim_long$logvi)

# Generate outcome (y)
yhat <- sim_long$b0i + sim_long$x_wc * sim_long$b1i
sim_long$res <- rnorm(nrow(sim_long), 0, sqrt(sim_long$var_cluster))
sim_long$y <- yhat + sim_long$res

# Create MNAR deviation scores to match Blimp: (y - alpha) and (y.lag - alpha)
# Here, alpha corresponds to the person-specific random intercept b0i
sim_long$y_dev_alpha <- sim_long$y - sim_long$b0i

# Lagged deviation (set baseline lag to 0)
sim_long$y_dev_alpha_lag <- ave(sim_long$y_dev_alpha, sim_long$id,
                                FUN = function(x) c(0, x[-length(x)]))

# Extract residuals for Level-2 covariance
sim$u_b0i <- sim$b0i - (b0icept + b0group * sim$group)
sim$u_b1i <- sim$b1i - (b1icept + b1group * sim$group)
sim$u_logvi <- sim$logvi - (-1.150)
sim_long$u_b0i <- rep(sim$u_b0i, each = length(days_prelim))
sim_long$u_b1i <- rep(sim$u_b1i, each = length(days_prelim))
sim_long$u_logvi <- rep(sim$u_logvi, each = length(days_prelim))

# Level-1 predictors (match Blimp): (y - alpha) and (y.lag - alpha)
pred_vars <- c("y_dev_alpha", "y_dev_alpha_lag")
X_L1_prelim <- as.matrix(sim_long[, pred_vars])
Sigma_L1 <- cov(X_L1_prelim)

cat("  Done.\n\n")

## ============================================================================
## STEP 3: SOLVE FOR MISSINGNESS MODEL COEFFICIENTS
## ============================================================================

cat("Step 3: Solving for coefficients...\n")

# Level-2 coefficients (u_b1i not used)
target_var_group <- r2_group * B
target_var_b0i <- r2_b0i * B
target_var_logvi <- r2_logvi * B
target_vars_L2 <- c(target_var_group, target_var_b0i, target_var_logvi)

gamma_init <- sqrt(target_vars_L2 / diag(Sigma_L2))
var_check <- as.numeric(t(gamma_init) %*% Sigma_L2 %*% gamma_init)
scale_factor <- sqrt(sum(target_vars_L2) / var_check)
gamma_L2 <- gamma_init * scale_factor

# Level-1 coefficients
target_vars_L1 <- c(
  r2_y * W,
  r2_lagy * W
)

beta_init <- sqrt(target_vars_L1 / diag(Sigma_L1))
var_check <- as.numeric(t(beta_init) %*% Sigma_L1 %*% beta_init)
scale_factor <- sqrt(sum(target_vars_L1) / var_check)
beta_L1 <- beta_init * scale_factor

cat("  Level-2 coefficients: group =", round(gamma_L2[1], 3),
    "| u_b0i =", round(gamma_L2[2], 3),
    "| u_logvi =", round(gamma_L2[3], 3), "\n")
cat("  Level-1 coefficients: res =", round(beta_L1[1], 3),
    "| lag_res =", round(beta_L1[2], 3), "\n\n")

## ============================================================================
## STEP 4: GENERATE ACTUAL DATA
## ============================================================================

cat("Step 4: Generating actual data (N =", N, ")...\n")

set.seed(seed)

# Joint distribution
sim <- combos[sample(seq_len(nrow(combos)), size = N, replace = TRUE,
                     prob = combos$Freq), c("Var1", "Var2")]
names(sim) <- c("male", "group")
sim$male <- as.integer(as.character(sim$male))
sim$group <- as.integer(as.character(sim$group))

# Fixed effects
b0i_hat <- b0icept + b0group * sim$group
b1i_hat <- b1icept + b1group * sim$group

# Random effects
u <- rmvnorm(N, c(0, 0, 0), sigma_b0b1logvi)
sim$b0i <- b0i_hat + u[, 1]
sim$b1i <- b1i_hat + u[, 2]
sim$logvi <- -1.150 + u[, 3]
sim$id <- 1:N

# Expand to long format
sim_long <- sim[rep(seq_len(nrow(sim)), each = length(days)), ]
sim_long$day <- rep(days, times = nrow(sim))
sim_long <- sim_long[order(sim_long$id, sim_long$day), ]

# Generate x
x_mean_j <- rnorm(N, mean = x_grand_mean, sd = sqrt(x_between_var))
x_mean_ij <- rep(x_mean_j, each = length(days))
sim_long$x <- x_mean_ij + rnorm(nrow(sim_long), mean = 0, sd = sqrt(x_within_var))

# Separate x into components
sim_long$x_pm <- ave(sim_long$x, sim_long$id, FUN = mean)
sim_long$x_wc <- sim_long$x - sim_long$x_pm

# Generate heterogeneous variance
sim_long$var_cluster <- exp(sim_long$logvi)

# Generate outcome
yhat <- sim_long$b0i + sim_long$x_wc * sim_long$b1i
sim_long$res <- rnorm(nrow(sim_long), 0, sqrt(sim_long$var_cluster))
sim_long$y <- yhat + sim_long$res

# Create MNAR deviation scores to match Blimp: (y - alpha) and (y.lag - alpha)
# Here, alpha corresponds to the person-specific random intercept b0i
sim_long$y_dev_alpha <- sim_long$y - sim_long$b0i

# Lagged deviation (set baseline lag to 0)
sim_long$y_dev_alpha_lag <- ave(sim_long$y_dev_alpha, sim_long$id,
                                FUN = function(x) c(0, x[-length(x)]))

# Extract residuals for missingness model
sim$u_b0i <- sim$b0i - (b0icept + b0group * sim$group)
sim$u_b1i <- sim$b1i - (b1icept + b1group * sim$group)
sim$u_logvi <- sim$logvi - (-1.150)
sim_long$u_b0i <- rep(sim$u_b0i, each = length(days))
sim_long$u_b1i <- rep(sim$u_b1i, each = length(days))
sim_long$u_logvi <- rep(sim$u_logvi, each = length(days))

long <- sim_long
cat("  Done.\n\n")

## ============================================================================
## STEP 5: GENERATE MISSINGNESS
## ============================================================================

cat("Step 5: Generating missingness...\n")

# Use RESIDUALS (orthogonal to group) for clean R² interpretation
# NOTE: u_b1i not used - only group, u_b0i, u_logvi
X_L2 <- cbind(group = long$group, u_b0i = long$u_b0i, u_logvi = long$u_logvi)
X_L1 <- as.matrix(long[, pred_vars])

# Compute linear predictors (without intercept)
eta_L2 <- X_L2 %*% gamma_L2
u_0j <- rnorm(N, mean = 0, sd = sqrt(sigma2_u0))
u_0j_long <- rep(u_0j, each = length(days))
eta_L1 <- X_L1 %*% beta_L1
e_ij <- rnorm(nrow(long), mean = 0, sd = sqrt(sigma2_e))

# Combine (without intercept yet)
eta_no_intercept <- eta_L2 + u_0j_long + eta_L1 + e_ij

# Calibrate day-specific intercepts to hit target missingness rates
cat("  Calibrating time-specific intercepts...\n")
alpha_vec <- numeric(length(miss_target))
names(alpha_vec) <- names(miss_target)

for (d_name in names(miss_target)) {
  d <- as.integer(d_name)
  idx <- which(long$day == d)
  targ <- miss_target[d_name]
  eta_d <- eta_no_intercept[idx]
  
  # Solve for intercept that gives mean(pnorm(alpha + eta)) = targ
  f <- function(a) mean(pnorm(a + eta_d)) - targ
  
  tryCatch({
    alpha_vec[d_name] <- uniroot(f, interval = c(-10, 10), extendInt = "yes")$root
  }, error = function(e) {
    alpha_vec[d_name] <- qnorm(targ) - mean(eta_d)
    warning(sprintf("Day %s: Using approximate intercept", d_name))
  })
}

# Compute missingness probabilities
alpha <- alpha_vec[as.character(long$day)]
eta <- alpha + eta_no_intercept
p_miss <- pnorm(eta)

# Simulate INTERMITTENT missingness (independent at each occasion)
set.seed(seed)
miss_flag <- as.integer(runif(nrow(long)) < p_miss)

long$missing <- miss_flag

# Apply missingness to y
long$y_mnar <- long$y
long$y_mnar[long$missing == 1L] <- NA

# Diagnostics
miss_emp <- with(long, tapply(missing == 1, day, mean, na.rm = TRUE)) * 100
overall_miss <- mean(is.na(long$y_mnar)) * 100

cat("\n  Target vs Achieved Missingness Rates by Day:\n")
cat("  ============================================\n")
for (d_name in names(miss_target)) {
  d <- as.integer(d_name)
  targ <- miss_target[d_name] * 100
  achieved <- miss_emp[d_name]
  cat(sprintf("  Day %2d: Target = %5.1f%%, Achieved = %5.1f%%\n", d, targ, achieved))
}
cat(sprintf("\n  Overall missingness: %.1f%%\n\n", overall_miss))

## ============================================================================
## STEP 6: DIAGNOSTICS
## ============================================================================

cat("Step 6: Validating variance decomposition...\n")

var_eta_L2 <- var(eta_L2)
var_u_0j_long <- var(u_0j_long)
var_eta_L1 <- var(eta_L1)
var_eij <- var(e_ij)

between_var <- var_eta_L2 + var_u_0j_long
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
var_group_component <- var(long$group * gamma_L2[1])
var_b0i_component <- var(long$u_b0i * gamma_L2[2])
var_logvi_component <- var(long$u_logvi * gamma_L2[3])

cat("Level-2 Variance Decomposition:\n")
cat(sprintf("  group contribution:   %.4f (%.1f%% of between_var)\n",
            var_group_component, 100*var_group_component/between_var))
cat(sprintf("  u_b0i contribution:   %.4f (%.1f%% of between_var)\n",
            var_b0i_component, 100*var_b0i_component/between_var))
cat(sprintf("  u_logvi contribution: %.4f (%.1f%% of between_var)\n",
            var_logvi_component, 100*var_logvi_component/between_var))
cat(sprintf("  u_0j contribution:    %.4f (%.1f%% of between_var)\n\n",
            var_u_0j_long, 100*var_u_0j_long/between_var))

# Probit models for MNAR signal (using deviation scores to match generation)
fit_time <- glm(I(missing == 1) ~ factor(day),
                data = long, family = binomial(link = "probit"))
fit_full <- glm(I(missing == 1) ~ factor(day) + group + u_b0i + u_logvi +
                  y_dev_alpha + y_dev_alpha_lag,
                data = long, family = binomial(link = "probit"))

LL_time <- as.numeric(logLik(fit_time))
LL_full <- as.numeric(logLik(fit_full))
delta_R2 <- 1 - LL_full/LL_time

cat("Step 7: MNAR signal strength\n")
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
       title = "Mean y by Day and Group (Intermittent Pattern)") +
  theme_minimal(base_size = 14)

## ============================================================================
## SAVE OUTPUT
## ============================================================================

mnar <- long[, c('l2id', 'day', 'day', 'y_mnar', 'y', 'x', 'x', 'missing','group')]
names(mnar) <- c('l2id', 'occasion', 'time', 'y', 'ycom', 'x', 'xcom', 'm','group')
mnar$x <- ifelse(is.na(mnar$y), NA, mnar$xcom)

fdir::set()
save(mnar, file = 'intensive-intermittent.rds')
write.csv(mnar, file = 'intensive-intermittent.csv', row.names = FALSE)

cat(rep("=", 80), "\n", sep="")
cat("COMPLETE - INTENSIVE INTERMITTENT PATTERN GENERATED\n")
cat(sprintf("All occasions can have missingness, N=%d, %d obs per cluster\n", N, length(days)))
cat(sprintf("Group 1: %.1f%%, Group 0: %.1f%%\n",
            mean(long$group) * 100, (1 - mean(long$group)) * 100))
cat(rep("=", 80), "\n\n", sep="")
