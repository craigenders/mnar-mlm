#------------------------------------------------------------------------------#
# LOAD R PACKAGES ----
#------------------------------------------------------------------------------#

options (scipen = 999)

# load packages
library(ggplot2)
library(patchwork)
library(rblimp)

#------------------------------------------------------------------------------#
# READ DATA ----
#------------------------------------------------------------------------------#

# github url for raw data
filepath <- 'https://raw.githubusercontent.com/craigenders/mnar-mlm/main/intensive-dropout.csv'

# create data frame from github data
intensive_dropout <- read.csv(filepath, stringsAsFactors = T)



# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')
source('https://raw.githubusercontent.com/craigenders/mnar-mlm/main/mnar-plotting.R')

#------------------------------------------------------------------------------#
# CMAR ----
#------------------------------------------------------------------------------#

# MODEL 1: CMAR ----
model1 <- rblimp(
  data = intensive_dropout,
  clusterid = 'l2id', 
  timeid = 'time',
  dropout = 'm = y (monotone)',
  latent = 'l2id = alpha beta omega',
  fixed = 'group time',
  center = 'groupmean = x',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    omega ~ intercept@g0o;
    alpha beta omega ~~ alpha beta omega;
    level1:
    y ~ intercept@alpha x@beta;
    var(y) ~ intercept@omega;',
  parameters = '
    diff = g1a; 
    d_diff = diff / sqrt(alpha.totalvar);',
  seed = 90291,
  chains = 4,
  burn = 25000,
  iter = 25000)

# print output
output(model1)

#------------------------------------------------------------------------------#
# TIME-RELATED CHANGES (INTENSIVE MEASUREMENTS) ----
#------------------------------------------------------------------------------#

# MISSINGNESS PROBS: Linear Time ----
time_linear <- rblimp(
  data = intensive_dropout,
  clusterid = 'l2id', 
  timeid = 'time',
  dropout = 'm = y (monotone)',
  latent = 'l2id = alpha beta omega',
  fixed = 'time group',
  center = 'groupmean = x',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    omega ~ intercept@g0o;
    alpha beta omega ~~ alpha beta omega;
    level1:
    y ~ intercept@alpha x@beta;
    var(y) ~ intercept@omega;
    missingness:
    d = ifelse(time < 7, 0, 1);
    m ~ intercept@-3 d group*d (time - 7)*d (time - 7)*group*d | intercept@0;',
  seed = 90291,
  chains = 4,
  burn = 25000,
  iter = 20000,
  nimps = 20)

# print output
output(time_linear)

# MISSINGNESS PROBS: Quadratic Time ----
time_quadratic <- rblimp(
  data = intensive_dropout,
  clusterid = 'l2id', 
  timeid = 'time',
  dropout = 'm = y (monotone)',
  latent = 'l2id = alpha beta omega',
  fixed = 'time group',
  center = 'groupmean = x',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    omega ~ intercept@g0o;
    alpha beta omega ~~ alpha beta omega;
    level1:
    y ~ intercept@alpha x@beta;
    var(y) ~ intercept@omega;
    missingness:
    d = ifelse(time < 7, 0, 1);
    m ~ intercept@-3 d group*d (time - 7)*d (time - 7)^2*d (time - 7)*group*d (time - 7)^2*group*d | intercept@0;',
  seed = 90291,
  chains = 4,
  burn = 25000,
  iter = 20000,
  nimps = 20)

# print output
output(time_quadratic)

# MISSINGNESS PROBS: Dummy-Coded Time ----
time_dummy <- rblimp(
  data = intensive_dropout,
  clusterid = 'l2id', 
  timeid = 'time',
  dropout = 'm = y (monotone)',
  latent = 'l2id = alpha beta omega',
  fixed = 'time group',
  center = 'groupmean = x',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    omega ~ intercept@g0o;
    alpha beta omega ~~ alpha beta omega;
    level1:
    y ~ intercept@alpha x@beta;
    var(y) ~ intercept@omega;
    missingness:
    d = ifelse(time < 7, 0, 1);
    m ~ intercept@-3 | intercept@0;
    { t in 1:19 } : m ~ (time == [t])*d (time == [t])*group*d;',
  seed = 90291,
  chains = 4,
  burn = 25000,
  iter = 25000)

# print output
output(time_dummy)

#------------------------------------------------------------------------------#
# PLOT MISSINGNESS PROBABILITIES (INTENSIVE) ----
#------------------------------------------------------------------------------#

ymax <- .15
ymin <- 0

# plot observed probabilities
plot_pmiss_obs <- plot_means(m ~ time | group, 
    model = time_dummy,
    ylab = "Probability",
    title = "Observed Probabilities",
    group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) +
    theme(legend.position = "top",legend.justification = "center") +
    scale_linetype_manual(values = c("dashed", "solid")) +
    geom_line(linewidth = .25)

# plot predicted probabilities from linear model
plot_pmiss_linear <- plot_means(m.1.probability ~ time | group, 
    model = time_linear,
    ylab = "Probability",
    title = "Linear Time",
    group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) +
    theme(legend.position = "top",legend.justification = "center") +
    scale_linetype_manual(values = c("dashed", "solid")) +
    geom_line(linewidth = .25)

# plot predicted probabilities from quadratic model
plot_pmiss_quadratic <- plot_means(m.1.probability ~ time | group, 
    model = time_quadratic,
    ylab = "Probability",
    title = "Quadratic Time",
    group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) +
    theme(legend.position = "top",legend.justification = "center") +
    scale_linetype_manual(values = c("dashed", "solid")) +
    geom_line(linewidth = .25)

# plot predicted probabilities from dummy-coded model
plot_pmiss_dummy <- plot_means(m.1.probability ~ time | group, 
    model = time_dummy,
    ylab = "Probability",
    title = "Dummy Coded Time",
    group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) +
    theme(legend.position = "top",legend.justification = "center") +
    scale_linetype_manual(values = c("dashed", "solid")) +
    geom_line(linewidth = .25)

# combine plots with patchwork
plot_pmiss <- (plot_pmiss_obs | plot_pmiss_linear) / (plot_pmiss_quadratic | plot_pmiss_dummy)
plot_pmiss

# compute average individual probabilities by time and group
pmiss_obs <- aggregate(m ~ time + group, data = time_dummy@average_imp, mean)
pmiss_linear <- aggregate(m.1.probability ~ time + group, data = time_linear@average_imp, mean)
pmiss_quadratic <- aggregate(m.1.probability ~ time + group, data = time_quadratic@average_imp, mean)
pmiss_dummy <- aggregate(m.1.probability ~ time + group, data = time_dummy@average_imp, mean)

# compute rmse of predicted vs. observed probabilities
rmse_pmiss_linear <- sqrt(mean((pmiss_linear$m.1.probability - pmiss_obs$m)^2))
rmse_pmiss_quadratic <- sqrt(mean((pmiss_quadratic$m.1.probability - pmiss_obs$m)^2))
rmse_pmiss_dummy <- sqrt(mean((pmiss_dummy$m.1.probability - pmiss_obs$m)^2))

# probability summary table
miss_fit <- data.frame(
  RMSE   = round(c(rmse_pmiss_linear, rmse_pmiss_quadratic, rmse_pmiss_dummy), 3),
  Min    = round(c(
    min(pmiss_linear$m.1.probability   - pmiss_obs$m),
    min(pmiss_quadratic$m.1.probability  - pmiss_obs$m),
    min(pmiss_dummy$m.1.probability   - pmiss_obs$m)), 3),
  Median = round(c(
    median(pmiss_linear$m.1.probability   - pmiss_obs$m),
    median(pmiss_quadratic$m.1.probability  - pmiss_obs$m),
    median(pmiss_dummy$m.1.probability   - pmiss_obs$m)), 3),
  Mean   = round(c(
    mean(pmiss_linear$m.1.probability   - pmiss_obs$m),
    mean(pmiss_quadratic$m.1.probability  - pmiss_obs$m),
    mean(pmiss_dummy$m.1.probability   - pmiss_obs$m)), 3),
  Max    = round(c(
    max(pmiss_linear$m.1.probability   - pmiss_obs$m),
    max(pmiss_quadratic$m.1.probability  - pmiss_obs$m),
    max(pmiss_dummy$m.1.probability   - pmiss_obs$m)), 3),
  row.names = c("Linear", "Quadratic", "Dummy")
)
miss_fit

#------------------------------------------------------------------------------#
# SHARED PARAMETER ----
#------------------------------------------------------------------------------#

# MODEL 2: Shared Parameter Model ----
model2 <- rblimp(
  data = intensive_dropout,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (monotone)',
  latent = 'l2id = alpha beta omega',
  fixed = 'group time',
  center = 'groupmean = x',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    omega ~ intercept@g0o;
    alpha beta omega ~~ alpha beta omega;
    level1:
    y ~ intercept@alpha x@beta;
    var(y) ~ intercept@omega;
    missingness:
    d = ifelse(time < 7, 0, 1);
    m ~ intercept@-3 alpha*d omega*d | intercept@0;
    { t in 1:19 } : m ~ (time == [t])*d (time == [t])*group*d;',
  parameters = '
    diff = g1a; 
    d_diff = diff / sqrt(alpha.totalvar);',
  seed = 90291,
  chains = 4,
  burn = 75000,
  iter = 75000)

# print output
output(model2)

# MODEL 3: Quadratic Shared Parameter Model ----
model3 <- rblimp(
  data = intensive_dropout,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (monotone)',
  latent = 'l2id = alpha beta omega',
  fixed = 'group time',
  center = 'groupmean = x',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    omega ~ intercept@g0o;
    alpha beta omega ~~ alpha beta omega;
    level1:
    y ~ intercept@alpha x@beta;
    var(y) ~ intercept@omega;
    missingness:
    d = ifelse(time < 7, 0, 1);
    m ~ intercept@-3 alpha*d omega*d alpha^2*d omega^2*d | intercept@0;
    { t in 1:19 } : m ~ (time == [t])*d (time == [t])*group*d;',
  parameters = '
    diff = g1a; 
    d_diff = diff / sqrt(alpha.totalvar);',
  seed = 90291,
  chains = 4,
  burn = 200000,
  iter = 200000)

# print output
output(model3)

# MODEL 4: Residual Shared Parameter Model ----
model4 <- rblimp(
  data = intensive_dropout,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (monotone)',
  latent = 'l2id = alpha beta omega',
  fixed = 'group time',
  center = 'groupmean = x',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    omega ~ intercept@g0o;
    alpha beta omega ~~ alpha beta omega;
    level1:
    y ~ intercept@alpha x@beta;
    var(y) ~ intercept@omega;
    missingness:
    d = ifelse(time < 7, 0, 1);
    alpha_res = alpha - (g0a + g1a*group);
    m ~ intercept@-3 d*alpha_res d*omega | intercept@0;
    { t in 1:19 } : m ~ d*(time == [t]) d*(time == [t])*group;',
  parameters = '
    diff = g1a; 
    d_diff = diff / sqrt(alpha.totalvar);',
  seed = 90291,
  chains = 4,
  burn = 25000,
  iter = 25000)

# print output
output(model4)

# MODEL 5: Shared Parameter Model With Covariate Latent Means ----
model5 <- rblimp(
  data = intensive_dropout,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (monotone)',
  latent = 'l2id = alpha beta omega',
  fixed = 'group time',
  center = 'groupmean = x',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    omega ~ intercept@g0o;
    alpha beta omega ~~ alpha beta omega;
    level1:
    y ~ intercept@alpha x@beta;
    var(y) ~ intercept@omega;
    missingness:
    d = ifelse(time < 7, 0, 1);
    m ~ intercept@-3 d*alpha d*omega d*x.mean | intercept@0;
    { t in 1:19 } : m ~ d*(time == [t]) d*(time == [t])*group;',
  parameters = '
    diff = g1a; 
    d_diff = diff / sqrt(alpha.totalvar);',
  seed = 90291,
  chains = 4,
  burn = 200000,
  iter = 200000)

# print output
output(model5)

#------------------------------------------------------------------------------#
# DIGGLE-KENWARD MODEL ----
#------------------------------------------------------------------------------#

# MODEL 6: Diggle-Kenward Selection Model ----
model6 <- rblimp(
  data = intensive_dropout,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (monotone)',
  latent = 'l2id = alpha beta omega',
  fixed = 'group time',
  center = 'groupmean = x',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    omega ~ intercept@g0o;
    alpha beta omega ~~ alpha beta omega;
    level1:
    y ~ intercept@alpha x@beta;
    var(y) ~ intercept@omega;
    missingness:
    d = ifelse(time < 7, 0, 1);
    m ~ intercept@-3 y*d y.lag*d | intercept@0;
    { t in 1:19 } : m ~ (time == [t])*d (time == [t])*group*d;',
  parameters = '
    diff = g1a; 
    d_diff = diff / sqrt(alpha.totalvar);',
  seed = 90291,
  chains = 4,
  burn = 25000,
  iter = 25000)

# print output
output(model6)

# MODEL 7: Quadratic Diggle-Kenward Model ----
model7 <- rblimp(
  data = intensive_dropout,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (monotone)',
  latent = 'l2id = alpha beta omega',
  fixed = 'group time',
  center = 'groupmean = x',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    omega ~ intercept@g0o;
    alpha beta omega ~~ alpha beta omega;
    level1:
    y ~ intercept@alpha x@beta;
    var(y) ~ intercept@omega;
    missingness:
    d = ifelse(time < 7, 0, 1);
    m ~ intercept@-3 y*d y^2*d y.lag*d | intercept@0;
    { t in 1:19 } : m ~ (time == [t])*d (time == [t])*group*d;',
  parameters = '
    diff = g1a; 
    d_diff = diff / sqrt(alpha.totalvar);',
  seed = 90291,
  chains = 4,
  burn = 200000,
  iter = 200000)

# print output
output(model7)

# MODEL 8: Residual Diggle-Kenward Model ----
model8 <- rblimp(
  data = intensive_dropout,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (monotone)',
  latent = 'l2id = alpha beta omega',
  fixed = 'group time',
  center = 'groupmean = x',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    omega ~ intercept@g0o;
    alpha beta omega ~~ alpha beta omega;
    level1:
    y ~ intercept@alpha x@beta;
    var(y) ~ intercept@omega;
    missingness:
    d = ifelse(time < 7, 0, 1);
    m ~ intercept@-3 (y - alpha)*d (y.lag - alpha)*d | intercept@0;
    { t in 1:19 } : m ~ (time == [t])*d (time == [t])*group*d;',
  parameters = '
    diff = g1a; 
    d_diff = diff / sqrt(alpha.totalvar);',
  seed = 90291,
  chains = 4,
  burn = 75000,
  iter = 75000)

# print output
output(model8)

# MODEL 9: Diggle-Kenward Model With Covariate and its Lag ----
model9 <- rblimp(
  data = intensive_dropout,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (monotone)',
  latent = 'l2id = alpha beta omega xmeans',
  fixed = 'group time',
  model = '
    level2:
    xmeans ~ intercept;
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    omega ~ intercept@g0o;
    alpha beta omega ~~ alpha beta omega;
    level1:
    x ~ intercept@xmeans;
    y ~ intercept@alpha (x - xmeans)@beta;
    var(y) ~ intercept@omega;
    missingness:
    d = ifelse(time < 7, 0, 1);
    m ~ intercept@-3 y*d y.lag*d x*d x.lag*d | intercept@0;
    { t in 1:19 } : m ~ (time == [t])*d (time == [t])*group*d;',
  parameters = '
    diff = g1a; 
    d_diff = diff / sqrt(alpha.totalvar);',
  seed = 90291,
  chains = 4,
  burn = 25000,
  iter = 25000)

# print output
output(model9)

#------------------------------------------------------------------------------#
# DISAGGREGATED MODEL ----
#------------------------------------------------------------------------------#

# MODEL 10: Disaggregated Model ----
model10 <- rblimp(
  data = intensive_dropout,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (monotone)',
  latent = 'l2id = alpha beta omega',
  fixed = 'group time',
  center = 'groupmean = x',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    omega ~ intercept@g0o;
    alpha beta omega ~~ alpha beta omega;
    level1:
    y ~ intercept@alpha x@beta;
    var(y) ~ intercept@omega;
    missingness:
    d = ifelse(time < 7, 0, 1);
    m ~ intercept@-3 (y - alpha)*d (y.lag - alpha)*d alpha*d omega*d | intercept@0;
    { t in 1:19 } : m ~ (time == [t])*d (time == [t])*group*d;',
  parameters = '
    diff = g1a; 
    d_diff = diff / sqrt(alpha.totalvar);',
  seed = 90291,
  chains = 4,
  burn = 200000,
  iter = 200000)

# print output
output(model10)

#------------------------------------------------------------------------------#
# EXTRACT ESTIMATES ----
#------------------------------------------------------------------------------#

# function to extract key estimates
extract_intensive_params <- function(object, method) {
  
  tab <- object@estimates
  
  rows <- c(
    "alpha ~ Intercept",
    "beta ~ Intercept",
    "alpha ~ group",
    "beta ~ group",
    "alpha residual variance",
    "beta residual variance",
    "Cor( alpha, beta )",
    "y Variance Q50%",
    "omega ~ Intercept",
    "omega residual variance",
    "Cor( alpha, omega )",
    "Cor( beta, omega )",
    "Parameter: diff",
    "Parameter: d_diff",
    "m R2: Coefficients"
  )
  
  res <- do.call(rbind, lapply(rows, function(r) {
    if (r %in% rownames(tab)) {
      round(tab[r, c("Estimate", "StdDev"), drop = FALSE], 2)
    } else {
      data.frame(Estimate = NA_real_, StdDev = NA_real_, row.names = r)
    }
  }))
  
  rownames(res) <- c(
    "Intercept (G = 0)",
    "Slope (G = 0)",
    "Intercept Diff.",
    "Slope Diff.",
    "Var(Intercept)",
    "Var(Slope)",
    "Cor(Intercept, Slope)",
    "Var(Residual)",
    "Intercept Log-Var",
    "Var(Log-Var)",
    "Cor(Intercept, Log-Var)",
    "Cor(Log-Var, Slope)",
    "Mean Diff.",
    "Std. Mean Diff.",
    "Pseudo-Rsq"
  )
  
  colnames(res) <- c(
    paste0("Est_", method),
    paste0("SD_", method)
  )
  
  res
}

# main summary table ----
table_summary <- cbind(
  extract_growth_params(model1, "MOD1"),
  extract_growth_params(model2, "MOD2"),
  extract_growth_params(model3, "MOD3"),
  extract_growth_params(model4, "MOD4"),
  extract_growth_params(model5, "MOD5"),
  extract_growth_params(model6, "MOD6"),
  extract_growth_params(model7, "MOD7"),
  extract_growth_params(model8, "MOD8"),
  extract_growth_params(model9, "MOD9"),
  extract_growth_params(model10, "MOD10")
)
table_summary

# mean difference table ----

# extract rows
mean_row   <- table_summary["Mean Diff.", ]
std_row    <- table_summary["Std. Mean Diff.", ]

# get method names
col_names <- colnames(table_summary)
methods <- unique(sub("^(Est|SD)_", "", col_names))

# build table
table_diff <- do.call(rbind, lapply(methods, function(m) {
  c(
    Mean_Diff      = mean_row[paste0("Est_", m)],
    SD_Mean_Diff   = mean_row[paste0("SD_", m)],
    Std_Mean_Diff  = std_row[paste0("Est_", m)],
    SD_Std_Mean    = std_row[paste0("SD_", m)]
  )
}))
rownames(table_diff) <- methods
table_diff <- as.data.frame(table_diff)
table_diff

# diagnostics table ----

# function to extract convergence diagnostics
extract_convergence <- function(object, method) {
  
  neff <- object@estimates[, "N_Eff"]
  neff <- neff[is.finite(neff)]
  
  psr_row <- as.numeric(object@psr[20, ])
  psr_row <- psr_row[is.finite(psr_row)]
  
  data.frame(
    Min_Neff = round(min(neff),    3),
    Max_Neff = round(max(neff),    3),
    Min_PSR  = round(min(psr_row), 3),
    Max_PSR  = round(max(psr_row), 3),
    row.names = method
  )
}

# build table
table_diag <- rbind(
  extract_convergence(model1, "MOD1"),
  extract_convergence(model2, "MOD2"),
  extract_convergence(model3, "MOD3"),
  extract_convergence(model4, "MOD4"),
  extract_convergence(model5, "MOD5"),
  extract_convergence(model6, "MOD6"),
  extract_convergence(model7, "MOD7"),
  extract_convergence(model8, "MOD8"),
  extract_convergence(model8, "MOD9"),
  extract_convergence(model10, "MOD10") 
)

# add number of iterations
table_diag$Iterations <- c(
  nrow(model1@iterations),
  nrow(model2@iterations),
  nrow(model3@iterations),
  nrow(model4@iterations),
  nrow(model5@iterations),
  nrow(model6@iterations),
  nrow(model7@iterations),
  nrow(model8@iterations),
  nrow(model9@iterations),
  nrow(model10@iterations)
)
table_diag


