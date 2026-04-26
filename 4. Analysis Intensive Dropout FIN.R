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
intensive_d <- read.csv(filepath, stringsAsFactors = T)

# rename hard coded indicator
names(intensive_d)[names(intensive_d) == "m"] <- "m_"

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')
source('https://raw.githubusercontent.com/craigenders/mnar-mlm/main/mnar-plotting.R')

#------------------------------------------------------------------------------#
# CMAR ----
#------------------------------------------------------------------------------#

# MODEL 1: CMAR ----
intensive_do_mar <- rblimp(
  data = intensive_d,
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
    m ~ intercept | 1@0;',
  parameters = '
    adiff = g1a; 
    d_adiff = adiff / sqrt(alpha.totalvar);
    bdiff = g1b; 
    d_bdiff = bdiff / sqrt(exp(g0o));',
  seed = 90291,
  chains = 4,
  burn = 25000,
  iter = 25000)

# print output
output(intensive_do_mar)

#------------------------------------------------------------------------------#
# TIME-RELATED CHANGES (INTENSIVE MEASUREMENTS) ----
#------------------------------------------------------------------------------#

# linear trend ----
intensive_do_timelin <- rblimp(
  data = intensive_d,
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
output(intensive_do_timelin)

# quadratic trend ----
intensive_do_timequad <- rblimp(
  data = intensive_d,
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
output(intensive_do_timequad)

# dummy-coded time ----
intensive_do_timedum <- rblimp(
  data = intensive_d,
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
output(intensive_do_timedum)

#------------------------------------------------------------------------------#
# PLOT MISSINGNESS PROBABILITIES (INTENSIVE) ----
#------------------------------------------------------------------------------#

ymax <- .15
ymin <- 0

# plot observed and predicted probabilities
pintensive_do_obs <- plot_means(m ~ time | group, 
    model = intensive_do_timedum,
    ylab = "Probability",
    title = "Observed Probabilities",
    group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) +
    theme(legend.position = "top",legend.justification = "center") +
    scale_linetype_manual(values = c("dashed", "solid")) +
    geom_line(linewidth = .25)

pintensive_do_dum <- plot_means(m.1.probability ~ time | group, 
    model = intensive_do_timedum,
    ylab = "Probability",
    title = "Dummy Coded Time",
    group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) +
    theme(legend.position = "top",legend.justification = "center") +
    scale_linetype_manual(values = c("dashed", "solid")) +
    geom_line(linewidth = .25)

pintensive_do_lin <- plot_means(m.1.probability ~ time | group, 
    model = intensive_do_timelin,
    ylab = "Probability",
    title = "Linear Time",
    group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) +
    theme(legend.position = "top",legend.justification = "center") +
    scale_linetype_manual(values = c("dashed", "solid")) +
    geom_line(linewidth = .25)

pintensive_do_quad <- plot_means(m.1.probability ~ time | group, 
    model = intensive_do_timequad,
    ylab = "Probability",
    title = "Quadratic Time",
    group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) +
    theme(legend.position = "top",legend.justification = "center") +
    scale_linetype_manual(values = c("dashed", "solid")) +
    geom_line(linewidth = .25)

pintensive_do_combined <- (pintensive_do_obs | pintensive_do_lin) / (pintensive_do_quad | pintensive_do_dum)
pintensive_do_combined

# compute marginal probabilities (average individual probabilities) by time and group
pmiss_intensive_do_obs <- aggregate(m ~ time + group, data = intensive_do_timedum@average_imp, mean)
pmiss_intensive_do_timedum <- aggregate(m.1.probability ~ time + group, data = intensive_do_timedum@average_imp, mean)
pmiss_intensive_do_timelin <- aggregate(m.1.probability ~ time + group, data = intensive_do_timelin@average_imp, mean)
pmiss_intensive_do_timequad <- aggregate(m.1.probability ~ time + group, data = intensive_do_timequad@average_imp, mean)

# compute rmse of marginal vs. observed probabilities
rmse_intensive_do_timedum <- sqrt(mean((pmiss_intensive_do_timedum$m.1.probability - pmiss_intensive_do_obs$m)^2))
rmse_intensive_do_timelin <- sqrt(mean((pmiss_intensive_do_timelin$m.1.probability - pmiss_intensive_do_obs$m)^2))
rmse_intensive_do_timequad <- sqrt(mean((pmiss_intensive_do_timequad$m.1.probability - pmiss_intensive_do_obs$m)^2))

miss_fit <- data.frame(
  RMSE   = round(c(rmse_intensive_do_timelin, rmse_intensive_do_timequad, rmse_intensive_do_timedum), 3),
  Min    = round(c(
    min(pmiss_intensive_do_timelin$m.1.probability   - pmiss_intensive_do_obs$m),
    min(pmiss_intensive_do_timequad$m.1.probability  - pmiss_intensive_do_obs$m),
    min(pmiss_intensive_do_timedum$m.1.probability   - pmiss_intensive_do_obs$m)), 3),
  Median = round(c(
    median(pmiss_intensive_do_timelin$m.1.probability   - pmiss_intensive_do_obs$m),
    median(pmiss_intensive_do_timequad$m.1.probability  - pmiss_intensive_do_obs$m),
    median(pmiss_intensive_do_timedum$m.1.probability   - pmiss_intensive_do_obs$m)), 3),
  Mean   = round(c(
    mean(pmiss_intensive_do_timelin$m.1.probability   - pmiss_intensive_do_obs$m),
    mean(pmiss_intensive_do_timequad$m.1.probability  - pmiss_intensive_do_obs$m),
    mean(pmiss_intensive_do_timedum$m.1.probability   - pmiss_intensive_do_obs$m)), 3),
  Max    = round(c(
    max(pmiss_intensive_do_timelin$m.1.probability   - pmiss_intensive_do_obs$m),
    max(pmiss_intensive_do_timequad$m.1.probability  - pmiss_intensive_do_obs$m),
    max(pmiss_intensive_do_timedum$m.1.probability   - pmiss_intensive_do_obs$m)), 3),
  row.names = c("Linear", "Quadratic", "Dummy")
)

miss_fit

#------------------------------------------------------------------------------#
# SHARED PARAMETER ----
#------------------------------------------------------------------------------#

# MODEL 2: Shared Parameter Model ----
intensive_do_sp <- rblimp(
  data = intensive_d,
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
    adiff = g1a; 
    d_adiff = adiff / sqrt(alpha.totalvar);
    bdiff = g1b; 
    d_bdiff = bdiff / sqrt(exp(g0o));',
  seed = 90291,
  chains = 4,
  burn = 75000,
  iter = 75000)

# print output
output(intensive_do_sp)

# MODEL 3: Quadratic Shared Parameter Model ----
intensive_do_spq <- rblimp(
  data = intensive_d,
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
    adiff = g1a; 
    d_adiff = adiff / sqrt(alpha.totalvar);
    bdiff = g1b; 
    d_bdiff = bdiff / sqrt(exp(g0o));',
  seed = 90291,
  chains = 4,
  burn = 200000,
  iter = 200000)

# print output
output(intensive_do_spq)

# MODEL 4: Residual Shared Parameter Model ----
intensive_do_spr <- rblimp(
  data = intensive_d,
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
    adiff = g1a; 
    d_adiff = adiff / sqrt(alpha.totalvar);
    bdiff = g1b; 
    d_bdiff = bdiff / sqrt(exp(g0o));',
  seed = 90291,
  chains = 4,
  burn = 25000,
  iter = 25000)

# print output
output(intensive_do_spr)

# MODEL 5: Shared Parameter Model With X Latent Means ----
intensive_do_spx <- rblimp(
  data = intensive_d,
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
    adiff = g1a; 
    d_adiff = adiff / sqrt(alpha.totalvar);
    bdiff = g1b; 
    d_bdiff = bdiff / sqrt(exp(g0o));',
  seed = 90291,
  chains = 4,
  burn = 200000,
  iter = 200000)

# print output
output(intensive_do_spx)

#------------------------------------------------------------------------------#
# DIGGLE-KENWARD MODEL ----
#------------------------------------------------------------------------------#

# MODEL 6: Diggle-Kenward Model ----
intensive_do_dk <- rblimp(
  data = intensive_d,
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
    adiff = g1a; 
    d_adiff = adiff / sqrt(alpha.totalvar);
    bdiff = g1b; 
    d_bdiff = bdiff / sqrt(exp(g0o));',
  seed = 90291,
  chains = 4,
  burn = 25000,
  iter = 25000)

# print output
output(intensive_do_dk)

# MODEL 7: Quadratic Diggle-Kenward Model ----
intensive_do_dkq <- rblimp(
  data = intensive_d,
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
    adiff = g1a; 
    d_adiff = adiff / sqrt(alpha.totalvar);
    bdiff = g1b; 
    d_bdiff = bdiff / sqrt(exp(g0o));',
  seed = 90291,
  chains = 4,
  burn = 200000,
  iter = 200000)

# print output
output(intensive_do_dkq)

# MODEL 8: Residual Diggle-Kenward Model ----
intensive_do_dkr <- rblimp(
  data = intensive_d,
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
    adiff = g1a; 
    d_adiff = adiff / sqrt(alpha.totalvar);
    bdiff = g1b; 
    d_bdiff = bdiff / sqrt(exp(g0o));',
  seed = 90291,
  chains = 4,
  burn = 75000,
  iter = 75000)

# print output
output(intensive_do_dkr)

# MODEL 9: Diggle-Kenward Model With X and Lag(X) ----
intensive_do_dkx <- rblimp(
  data = intensive_d,
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
    adiff = g1a; 
    d_adiff = adiff / sqrt(alpha.totalvar);
    bdiff = g1b; 
    d_bdiff = bdiff / sqrt(exp(g0o));',
  seed = 90291,
  chains = 4,
  burn = 25000,
  iter = 25000)

# print output
output(intensive_do_dkx)

# intensive_do_dkx@estimates

#------------------------------------------------------------------------------#
# DISAGGREGATED MODEL ----
#------------------------------------------------------------------------------#

# MODEL 10: Disaggregated Model ----
intensive_do_dis <- rblimp(
  data = intensive_d,
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
    adiff = g1a; 
    d_adiff = adiff / sqrt(alpha.totalvar);
    bdiff = g1b; 
    d_bdiff = bdiff / sqrt(exp(g0o));',
  seed = 90291,
  chains = 4,
  burn = 200000,
  iter = 200000)

# print output
output(intensive_do_dis)

#------------------------------------------------------------------------------#
# EXTRACT ESTIMATES ----
#------------------------------------------------------------------------------#

extract_int_params <- function(object, method) {
  
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
    "Parameter: adiff",
    "Parameter: d_adiff",
    "Parameter: bdiff",
    "Parameter: d_bdiff",
    "m R2: Coefficients"
  )
  
  res <- round(tab[rows, c("Estimate", "StdDev"), drop = FALSE],2)
  
  # Rename rows to cleaner presentation labels
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
    "Slope Diff.",
    "Std. Slope Diff.",
    "Pseudo-Rsq"
  )
  
  colnames(res) <- c(
    paste0("Est_", method),
    paste0("SD_", method)
  )
  
  res
}

# main summary table ----

mar_tab <- extract_int_params(intensive_do_mar, "MAR")
sp_tab <- extract_int_params(intensive_do_sp, "SP")
spq_tab <- extract_int_params(intensive_do_spq, "SPQ")
spr_tab <- extract_int_params(intensive_do_spr, "SPR")
spx_tab <- extract_int_params(intensive_do_spx, "SPX")
dk_tab <- extract_int_params(intensive_do_dk, "DK")
dkq_tab <- extract_int_params(intensive_do_dkq, "DKQ")
dkr_tab <- extract_int_params(intensive_do_dkr, "DKR")
dkx_tab <- extract_int_params(intensive_do_dkx, "DKX")
dis_tab <- extract_int_params(intensive_do_dis, "DIS")

tab <- cbind(mar_tab,sp_tab,spq_tab,spr_tab,spx_tab,dk_tab,dkq_tab,dkr_tab,dkx_tab,dis_tab)
tab_intensive_do <- tab
tab_intensive_do

# mean difference table ----

# Extract rows
tab <- tab_intensive_do 
mean_row   <- tab["Mean Diff.", ]
std_row    <- tab["Std. Mean Diff.", ]
# mean_row   <- tab["Slope Diff.", ]
# std_row    <- tab["Std. Slope Diff.", ]
pseudo_row <- tab["Pseudo-Rsq", ]

# Get method names
col_names <- colnames(tab)
methods <- unique(sub("^(Est|SD)_", "", col_names))

# Build output table
out <- do.call(rbind, lapply(methods, function(m) {
  c(
    Mean_Diff      = mean_row[paste0("Est_", m)],
    SD_Mean_Diff   = mean_row[paste0("SD_", m)],
    Std_Mean_Diff  = std_row[paste0("Est_", m)],
    SD_Std_Mean    = std_row[paste0("SD_", m)],
    Pseudo_Rsq     = pseudo_row[paste0("Est_", m)]
  )
}))

# Final formatting
rownames(out) <- methods
out <- as.data.frame(out)

iter_counts <- c(
  MAR = nrow(intensive_i_mar@iterations),
  WC  = nrow(intensive_i_wc@iterations),
  WCQ = nrow(intensive_i_wcq@iterations),
  WCR = nrow(intensive_i_wcr@iterations),
  DK  = nrow(intensive_i_dk@iterations),
  DKQ = nrow(intensive_i_dkq@iterations),
  DKR = nrow(intensive_i_dkr@iterations),
  DKX = nrow(intensive_i_dkx@iterations),
  DIS = nrow(intensive_i_dis@iterations)
)

out <- data.frame(
  Mean_Diff     = out[, 1],
  SD            = out[, 2],
  gap1          = NA,
  Std_Mean_Diff = out[, 3],
  SD2           = out[, 4],
  gap2          = NA,
  Pseudo_R2     = out[, 5],
  Iterations    = iter_counts[rownames(out)],
  row.names     = rownames(out)
)

es_intensive_do <- out
es_intensive_do

# diagnostics table ----

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

conv_intensive_do <- rbind(
  extract_convergence(intensive_do_mar, "MAR"),
  extract_convergence(intensive_do_sp, "SP"),
  extract_convergence(intensive_do_spq, "SPQ"),
  extract_convergence(intensive_do_spr, "SPR"),
  extract_convergence(intensive_do_spx, "SPX"),
  extract_convergence(intensive_do_dk, "DK"),
  extract_convergence(intensive_do_dkq, "DKQ"),
  extract_convergence(intensive_do_dkr, "DKR"),
  extract_convergence(intensive_do_dkx, "DKX"),
  extract_convergence(intensive_do_dis, "DIS")
)

conv_intensive_do

