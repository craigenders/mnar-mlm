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
filepath <- 'https://raw.githubusercontent.com/craigenders/mnar-mlm/main/intensive-intermittent.csv'

# create data frame from github data
intensive_i <- read.csv(filepath, stringsAsFactors = T)

# rename hard coded indicator
names(intensive_i)[names(intensive_i) == "m"] <- "m_"

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')
source('https://raw.githubusercontent.com/craigenders/mnar-mlm/main/mnar-plotting.R')

#------------------------------------------------------------------------------#
# CMAR ----
#------------------------------------------------------------------------------#

# MODEL 1: CMAR ----
intensive_im_mar <- rblimp(
  data = intensive_i,
  clusterid = 'l2id', 
  timeid = 'time',
  dropout = 'm = y (missing)',
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
    m ~ intercept;',
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
output(intensive_im_mar)

#------------------------------------------------------------------------------#
# ICC FOR THE MISSINGNESS INDICATOR ----
#------------------------------------------------------------------------------#

# fit unconditional model
icc_intensive_i <- rblimp(
  data = intensive_i,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (missing)',
  model = 'm ~ intercept | intercept;',
  seed = 90291,
  chains = 4,
  burn = 25000,
  iter = 25000)

# print output
output(icc_intensive_i)

#------------------------------------------------------------------------------#
# TIME-RELATED CHANGES ----
#------------------------------------------------------------------------------#

# linear trend ----
intensive_im_timelin <- rblimp(
  data = intensive_i,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (missing)',
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
    m ~ intercept time group time*group | intercept;',
  seed = 90291,
  chains = 4,
  burn = 25000,
  iter = 25000)

# print output
output(intensive_im_timelin)

# quadratic trend ----
intensive_im_timequad <- rblimp(
  data = intensive_i,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (missing)',
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
    m ~ intercept time time^2 group time*group time^2*group | intercept;',
  seed = 90291,
  chains = 4,
  burn = 25000,
  iter = 25000)

# print output
output(intensive_im_timequad)

# dummy-coded time ----
intensive_im_timedum <- rblimp(
  data = intensive_i,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (missing)',
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
    m ~ intercept group | intercept;
    { t in 1:19 } : m ~ (time == [t]) (time == [t])*group;',
  waldtest = '
    m ~ intercept group | intercept;
    { t in 1:19 } : m ~ (time == [t]);',
  seed = 90291,
  chains = 4,
  burn = 25000,
  iter = 25000)

# print output
output(intensive_im_timedum)

#------------------------------------------------------------------------------#
# PLOT MISSINGNESS PROBABILITIES (INTENSIVE) ----
#------------------------------------------------------------------------------#

ymax <- .35
ymin <- 0

# plot observed and predicted probabilities
pintensive_im_obs <- plot_means(m ~ time | group, 
    model = intensive_im_timedum,
    ylab = "Probability",
    title = "Observed Probabilities",
    group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) + xlim(0,21) +
    theme(legend.position = "top",legend.justification = "center") +
    scale_linetype_manual(values = c("dashed", "solid")) +
    geom_line(linewidth = .25)

pintensive_im_dum <- plot_means(m.1.probability ~ time | group, 
    model = intensive_im_timedum,
    ylab = "Probability",
    title = "Dummy Coded Time",
    group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) + xlim(0,21) +
    theme(legend.position = "top",legend.justification = "center") +
    scale_linetype_manual(values = c("dashed", "solid")) +
    geom_line(linewidth = .25)

pintensive_im_lin <- plot_means(m.1.probability ~ time | group, 
    model = intensive_im_timelin,
    ylab = "Probability",
    title = "Linear Time",
    group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) + xlim(0,21) +
    theme(legend.position = "top",legend.justification = "center") +
    scale_linetype_manual(values = c("dashed", "solid")) +
    geom_line(linewidth = .25)

pintensive_im_quad <- plot_means(m.1.probability ~ time | group, 
    model = intensive_im_timequad,
    ylab = "Probability",
    title = "Quadratic Time",
    group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) + xlim(0,21) +
    theme(legend.position = "top",legend.justification = "center") +
    scale_linetype_manual(values = c("dashed", "solid")) +
    geom_line(linewidth = .25)

pintensive_im_obs
pintensive_im_dum
pintensive_im_lin
pintensive_im_quad

# compute marginal probabilities (average individual probabilities) by time and group
pmiss_intensive_im_obs <- aggregate(m ~ time + group, data = intensive_im_timedum@average_imp, mean)
pmiss_intensive_im_timelin <- aggregate(m.1.probability ~ time + group, data = intensive_im_timelin@average_imp, mean)
pmiss_intensive_im_timequad <- aggregate(m.1.probability ~ time + group, data = intensive_im_timequad@average_imp, mean)
pmiss_intensive_im_timedum <- aggregate(m.1.probability ~ time + group, data = intensive_im_timedum@average_imp, mean)

# compute rmse of marginal vs. observed probabilities
rmse_intensive_im_timelin <- sqrt(mean((pmiss_intensive_im_timelin$m.1.probability - pmiss_intensive_im_obs$m)^2))
rmse_intensive_im_timequad <- sqrt(mean((pmiss_intensive_im_timequad$m.1.probability - pmiss_intensive_im_obs$m)^2))
rmse_intensive_im_timedum <- sqrt(mean((pmiss_intensive_im_timedum$m.1.probability - pmiss_intensive_im_obs$m)^2))

rmse_intensive_im_timelin
rmse_intensive_im_timequad
rmse_intensive_im_timedum

# summarize difference between marginal vs. observed probabilities
summary(pmiss_intensive_im_timelin$m.1.probability - pmiss_intensive_im_obs$m)
summary(pmiss_intensive_im_timequad$m.1.probability - pmiss_intensive_im_obs$m)
summary(pmiss_intensive_im_timedum$m.1.probability - pmiss_intensive_im_obs$m)

#------------------------------------------------------------------------------#
# SHARED PARAMETER ----
#------------------------------------------------------------------------------#

# MODEL 2: Shared Parameter Model ----
intensive_im_sp <- rblimp(
  data = intensive_i,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (missing)',
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
    m ~ intercept group alpha omega | intercept;
    { t in 1:19 } : m ~ (time == [t]) (time == [t])*group;',
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
output(intensive_im_sp)

# MODEL 3: Quadratic Shared Parameter Model ----
intensive_im_spq <- rblimp(
  data = intensive_i,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (missing)',
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
    m ~ intercept group alpha alpha^2 omega omega^2 | intercept;
    { t in 1:19 } : m ~ (time == [t]) (time == [t])*group;',
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
output(intensive_im_spq)

# MODEL 4: Residual Shared Parameter Model ----
intensive_im_spr <- rblimp(
  data = intensive_i,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (missing)',
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
    alpha_res = alpha - (g0a + g1a*group);
    m ~ intercept group alpha_res omega | intercept;
    { t in 1:19 } : m ~ (time == [t]) (time == [t])*group;',
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
output(intensive_im_spr)

# MODEL 5: Shared Parameter Model With X Latent Means ----
intensive_im_spx <- rblimp(
  data = intensive_i,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (missing)',
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
    m ~ intercept group alpha omega x.mean | intercept;
    { t in 1:19 } : m ~ (time == [t]) (time == [t])*group;',
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
output(intensive_im_spx)

#------------------------------------------------------------------------------#
# DIGGLE-KENWARD MODEL ----
#------------------------------------------------------------------------------#

# MODEL 6: Diggle-Kenward Model ----
intensive_im_dk <- rblimp(
  data = intensive_i,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (missing)',
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
    m ~ intercept group y y.lag | intercept;
    { t in 1:19} : m ~ (time == [t]) (time == [t])*group;',
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
output(intensive_im_dk)

# MODEL 7: Quadratic Diggle-Kenward Model ----
intensive_im_dkq <- rblimp(
  data = intensive_i,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (missing)',
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
    m ~ intercept group y y^2 y.lag | intercept;
    { t in 1:19} : m ~ (time == [t]) (time == [t])*group;',
  parameters = '
    adiff = g1a; 
    d_adiff = adiff / sqrt(alpha.totalvar);
    bdiff = g1b; 
    d_bdiff = bdiff / sqrt(exp(g0o));',
  seed = 90291,
  chains = 4,
  burn = 50000,
  iter = 50000)

# print output
output(intensive_im_dkq)

# MODEL 8: Residual Diggle-Kenward Model ----
intensive_im_dkr <- rblimp(
  data = intensive_i,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (missing)',
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
    m ~ intercept group (y - alpha) (y.lag - alpha) | intercept;
    { t in 1:19 } : m ~ (time == [t]) (time == [t])*group;',
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
output(intensive_im_dkr)

# MODEL 9: Diggle-Kenward Model With X and Lag(X) ----
intensive_im_dkx <- rblimp(
  data = intensive_i,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (missing)',
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
    m ~ intercept group y y.lag x x.lag | intercept;
    { t in 1:19} : m ~ (time == [t]) (time == [t])*group;',
  parameters = '
    adiff = g1a; 
    d_adiff = adiff / sqrt(alpha.totalvar);
    bdiff = g1b; 
    d_bdiff = bdiff / sqrt(exp(g0o));',
  seed = 90291,
  chains = 4,
  burn = 50000,
  iter = 50000)

# print output
output(intensive_im_dkx)

#------------------------------------------------------------------------------#
# DISAGGREGATED MODEL ----
#------------------------------------------------------------------------------#

# MODEL 10: Disaggregated Model ----
intensive_im_dis <- rblimp(
  data = intensive_i,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (missing)',
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
    m ~ intercept group (y - alpha) (y.lag - alpha) alpha omega | intercept;
    { t in 1:19 } : m ~ (time == [t]) (time == [t])*group;',
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
output(intensive_im_dis)

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

mar_tab <- extract_int_params(intensive_im_mar, "MAR")
sp_tab <- extract_int_params(intensive_im_sp, "SP")
spq_tab <- extract_int_params(intensive_im_spq, "SPQ")
spr_tab <- extract_int_params(intensive_im_spr, "SPR")
spx_tab <- extract_int_params(intensive_im_spx, "SPX")
dk_tab <- extract_int_params(intensive_im_dk, "DK")
dkq_tab <- extract_int_params(intensive_im_dkq, "DKQ")
dkr_tab <- extract_int_params(intensive_im_dkr, "DKR")
dkx_tab <- extract_int_params(intensive_im_dkx, "DKX")
dis_tab <- extract_int_params(intensive_im_dis, "DIS")

tab <- cbind(mar_tab,sp_tab,spq_tab,spr_tab,spx_tab,dk_tab,dkq_tab,dkr_tab,dkx_tab,dis_tab)
tab_intensive_im <- tab
tab_intensive_im

# mean difference table ----

# Extract rows
tab <- tab_intensive_im
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
  MAR = nrow(intensive_im_mar@iterations),
  SP  = nrow(intensive_im_sp@iterations),
  SPQ = nrow(intensive_im_spq@iterations),
  SPR = nrow(intensive_im_spr@iterations),
  DK  = nrow(intensive_im_dk@iterations),
  DKQ = nrow(intensive_im_dkq@iterations),
  DKD = nrow(intensive_im_dkd@iterations),
  DIS = nrow(intensive_im_dis@iterations)
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

es_intensive_im <- out
es_intensive_im

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

conv_intensive_im <- rbind(
  extract_convergence(intensive_im_mar, "MAR"),
  extract_convergence(intensive_im_sp, "SP"),
  extract_convergence(intensive_im_spq, "SPQ"),
  extract_convergence(intensive_im_spr, "SPR"),
  extract_convergence(intensive_im_spx, "SPX"),
  extract_convergence(intensive_im_dk, "DK"),
  extract_convergence(intensive_im_dkq, "DKQ"),
  extract_convergence(intensive_im_dkr, "DKR"),
  extract_convergence(intensive_im_dkx, "DKX"),
  extract_convergence(intensive_im_dis, "DIS")
)

conv_intensive_im

