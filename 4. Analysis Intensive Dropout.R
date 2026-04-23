#------------------------------------------------------------------------------#
# LOAD R PACKAGES ----
#------------------------------------------------------------------------------#

options (scipen = 999)

# load packages
library(ggplot2)
library(patchwork)
library(rblimp)

# set_blimp('/applications/blimp/blimp')
# set_blimp('/applications/blimp/blimp-nightly')
# remotes::update_packages('rblimp')

#------------------------------------------------------------------------------#
# READ DATA ----
#------------------------------------------------------------------------------#

# github url for raw data
filepath <- 'https://raw.githubusercontent.com/craigenders/mnar-mlm/main/intensive-dropout.csv'

# create data frame from github data
intensive_d <- read.csv(filepath, stringsAsFactors = T)

intensive_d_comp <- intensive_d

intensive_d$y[is.na(intensive_d$m)] <- NA
intensive_d$x[is.na(intensive_d$m)] <- NA
intensive_d <- intensive_d[!is.na(intensive_d$m),]

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')
source('https://raw.githubusercontent.com/craigenders/mnar-mlm/main/mnar-plotting.R')

#------------------------------------------------------------------------------#
# COMPLETE DATA ----
#------------------------------------------------------------------------------#

intensive_d_com <- rblimp(
  data = intensive_d_comp,
  clusterid = 'l2id', 
  latent = 'l2id = alpha beta omega',
  fixed = 'group',
  center = 'groupmean = xcom',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    omega ~ intercept@g0o;
    alpha beta omega ~~ alpha beta omega;
    level1:
    ycom ~ intercept@alpha xcom@beta;
    var(ycom) ~ intercept@omega;',
  parameters = '
    adiff = g1a; 
    d_adiff = adiff / sqrt(alpha.totalvar);
    bdiff = g1b; 
    d_bdiff = bdiff / sqrt(exp(g0o));',
  seed = 90291,
  chains = 4,
  burn = 10000,
  iter = 10000)

# print output
output(intensive_d_com)

#------------------------------------------------------------------------------#
# CMAR ----
#------------------------------------------------------------------------------#

# Model 1: CMAR ----
intensive_d_mar <- rblimp(
  data = intensive_d,
  clusterid = 'l2id', 
  latent = 'l2id = alpha beta omega',
  fixed = 'group',
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
  burn = 20000,
  iter = 20000)

# print output
output(intensive_d_mar)

#------------------------------------------------------------------------------#
# TIME-RELATED CHANGES (INTENSIVE MEASUREMENTS) ----
#------------------------------------------------------------------------------#

# linear trend ----
intensive_d_tlin <- rblimp(
  data = intensive_d,
  clusterid = 'l2id', 
  # transform = 'm = ismissing(y)',
  # timeid = 'time',
  # dropout = 'm = y (missing)',
  ordinal = 'm group',
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
  burn = 20000,
  iter = 20000,
  nimps = 20)

# print output
output(intensive_d_tlin)

# quadratic trend ----
intensive_d_tquad <- rblimp(
  data = intensive_d,
  clusterid = 'l2id', 
  # transform = 'm = ismissing(y)',
  # timeid = 'time',
  # dropout = 'm = y (missing)',
  ordinal = 'm group',
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
  burn = 20000,
  iter = 20000,
  nimps = 20)

# print output
output(intensive_d_tquad)

# dummy-coded time ----
intensive_d_tdum <- rblimp(
  data = intensive_d,
  clusterid = 'l2id', 
  # timeid = 'time',
  # dropout = 'm = y (missing)',
  ordinal = 'm',
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
  burn = 20000,
  iter = 20000)

# print output
output(intensive_d_tdum)

#------------------------------------------------------------------------------#
# PLOT MISSINGNESS PROBABILITIES (INTENSIVE) ----
#------------------------------------------------------------------------------#

ymax <- .15
ymin <- 0

int_d_obs <- plot_means(m ~ time | group, 
                      model = intensive_d_tdum,
                      ylab = "Probability",
                      title = "B. Observed Probabilities",
                      group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) +
  theme(legend.position = "top",legend.justification = "center") +
  scale_linetype_manual(values = c("dashed", "solid")) +
  geom_line(linewidth = .25)

int_d_dum <- plot_means(m.1.probability ~ time | group, 
                      model = intensive_d_tdum,
                      ylab = "Probability",
                      title = "H. Dummy Coded Time",
                      group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) +
  theme(legend.position = "top",legend.justification = "center") +
  scale_linetype_manual(values = c("dashed", "solid")) +
  geom_line(linewidth = .25)

int_d_lin <- plot_means(m.1.probability ~ time | group, 
                      model = intensive_d_tlin,
                      ylab = "Probability",
                      title = "D. Linear Time",
                      group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) +
  theme(legend.position = "top",legend.justification = "center") +
  scale_linetype_manual(values = c("dashed", "solid")) +
  geom_line(linewidth = .25)

int_d_quad <- plot_means(m.1.probability ~ time | group, 
                       model = intensive_d_tquad,
                       ylab = "Probability",
                       title = "F. Quadratic Time",
                       group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) +
  theme(legend.position = "top",legend.justification = "center") +
  scale_linetype_manual(values = c("dashed", "solid")) +
  geom_line(linewidth = .25)

# compute marginal probabilities (average individual probabilities) by time and group
pmiss_intensive_d_obs <- aggregate(m ~ time + group, data = intensive_d_tdum@average_imp, mean)
pmiss_intensive_d_tdum <- aggregate(m.1.probability ~ time + group, data = intensive_d_tdum@average_imp, mean)
pmiss_intensive_d_tlin <- aggregate(m.1.probability ~ time + group, data = intensive_d_tlin@average_imp, mean)
pmiss_intensive_d_tquad <- aggregate(m.1.probability ~ time + group, data = intensive_d_tquad@average_imp, mean)

# compute rmse of marginal vs. observed probabilities
rmse_int_d_tdum <- sqrt(mean((pmiss_intensive_d_tdum$m.1.probability - pmiss_intensive_d_obs$m)^2))
rmse_int_d_tlin <- sqrt(mean((pmiss_intensive_d_tlin$m.1.probability - pmiss_intensive_d_obs$m)^2))
rmse_int_d_tquad <- sqrt(mean((pmiss_intensive_d_tquad$m.1.probability - pmiss_intensive_d_obs$m)^2))
rmse_int_d_tdum; rmse_int_d_tlin; rmse_int_d_tquad

# summarize difference between marginal vs. observed probabilities
summary(pmiss_intensive_d_tdum$m.1.probability - pmiss_intensive_d_obs$m)
summary(pmiss_intensive_d_tlin$m.1.probability - pmiss_intensive_d_obs$m)
summary(pmiss_intensive_d_tquad$m.1.probability - pmiss_intensive_d_obs$m)

#------------------------------------------------------------------------------#
# SHARED PARAMETER ----
#------------------------------------------------------------------------------#

# Model 2: Shared Parameter Model ----
intensive_d_wc <- rblimp(
  data = intensive_d,
  clusterid = 'l2id',
  # transform = 'm = ismissing(y)',
  ordinal = 'm',
  timeid = 'time',
  # dropout = 'm = y (missing)',
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
output(intensive_d_wc)

# Model 3: Quadratic Shared Parameter Model ----
intensive_d_wcq <- rblimp(
  data = intensive_d,
  clusterid = 'l2id',
  # transform = 'm = ismissing(y)',
  ordinal = 'm',
  timeid = 'time',
  # dropout = 'm = y (missing)',
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
output(intensive_d_wcq)

# Model 4: Residualized Shared Parameter Model ----
intensive_d_wcr <- rblimp(
  data = intensive_d,
  clusterid = 'l2id',
  # transform = 'm = ismissing(y)',
  ordinal = 'm',
  timeid = 'time',
  # dropout = 'm = y (missing)',
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
  burn = 20000,
  iter = 20000)

# print output
output(intensive_d_wcr)

# Model 5: Shared Parameter Model With X Latent Means ----
intensive_d_wcx <- rblimp(
  data = intensive_d,
  clusterid = 'l2id',
  # transform = 'm = ismissing(y)',
  ordinal = 'm',
  timeid = 'time',
  # dropout = 'm = y (missing)',
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
    m ~ intercept@-3 d*alpha_res d*omega d*x.mean | intercept@0;
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
output(intensive_d_wcx)

#------------------------------------------------------------------------------#
# DIGGLE-KENWARD MODEL ----
#------------------------------------------------------------------------------#

# Model 6: Diggle-Kenward Model ----
intensive_d_dk <- rblimp(
  data = intensive_d,
  clusterid = 'l2id',
  # transform = 'm = ismissing(y)',
  ordinal = 'm',
  timeid = 'time',
  # dropout = 'm = y (missing)',
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
  burn = 30000,
  iter = 30000)

# print output
output(intensive_d_dk)

set_blimp('/applications/blimp/blimp-nightly')
set_blimp('/applications/blimp/blimp')

# Model 7: Quadratic Diggle-Kenward Model ----
intensive_d_dkq <- rblimp(
  data = intensive_d,
  clusterid = 'l2id',
  # transform = 'm = ismissing(y)',
  ordinal = 'm',
  timeid = 'time',
  # dropout = 'm = y (missing)',
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
  burn = 125000,
  iter = 125000)

# print output
output(intensive_d_dkq)

# Model 8: Residual Diggle-Kenward Model ----
intensive_d_dkr <- rblimp(
  data = intensive_d,
  clusterid = 'l2id',
  # transform = 'm = ismissing(y)',
  ordinal = 'm',
  timeid = 'time',
  # dropout = 'm = y (missing)',
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
  burn = 50000,
  iter = 50000)

# print output
output(intensive_d_dkr)

# Model 9: Diggle-Kenward Model With X and Lag(X) ----
intensive_d_dkx <- rblimp(
  data = intensive_d,
  clusterid = 'l2id',
  # transform = 'm = ismissing(y)',
  ordinal = 'm',
  timeid = 'time',
  # dropout = 'm = y (missing)',
  latent = 'l2id = alpha beta omega xmeans',
  fixed = 'group time',
  # center = 'groupmean = x',
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
  burn = 30000,
  iter = 30000)

# print output
output(intensive_d_dkx)

# intensive_d_dkx@estimates

#------------------------------------------------------------------------------#
# DISAGGREGATED MODEL ----
#------------------------------------------------------------------------------#

# Model 10: Disaggregated Model ----
intensive_d_dis <- rblimp(
  data = intensive_d,
  clusterid = 'l2id',
  # transform = 'm = ismissing(y)',
  ordinal = 'm',
  timeid = 'time',
  # dropout = 'm = y (missing)',
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
    m ~ intercept@-3 (y - alpha)*d (y.lag - alpha)*d alpha_res*d omega*d | intercept@0;
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
output(intensive_d_dis)

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

# com_tab <- extract_growth_d_params(growth_d_com, "COM")
mar_tab <- extract_int_params(intensive_d_mar, "MAR")
wc_tab <- extract_int_params(intensive_d_wc, "WC")
wcq_tab <- extract_int_params(intensive_d_wcq, "WCQ")
wcr_tab <- extract_int_params(intensive_d_wcr, "WCR")
wcx_tab <- extract_int_params(intensive_d_wcx, "WCX")
dk_tab <- extract_int_params(intensive_d_dk, "DK")
dkq_tab <- extract_int_params(intensive_d_dkq, "DKQ")
dkr_tab <- extract_int_params(intensive_d_dkr, "DKR")
dkx_tab <- extract_int_params(intensive_d_dkx, "DKX")
dis_tab <- extract_int_params(intensive_d_dis, "DIS")

tab <- cbind(mar_tab,wc_tab,wcq_tab,wcr_tab,wcx_tab,dk_tab,dkq_tab,dkr_tab,dkx_tab,dis_tab)
tab_intensive_do <- tab

# write.csv(tab_intensive_do,file = '~/desktop/MNAR Results/tab_intensive_do.csv')

# mean difference table ----

# rearrange for table

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
colnames(out) <- c("Mean_Diff", "SD", "Std_Mean_Diff", "SD", "Pseudo_R²")

out
es_intensive_do <- out

# write.csv(es_intensive_do,file = '~/desktop/MNAR Results/es_intensive_do.csv')

# diagnostics table ----

extract_convergence <- function(object, method) {
  
  neff <- object@estimates[, "N_Eff"]
  neff <- neff[is.finite(neff)]
  
  psr_row <- as.numeric(object@psr[20, ])
  psr_row <- psr_row[is.finite(psr_row)]
  
  data.frame(
    Min_Neff = round(min(neff),    0),
    Max_Neff = round(max(neff),    0),
    Min_PSR  = round(min(psr_row), 3),
    Max_PSR  = round(max(psr_row), 3),
    row.names = method
  )
}

conv_intensive_do <- rbind(
  extract_convergence(intensive_d_mar, "MAR"),
  extract_convergence(intensive_d_wc, "WC"),
  extract_convergence(intensive_d_wcq, "WCQ"),
  extract_convergence(intensive_d_wcr, "WCR"),
  extract_convergence(intensive_d_wcx, "WCX"),
  extract_convergence(intensive_d_dk, "DK"),
  extract_convergence(intensive_d_dkq, "DKQ"),
  extract_convergence(intensive_d_dkr, "DKR"),
  extract_convergence(intensive_d_dkx, "DKX"),
  extract_convergence(intensive_d_dis, "DIS")
)

# write.csv(conv_intensive_do,file = '~/desktop/MNAR Results/conv_intensive_do.csv')


