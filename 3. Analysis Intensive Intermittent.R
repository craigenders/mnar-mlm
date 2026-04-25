# min(intensive_d_dk@estimates[,7], na.rm = T)
# rownames(intensive_d_dk@estimates)[which.min(intensive_d_dk@estimates[,7])]

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
filepath <- 'https://raw.githubusercontent.com/craigenders/mnar-mlm/main/intensive-intermittent.csv'

# create data frame from github data
intensive_i <- read.csv(filepath, stringsAsFactors = T)

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')
source('https://raw.githubusercontent.com/craigenders/mnar-mlm/main/mnar-plotting.R')

#------------------------------------------------------------------------------#
# COMPLETE DATA ----
#------------------------------------------------------------------------------#

intensive_i_com <- rblimp(
  data = intensive_i,
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
  burn = 25000,
  iter = 25000)

# print output
output(intensive_i_com)

#------------------------------------------------------------------------------#
# CMAR ----
#------------------------------------------------------------------------------#

# Model 1: CMAR ----
intensive_i_mar <- rblimp(
  data = intensive_i,
  clusterid = 'l2id', 
  latent = 'l2id = alpha beta omega',
  fixed = 'group',
  # center = 'groupmean = x',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    omega ~ intercept@g0o;
    alpha beta omega ~~ alpha beta omega;
    level1:
    xcent = x - x.mean;
    y ~ intercept@alpha xcent@beta;
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
output(intensive_i_mar)

#------------------------------------------------------------------------------#
# ICC FOR THE MISSINGNESS INDICATOR ----
#------------------------------------------------------------------------------#

# fit unconditional model
icc_intensive_i <- rblimp(
  data = intensive_i,
  clusterid = 'l2id', 
  # transform = 'm = ismissing(y)',
  ordinal = 'm',
  # timeid = 'time',
  # dropout = 'm = y (missing)',
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
intensive_i_tlin <- rblimp(
  data = intensive_i,
  clusterid = 'l2id', 
  # transform = 'm = ismissing(y)',
  ordinal = 'm',
  # timeid = 'time',
  # dropout = 'm = y (missing)',
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
output(intensive_i_tlin)

# quadratic trend ----
intensive_i_tquad <- rblimp(
  data = intensive_i,
  clusterid = 'l2id', 
  # transform = 'm = ismissing(y)',
  ordinal = 'm',
  # timeid = 'time',
  # dropout = 'm = y (missing)',
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
output(intensive_i_tquad)

# dummy-coded time ----
intensive_i_tdum <- rblimp(
  data = intensive_i,
  clusterid = 'l2id', 
  # transform = 'm = ismissing(y)',
  ordinal = 'm',
  # timeid = 'time',
  # dropout = 'm = y (missing)',
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
output(intensive_i_tdum)

#------------------------------------------------------------------------------#
# PLOT MISSINGNESS PROBABILITIES (INTENSIVE) ----
#------------------------------------------------------------------------------#

ymax <- .35
ymin <- 0

int_i_obs <- plot_means(m ~ time | group, 
                         model = intensive_i_tdum,
                         ylab = "Probability",
                         title = "B. Observed Probabilities",
                         group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) + xlim(0,21) +
  theme(legend.position = "top",legend.justification = "center") +
  scale_linetype_manual(values = c("dashed", "solid")) +
  geom_line(linewidth = .25)

int_i_dum <- plot_means(m.1.probability ~ time | group, 
                      model = intensive_i_tdum,
                      ylab = "Probability",
                      title = "H. Dummy Coded Time",
                      group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) + xlim(0,21) +
  theme(legend.position = "top",legend.justification = "center") +
  scale_linetype_manual(values = c("dashed", "solid")) +
  geom_line(linewidth = .25)

int_i_lin <- plot_means(m.1.probability ~ time | group, 
                      model = intensive_i_tlin,
                      ylab = "Probability",
                      title = "D. Linear Time",
                      group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) + xlim(0,21) +
  theme(legend.position = "top",legend.justification = "center") +
  scale_linetype_manual(values = c("dashed", "solid")) +
  geom_line(linewidth = .25)

int_i_quad <- plot_means(m.1.probability ~ time | group, 
                       model = intensive_i_tquad,
                       ylab = "Probability",
                       title = "F. Quadratic Time",
                       group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) + xlim(0,21) +
  theme(legend.position = "top",legend.justification = "center") +
  scale_linetype_manual(values = c("dashed", "solid")) +
  geom_line(linewidth = .25)

int_i_obs; int_i_dum; int_i_lin; int_i_quad

# compute marginal probabilities (average individual probabilities) by time and group
pmiss_intensive_i_obs <- aggregate(m ~ time + group, data = intensive_i_tdum@average_imp, mean)
pmiss_intensive_i_tlin <- aggregate(m.1.probability ~ time + group, data = intensive_i_tlin@average_imp, mean)
pmiss_intensive_i_tquad <- aggregate(m.1.probability ~ time + group, data = intensive_i_tquad@average_imp, mean)
pmiss_intensive_i_tdum <- aggregate(m.1.probability ~ time + group, data = intensive_i_tdum@average_imp, mean)

# compute rmse of marginal vs. observed probabilities
rmse_int_i_tlin <- sqrt(mean((pmiss_intensive_i_tlin$m.1.probability - pmiss_intensive_i_obs$m)^2))
rmse_int_i_tquad <- sqrt(mean((pmiss_intensive_i_tquad$m.1.probability - pmiss_intensive_i_obs$m)^2))
rmse_int_i_tdum <- sqrt(mean((pmiss_intensive_i_tdum$m.1.probability - pmiss_intensive_i_obs$m)^2))
rmse_int_i_tlin; rmse_int_i_tquad; rmse_int_i_tdum

# summarize difference between marginal vs. observed probabilities
summary(pmiss_intensive_i_tlin$m.1.probability - pmiss_intensive_i_obs$m)
summary(pmiss_intensive_i_tquad$m.1.probability - pmiss_intensive_i_obs$m)
summary(pmiss_intensive_i_tdum$m.1.probability - pmiss_intensive_i_obs$m)

#------------------------------------------------------------------------------#
# SHARED PARAMETER ----
#------------------------------------------------------------------------------#

# Model 2: Shared Parameter Model ----
intensive_i_wc <- rblimp(
  data = intensive_i,
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
output(intensive_i_wc)

# Model 3: Quadratic Shared Parameter Model ----
intensive_i_wcq <- rblimp(
  data = intensive_i,
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
output(intensive_i_wcq)

# Model 4: Residualized Shared Parameter Model ----
intensive_i_wcr <- rblimp(
  data = intensive_i,
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
output(intensive_i_wcr)

# Model 5: Shared Parameter Model With X Latent Means ----
intensive_i_wcx <- rblimp(
  data = intensive_i,
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
output(intensive_i_wcx)

#------------------------------------------------------------------------------#
# DIGGLE-KENWARD MODEL ----
#------------------------------------------------------------------------------#

# Model 6: Diggle-Kenward Model ----
intensive_i_dk <- rblimp(
  data = intensive_i,
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
output(intensive_i_dk)

# Model 7: Quadratic Diggle-Kenward Model ----
intensive_i_dkq <- rblimp(
  data = intensive_i,
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
output(intensive_i_dkq)

# Model 8: Residual Diggle-Kenward Model ----
intensive_i_dkr <- rblimp(
  data = intensive_i,
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
output(intensive_i_dkr)

# Model 9: Diggle-Kenward Model With X and Lag(X) ----
intensive_i_dkx <- rblimp(
  data = intensive_i,
  clusterid = 'l2id',
  # transform = 'm = ismissing(y)',
  ordinal = 'm',
  timeid = 'time',
  # dropout = 'm = y (missing)',
  latent = 'l2id = alpha beta omega xmean',
  fixed = 'group time',
  # center = 'groupmean = x',
  model = '
    level2:
    xmean ~ intercept;
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    omega ~ intercept@g0o;
    alpha beta omega ~~ alpha beta omega;
    level1:
    x ~ intercept@xmean;
    y ~ intercept@alpha (x - xmean)@beta;
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
output(intensive_i_dkx)

#------------------------------------------------------------------------------#
# DISAGGREGATED MODEL ----
#------------------------------------------------------------------------------#

# Model 10: Disaggregated Model ----
intensive_i_dis <- rblimp(
  data = intensive_i,
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
output(intensive_i_dis)

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
mar_tab <- extract_int_params(intensive_i_mar, "MAR")
wc_tab <- extract_int_params(intensive_i_wc, "WC")
wcq_tab <- extract_int_params(intensive_i_wcq, "WCQ")
wcr_tab <- extract_int_params(intensive_i_wcr, "WCR")
wcx_tab <- extract_int_params(intensive_i_wcx, "WCX")
dk_tab <- extract_int_params(intensive_i_dk, "DK")
dkq_tab <- extract_int_params(intensive_i_dkq, "DKQ")
dkr_tab <- extract_int_params(intensive_i_dkr, "DKR")
dkx_tab <- extract_int_params(intensive_i_dkx, "DKX")
dis_tab <- extract_int_params(intensive_i_dis, "DIS")

tab <- cbind(mar_tab,wc_tab,wcq_tab,wcr_tab,wcx_tab,dk_tab,dkq_tab,dkr_tab,dkx_tab,dis_tab)
tab_intensive_im <- tab

# write.csv(tab_intensive_im,file = '~/desktop/MNAR Results/tab_intensive_im.csv')

# mean difference table ----

# rearrange for table

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
# colnames(out) <- c("Mean_Diff", "SD", "Std_Mean_Diff", "SD", "Pseudo_R²")

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

# out$Iterations <- iter_counts[rownames(out)]

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
# write.csv(es_intensive_im,file = '~/desktop/MNAR Results/es_intensive_im.csv')

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
  extract_convergence(intensive_i_mar, "MAR"),
  extract_convergence(intensive_i_wc, "WC"),
  extract_convergence(intensive_i_wcq, "WCQ"),
  extract_convergence(intensive_i_wcr, "WCR"),
  extract_convergence(intensive_i_wcx, "WCX"),
  extract_convergence(intensive_i_dk, "DK"),
  extract_convergence(intensive_i_dkq, "DKQ"),
  extract_convergence(intensive_i_dkr, "DKR"),
  extract_convergence(intensive_i_dkx, "DKX"),
  extract_convergence(intensive_i_dis, "DIS")
)

# write.csv(conv_intensive_im,file = '~/desktop/MNAR Results/conv_intensive_im.csv')


#------------------------------------------------------------------------------#
# FIGURE 3 OLD ----
#------------------------------------------------------------------------------#

# figure3 <- int_i_obs / int_i_lin / int_i_quad / int_i_dum
# 
# ggsave(
#   filename = "~/desktop/Figure 3. Time Related (Intensive IM).pdf",
#   plot = figure3,
#   width = 8.5,
#   height = 11,
#   units = "in"
# )



#------------------------------------------------------------------------------#
# CURSIO ET AL. MODEL (INTENSIVE MEASUREMENTS) ----
#------------------------------------------------------------------------------#

# intensive_i_cursio_1pl <- rblimp(
#   data = intensive_i,
#   clusterid = 'l2id', 
#   transform = 'm = ismissing(y)',
#   # timeid = 'occasion',
#   # dropout = 'm = y (missing)',
#   ordinal = 'm group',
#   nominal = 'occasion',
#   latent = 'l2id = alpha beta u0i',
#   fixed = 'occasion group time',
#   model = '
#     level2:
#     alpha ~ intercept@g0a group@g1a;
#     beta ~ intercept@g0b group@g1b;
#     alpha ~~ beta;
#     level1:
#     y ~ intercept@alpha time@beta;
#     missingness:
#     u0i ~ intercept;
#     m ~ intercept@u0i occasion;',
#   seed = 90291,
  # chains = 4,
#   burn = 25000,
#   iter = 10000,
#   nimps = 20)
# 
# # print output
# output(intensive_i_cursio_1pl)
# 
# # cursio 2pl model
# intensive_i_cursio_2pl <- rblimp(
#   data = intensive_i,
#   clusterid = 'l2id', 
#   transform = 'm = ismissing(y)',
#   # timeid = 'occasion',
#   # dropout = 'm = y (missing)',
#   ordinal = 'm group',
#   nominal = 'occasion',
#   latent = 'l2id = alpha beta u0i',
#   fixed = 'time occasion group',
#   model = '
#     level2:
#     alpha ~ intercept@g0a group@g1a;
#     beta ~ intercept@g0b group@g1b;
#     alpha ~~ beta;
#     level1:
#     y ~ intercept@alpha time@beta;
#     missingness:
#     u0i ~ intercept;
#     m ~ intercept@u0i occasion occasion*u0i;',
#   seed = 90291,
  # chains = 4,
#   burn = 50000,
#   iter = 50000,
#   nimps = 20)
# 
# # print output
# output(intensive_i_cursio_2pl)
