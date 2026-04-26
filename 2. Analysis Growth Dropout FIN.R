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
filepath1 <- 'https://raw.githubusercontent.com/craigenders/mnar-mlm/main/growth-dropout.csv'

# create data frame from github data
growth_d <- read.csv(filepath1, stringsAsFactors = T)

# rename hard coded indicator
names(growth_d)[names(growth_d) == "m"] <- "m_"

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')
source('https://raw.githubusercontent.com/craigenders/mnar-mlm/main/mnar-plotting.R')

#------------------------------------------------------------------------------#
# CMAR ----
#------------------------------------------------------------------------------#

# MODEL 1: CMAR ----
growth_do_mar <- rblimp(
  data = growth_d,
  clusterid = 'l2id', 
  timeid = 'time',
  dropout = 'm = y (monotone)',
  latent = 'l2id = alpha beta',
  fixed = 'group time',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    alpha ~~ beta;
    level1:
    y ~ intercept@alpha time@beta;
    m ~ intercept | 1@0',
  parameters = '
    diff = (((g0a+g1a)  + 4*(g0b+g1b)) - (g0a + 4*g0b)); 
    d_diff = diff / sqrt(y.totalvar + alpha.totalvar);',
  seed = 90291,
  chains = 4,
  burn = 25000,
  iter = 25000)

# print output
output(growth_do_mar)

#------------------------------------------------------------------------------#
# TIME-RELATED CHANGES ----
#------------------------------------------------------------------------------#

# linear trend
growth_do_timelin <- rblimp(
  data = growth_d,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (monotone)',
  latent = 'l2id = alpha beta',
  fixed = 'time group',
  model = '
    level2:
    alpha ~ intercept group;
    beta ~ intercept group;
    alpha ~~ beta;
    level1:
    y ~ intercept@alpha time@beta;
    missingness:
    m ~ intercept@-3 time@b1 time*group@b2 | intercept@0;',
  seed = 90291,
  chains = 4,
  burn = 25000,
  iter = 25000)

# print output
output(growth_do_timelin)

# quadratic trend
growth_do_timequad <- rblimp(
  data = growth_d,
  clusterid = 'l2id', 
  timeid = 'time',
  dropout = 'm = y (monotone)',
  latent = 'l2id = alpha beta',
  fixed = 'time group',
  model = '
    level2:
    alpha ~ intercept group;
    beta ~ intercept group;
    alpha ~~ beta;
    level1:
    y ~ intercept@alpha time@beta;
    missingness:
    m ~ intercept@-3 time time^2 time*group time^2*group | intercept@0;',
  seed = 90291,
  chains = 4,
  burn = 25000,
  iter = 25000)

# print output
output(growth_do_timequad)

# dummy coded time
growth_do_timedum <- rblimp(
  data = growth_d,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (monotone)',
  latent = 'l2id = alpha beta',
  fixed = 'time group',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    alpha ~~ beta;
    level1:
    y ~ intercept@alpha time@beta;
    missingness:
    m ~ intercept@-3 | intercept@0;
    { t in 1:4 } : m ~ (time == [t]) (time == [t])*group;',
  parameters = '
    diff = (((g0a+g1a)  + 4*(g0b+g1b)) - (g0a + 4*g0b)); 
    d_diff = diff / sqrt(y.totalvar + alpha.totalvar);',
  seed = 90291,
  chains = 4,
  burn = 25000,
  iter = 25000)

# print output
output(growth_do_timedum)

#------------------------------------------------------------------------------#
# PLOT MISSINGNESS PROBABILITIES ----
#------------------------------------------------------------------------------#

ymax <- .5
ymin <- 0

# plot observed and predicted probabilities
pgrowth_do_obs <- plot_means(m ~ time | group, 
    model = growth_do_timedum,
    ylab = "Probability",
    title = "Observed Probabilities",
    group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) +
    theme(legend.position = "top",legend.justification = "center") +
    scale_linetype_manual(values = c("dashed", "solid")) +
    geom_line(linewidth = .25)

pgrowth_do_dum <- plot_means(m.1.probability ~ time | group, 
    model = growth_do_timedum,
    ylab = "Probability",
    title = "Dummy Coded Time",
    group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) +
    theme(legend.position = "top",legend.justification = "center") +
    scale_linetype_manual(values = c("dashed", "solid")) +
    geom_line(linewidth = .25)

pgrowth_do_lin <- plot_means(m.1.probability ~ time | group, 
    model = growth_do_timelin,
    ylab = "Probability",
    title = "Linear Time",
    group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) +
    theme(legend.position = "top",legend.justification = "center") +
    scale_linetype_manual(values = c("dashed", "solid")) +
    geom_line(linewidth = .25)

pgrowth_do_quad <- plot_means(m.1.probability ~ time | group, 
    model = growth_do_timequad,
    ylab = "Probability",
    title = "Quadratic Time",
    group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) +
    theme(legend.position = "top",legend.justification = "center") +
    scale_linetype_manual(values = c("dashed", "solid")) +
    geom_line(linewidth = .25)

pgrowth_do_combined <- (pgrowth_do_obs | pgrowth_do_lin) / (pgrowth_do_quad | pgrowth_do_dum)
pgrowth_do_combined

# compute marginal probabilities (average individual probabilities) by time and group
pmiss_growth_do_obs <- aggregate(m ~ time + group, data = growth_do_timedum@average_imp, mean)
pmiss_growth_do_timedum <- aggregate(m.1.probability ~ time + group, data = growth_do_timedum@average_imp, mean)
pmiss_growth_do_timelin <- aggregate(m.1.probability ~ time + group, data = growth_do_timelin@average_imp, mean)
pmiss_growth_do_timequad <- aggregate(m.1.probability ~ time + group, data = growth_do_timequad@average_imp, mean)

# compute rmse of marginal vs. observed probabilities
rmse_growth_do_timedum <- sqrt(mean((pmiss_growth_do_timedum$m.1.probability - pmiss_growth_do_obs$m)^2))
rmse_growth_do_timelin <- sqrt(mean((pmiss_growth_do_timelin$m.1.probability - pmiss_growth_do_obs$m)^2))
rmse_growth_do_timequad <- sqrt(mean((pmiss_growth_do_timequad$m.1.probability - pmiss_growth_do_obs$m)^2))

miss_fit <- data.frame(
  RMSE   = round(c(rmse_growth_do_timelin, rmse_growth_do_timequad, rmse_growth_do_timedum), 3),
  Min    = round(c(
    min(pmiss_growth_do_timelin$m.1.probability   - pmiss_growth_do_obs$m),
    min(pmiss_growth_do_timequad$m.1.probability  - pmiss_growth_do_obs$m),
    min(pmiss_growth_do_timedum$m.1.probability   - pmiss_growth_do_obs$m)), 3),
  Median = round(c(
    median(pmiss_growth_do_timelin$m.1.probability   - pmiss_growth_do_obs$m),
    median(pmiss_growth_do_timequad$m.1.probability  - pmiss_growth_do_obs$m),
    median(pmiss_growth_do_timedum$m.1.probability   - pmiss_growth_do_obs$m)), 3),
  Mean   = round(c(
    mean(pmiss_growth_do_timelin$m.1.probability   - pmiss_growth_do_obs$m),
    mean(pmiss_growth_do_timequad$m.1.probability  - pmiss_growth_do_obs$m),
    mean(pmiss_growth_do_timedum$m.1.probability   - pmiss_growth_do_obs$m)), 3),
  Max    = round(c(
    max(pmiss_growth_do_timelin$m.1.probability   - pmiss_growth_do_obs$m),
    max(pmiss_growth_do_timequad$m.1.probability  - pmiss_growth_do_obs$m),
    max(pmiss_growth_do_timedum$m.1.probability   - pmiss_growth_do_obs$m)), 3),
  row.names = c("Linear", "Quadratic", "Dummy")
)

miss_fit

#------------------------------------------------------------------------------#
# SHARED PARAMETER MODEL ----
#------------------------------------------------------------------------------#

# MODEL 2: Shared Parameter Model ----
growth_do_sp <- rblimp(
  data = growth_d,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (monotone)',
  latent = 'l2id = alpha beta',
  fixed = 'group time',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    alpha ~~ beta;
    level1:
    y ~ intercept@alpha time@beta;
    missingness:
    d = ifelse(time < 1, 0, 1);
    m ~ intercept@-3 alpha*d beta*d | intercept@0;
    { t in 1:4 } : m ~ (time == [t]) (time == [t])*group;',
  parameters = '
    diff = (((g0a+g1a)  + 4*(g0b+g1b)) - (g0a + 4*g0b)); 
    d_diff = diff / sqrt(y.totalvar + alpha.totalvar);',
  seed = 90291,
  chains = 4,
  burn = 25000,
  iter = 25000)

# print output
output(growth_do_sp)

# MODEL 3: Quadratic Shared Parameter Model ----
growth_do_spq <- rblimp(
  data = growth_d,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (monotone)',
  latent = 'l2id = alpha beta',
  fixed = 'group time',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    alpha ~~ beta;
    level1:
    y ~ intercept@alpha time@beta;
    missingness:
    d = ifelse(time < 1, 0, 1);
    m ~ intercept@-3 alpha*d alpha*d^2 beta*d beta*d^2 | intercept@0;
    { t in 1:4 } : m ~ (time == [t]) (time == [t])*group;',
  parameters = '
    diff = (((g0a+g1a)  + 4*(g0b+g1b)) - (g0a + 4*g0b)); 
    d_diff = diff / sqrt(y.totalvar + alpha.totalvar);',
  seed = 90291,
  chains = 4,
  burn = 25000,
  iter = 25000)

# print output
output(growth_do_spq)

# MODEL 4: Residual Shared Parameter Model ----
growth_do_spr <- rblimp(
  data = growth_d,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (monotone)',
  latent = 'l2id = alpha beta',
  fixed = 'group time',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    alpha ~~ beta;
    level1:
    y ~ intercept@alpha time@beta;
    missingness:
    alpha_res = alpha - (g0a + g1a*group);
    beta_res = beta - (g0b + g1b*group);
    d = ifelse(time < 1, 0, 1);
    m ~ intercept@-3 alpha_res*d beta_res*d | intercept@0;
    { t in 1:4 } : m ~ (time == [t]) (time == [t])*group;',
  parameters = '
    diff = (((g0a+g1a)  + 4*(g0b+g1b)) - (g0a + 4*g0b)); 
    d_diff = diff / sqrt(y.totalvar + alpha.totalvar);',
  seed = 90291,
  chains = 4,
  burn = 200000,
  iter = 200000)

# print output
output(growth_do_spr)

#------------------------------------------------------------------------------#
# SELECTION MODEL ----
#------------------------------------------------------------------------------#

# MODEL 5: Diggle-Kenward Model ----
growth_do_dk <- rblimp(
  data = growth_d,
  clusterid = 'l2id',
  timeid = 'time',
    dropout = 'm = y (monotone)',
  latent = 'l2id = alpha beta',
  fixed = 'group time',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    alpha ~~ beta;
    level1:
    y ~ intercept@alpha time@beta;
    missingness:
    d = ifelse(time < 1, 0, 1);
    m ~ intercept@-3 y*d y.lag*d | intercept@0;
    { t in 1:4 } : m ~ (time == [t]) (time == [t])*group;',
  parameters = '
    diff = (((g0a+g1a)  + 4*(g0b+g1b)) - (g0a + 4*g0b)); 
    d_diff = diff / sqrt(y.totalvar + alpha.totalvar);',
  seed = 90291,
  chains = 4,
  burn = 25000,
  iter = 25000)

# print output
output(growth_do_dk)

# MODEL 6: Quadratic Diggle-Kenward Model ----
growth_do_dkq <- rblimp(
  data = growth_d,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (monotone)',
  latent = 'l2id = alpha beta',
  fixed = 'group time',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    alpha ~~ beta;
    level1:
    y ~ intercept@alpha time@beta;
    missingness:
    d = ifelse(time < 1, 0, 1);
    m ~ intercept@-3 y*d y^2*d y.lag*d | intercept@0;
    { t in 1:4 } : m ~ (time == [t]) (time == [t])*group;',
  parameters = '
    diff = (((g0a+g1a)  + 4*(g0b+g1b)) - (g0a + 4*g0b)); 
    d_diff = diff / sqrt(y.totalvar + alpha.totalvar);',
  seed = 90291,
  chains = 4,
  burn = 50000,
  iter = 50000)

# print output
output(growth_do_dkq)

# MODEL 7: Residual Diggle-Kenward Model ----
growth_do_dkd <- rblimp(
  data = growth_d,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (monotone)',
  latent = 'l2id = alpha beta',
  fixed = 'group time',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    alpha ~~ beta;
    level1:
    y ~ intercept@alpha time@beta;
    missingness:
    d = ifelse(time < 1, 0, 1);
    yw = y - (alpha + beta*time);
    lagyw = y.lag - (alpha + beta*(time - 1));
    m ~ intercept@-3 yw*d lagyw*d | intercept@0;
    { t in 1:4 } : m ~ (time == [t]) (time == [t])*group;',
  parameters = '
    diff = (((g0a+g1a)  + 4*(g0b+g1b)) - (g0a + 4*g0b)); 
    d_diff = diff / sqrt(y.totalvar + alpha.totalvar);',
  seed = 90291,
  chains = 4,
  burn = 200000,
  iter = 200000)

# print output
output(growth_do_dkd)

#------------------------------------------------------------------------------#
# DISAGGREGATED MODEL ----
#------------------------------------------------------------------------------#

# MODEL 8: Disaggregated Model ----
growth_do_dis <- rblimp(
  data = growth_d,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (monotone)',
  latent = 'l2id = alpha beta',
  fixed = 'group time',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    alpha ~~ beta;
    level1:
    y ~ intercept@alpha time@beta;
    missingness:
    d = ifelse(time < 1, 0, 1);
    yw = y - (alpha + beta*time);
    lagyw = y.lag - (alpha + beta*(time - 1));
    m ~ intercept@-3 yw*d lagyw*d alpha*d beta*d | intercept@0;
    { t in 1:4 } : m ~ (time == [t]) (time == [t])*group;',
  parameters = '
    diff = (((g0a+g1a)  + 4*(g0b+g1b)) - (g0a + 4*g0b)); 
    d_diff = diff / sqrt(y.totalvar + alpha.totalvar);',
  seed = 90291,
  chains = 4,
  burn = 100000,
  iter = 100000)

# print output
output(growth_do_dis)

#------------------------------------------------------------------------------#
# EXTRACT ESTIMATES ----
#------------------------------------------------------------------------------#

extract_growth_params <- function(object, method) {
  
  tab <- object@estimates
  
  rows <- c(
    "alpha ~ Intercept",
    "beta ~ Intercept",
    "alpha ~ group",
    "beta ~ group",
    "alpha residual variance",
    "beta residual variance",
    "Cor( alpha, beta )",
    "y residual variance",
    "Parameter: diff",
    "Parameter: d_diff",
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

mar_tab <- extract_growth_params(growth_do_mar, "MAR")
sp_tab  <- extract_growth_params(growth_do_sp, "SP")
spq_tab <- extract_growth_params(growth_do_spq, "SPQ")
spr_tab <- extract_growth_params(growth_do_spr, "SPR")
dk_tab  <- extract_growth_params(growth_do_dk, "DK")
dkq_tab <- extract_growth_params(growth_do_dkq, "DKQ")
dkd_tab <- extract_growth_params(growth_do_dkd, "DKD")
dis_tab <- extract_growth_params(growth_do_dis, "DIS")

tab <- cbind(mar_tab,sp_tab,spq_tab,spr_tab,dk_tab,dkq_tab,dkd_tab,dis_tab)
tab_growth_do <- tab
tab_growth_do

# mean difference table ----

# Extract rows
mean_row   <- tab["Mean Diff.", ]
std_row    <- tab["Std. Mean Diff.", ]
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
  MAR = nrow(growth_do_mar@iterations),
  SP  = nrow(growth_do_sp@iterations),
  SPQ = nrow(growth_do_spq@iterations),
  SPR = nrow(growth_do_spr@iterations),
  DK  = nrow(growth_do_dk@iterations),
  DKQ = nrow(growth_do_dkq@iterations),
  DKD = nrow(growth_do_dkd@iterations),
  DIS = nrow(growth_do_dis@iterations)
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

es_growth_do <- out
es_growth_do

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

conv_growth_do <- rbind(
  extract_convergence(growth_do_mar, "MAR"),
  extract_convergence(growth_do_sp, "SP"),
  extract_convergence(growth_do_spq, "SPQ"),
  extract_convergence(growth_do_spr, "SPR"),
  extract_convergence(growth_do_dk, "DK"),
  extract_convergence(growth_do_dkq, "DKQ"),
  extract_convergence(growth_do_dkd, "DKD"),
  extract_convergence(growth_do_dis, "DIS")
)

conv_growth_do

