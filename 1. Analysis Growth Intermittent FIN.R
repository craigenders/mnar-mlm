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
filepath1 <- 'https://raw.githubusercontent.com/craigenders/mnar-mlm/main/growth-intermittent.csv'

# create data frame from github data
growth_i <- read.csv(filepath1, stringsAsFactors = T)

# rename hard coded indicator
names(growth_i)[names(growth_i) == "m"] <- "m_"

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')
source('https://raw.githubusercontent.com/craigenders/mnar-mlm/main/mnar-plotting.R')

#------------------------------------------------------------------------------#
# CMAR ----
#------------------------------------------------------------------------------#

# MODEL 1: CMAR ----
growth_im_mar <- rblimp(
  data = growth_i,
  clusterid = 'l2id', 
  timeid = 'time',
  dropout = 'm = y (missing)',
  latent = 'l2id = alpha beta',
  fixed = 'group time',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    alpha ~~ beta;
    level1:
    y ~ intercept@alpha time@beta;
    m ~ intercept',
  parameters = '
    diff = (((g0a+g1a)  + 4*(g0b+g1b)) - (g0a + 4*g0b)); 
    d_diff = diff / sqrt(y.totalvar + alpha.totalvar);',
  seed = 90291,
  chains = 4,
  burn = 25000,
  iter = 25000)

# print output
output(growth_im_mar)

#------------------------------------------------------------------------------#
# ICC FOR THE MISSINGNESS INDICATOR ----
#------------------------------------------------------------------------------#

# fit unconditional model
icc_growth_i <- rblimp(
  data = growth_i,
  clusterid = 'l2id', 
  timeid = 'time',
  dropout = 'm = y (missing)',
  model = 'm ~ intercept | intercept;',
  seed = 90291,
  chains = 4,
  burn = 25000,
  iter = 25000)

# print output
output(icc_growth_i)

#------------------------------------------------------------------------------#
# TIME-RELATED CHANGES ----
#------------------------------------------------------------------------------#

# linear trend
growth_im_timelin <- rblimp(
  data = growth_i,
  clusterid = 'l2id', 
  timeid = 'time',
  dropout = 'm = y (missing)',
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
    m ~ intercept time group time*group | intercept;',
  seed = 90291,
  chains = 4,
  burn = 25000,
  iter = 25000)

# print output
output(growth_im_timelin)

# quadratic trend
growth_im_timequad <- rblimp(
  data = growth_i,
  clusterid = 'l2id', 
  timeid = 'time',
  dropout = 'm = y (missing)',
  latent = 'l2id = alpha beta',
  fixed = 'time group',
  model = '
    level2:
    alpha ~ 1 group;
    beta ~ 1 group;
    alpha ~~ beta;
    level1:
    y ~ intercept@alpha time@beta;
    missingness:
    m ~ intercept time time^2 group time*group time^2*group | intercept;',
  seed = 90291,
  chains = 4,
  burn = 25000,
  iter = 25000)

# print output
output(growth_im_timequad)

# dummy coded time
growth_im_timedum <- rblimp(
  data = growth_i,
  clusterid = 'l2id', 
  timeid = 'time',
  dropout = 'm = y (missing)',
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
    m ~ intercept group | intercept;
    { t in 1:4 } : m ~ (time == [t]) (time == [t])*group;',
  parameters = '
    diff = (((g0a+g1a)  + 4*(g0b+g1b)) - (g0a + 4*g0b)); 
    d_diff = diff / sqrt(y.totalvar + alpha.totalvar);',
  seed = 90291,
  chains = 4,
  burn = 25000,
  iter = 25000)

# print output
output(growth_im_timedum)

#------------------------------------------------------------------------------#
# PLOT MISSINGNESS PROBABILITIES ----
#------------------------------------------------------------------------------#

ymax <- .35
ymin <- 0

# plot observed and predicted probabilities
pgrowth_im_obs <- plot_means(m ~ time | group, 
    model = growth_im_timedum,
    ylab = "Probability",
    title = "Observed Probabilities",
    group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) +
    theme(legend.position = "top",legend.justification = "center") +
    scale_linetype_manual(values = c("dashed", "solid")) +
    geom_line(linewidth = .25)

pgrowth_im_dum <- plot_means(m.1.probability ~ time | group, 
    model = growth_im_timedum,
    ylab = "Probability",
    title = "Dummy Coded Time",
    group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) +
    theme(legend.position = "top",legend.justification = "center") +
    scale_linetype_manual(values = c("dashed", "solid")) +
    geom_line(linewidth = .25)

pgrowth_im_lin <- plot_means(m.1.probability ~ time | group, 
    model = growth_im_timelin,
    ylab = "Probability",
    title = "Linear Time",
    group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) +
    theme(legend.position = "top",legend.justification = "center") +
    scale_linetype_manual(values = c("dashed", "solid")) +
    geom_line(linewidth = .25)

pgrowth_im_quad <- plot_means(m.1.probability ~ time | group, 
    model = growth_im_timequad,
    ylab = "Probability",
    title = "Quadratic Time",
    group_labels = c("0" = "0", "1" = "1")) + ylim(ymin,ymax) +
    theme(legend.position = "top",legend.justification = "center") +
    scale_linetype_manual(values = c("dashed", "solid")) +
    geom_line(linewidth = .25)

pgrowth_im_combined <- (pgrowth_im_obs | pgrowth_im_lin) / (pgrowth_im_quad | pgrowth_im_dum)
pgrowth_im_combined

# compute marginal probabilities (average individual probabilities) by time and group
pmiss_growth_im_obs <- aggregate(m ~ time + group, data = growth_im_timedum@average_imp, mean)
pmiss_growth_im_timedum <- aggregate(m.1.probability ~ time + group, data = growth_im_timedum@average_imp, mean)
pmiss_growth_im_timelin <- aggregate(m.1.probability ~ time + group, data = growth_im_timelin@average_imp, mean)
pmiss_growth_im_timequad <- aggregate(m.1.probability ~ time + group, data = growth_im_timequad@average_imp, mean)

# compute rmse of marginal vs. observed probabilities
rmse_growth_im_timedum <- sqrt(mean((pmiss_growth_im_timedum$m.1.probability - pmiss_growth_im_obs$m)^2))
rmse_growth_im_timelin <- sqrt(mean((pmiss_growth_im_timelin$m.1.probability - pmiss_growth_im_obs$m)^2))
rmse_growth_im_timequad <- sqrt(mean((pmiss_growth_im_timequad$m.1.probability - pmiss_growth_im_obs$m)^2))

miss_fit <- data.frame(
  RMSE   = round(c(rmse_growth_im_timelin, rmse_growth_im_timequad, rmse_growth_im_timedum), 3),
  Min    = round(c(
    min(pmiss_growth_im_timelin$m.1.probability   - pmiss_growth_im_obs$m),
    min(pmiss_growth_im_timequad$m.1.probability  - pmiss_growth_im_obs$m),
    min(pmiss_growth_im_timedum$m.1.probability   - pmiss_growth_im_obs$m)), 3),
  Median = round(c(
    median(pmiss_growth_im_timelin$m.1.probability   - pmiss_growth_im_obs$m),
    median(pmiss_growth_im_timequad$m.1.probability  - pmiss_growth_im_obs$m),
    median(pmiss_growth_im_timedum$m.1.probability   - pmiss_growth_im_obs$m)), 3),
  Mean   = round(c(
    mean(pmiss_growth_im_timelin$m.1.probability   - pmiss_growth_im_obs$m),
    mean(pmiss_growth_im_timequad$m.1.probability  - pmiss_growth_im_obs$m),
    mean(pmiss_growth_im_timedum$m.1.probability   - pmiss_growth_im_obs$m)), 3),
  Max    = round(c(
    max(pmiss_growth_im_timelin$m.1.probability   - pmiss_growth_im_obs$m),
    max(pmiss_growth_im_timequad$m.1.probability  - pmiss_growth_im_obs$m),
    max(pmiss_growth_im_timedum$m.1.probability   - pmiss_growth_im_obs$m)), 3),
  row.names = c("Linear", "Quadratic", "Dummy")
)

miss_fit

#------------------------------------------------------------------------------#
# SHARED PARAMETER MODEL ----
#------------------------------------------------------------------------------#

# MODEL 2: Shared Parameter Model ----
growth_im_sp <- rblimp(
  data = growth_i,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (missing)',
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
    m ~ intercept group alpha beta | intercept;
    { t in 1:4 } : m ~ (time == [t]) (time == [t])*group;',
  parameters = '
    diff = (((g0a+g1a)  + 4*(g0b+g1b)) - (g0a + 4*g0b)); 
    d_diff = diff / sqrt(y.totalvar + alpha.totalvar);',
  seed = 90291,
  chains = 4,
  burn = 25000,
  iter = 25000)

# print output
output(growth_im_sp)

# MODEL 3: Quadratic Shared Parameter Model ----
growth_im_spq <- rblimp(
  data = growth_i,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (missing)',
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
    m ~ intercept group alpha alpha^2 beta beta^2 | intercept;
    { t in 1:4 } : m ~ (time == [t]) (time == [t])*group;',
  parameters = '
    diff = (((g0a+g1a)  + 4*(g0b+g1b)) - (g0a + 4*g0b)); 
    d_diff = diff / sqrt(y.totalvar + alpha.totalvar);',
  seed = 90291,
  chains = 4,
  burn = 25000,
  iter = 25000)

# print output
output(growth_im_spq)

# MODEL 4: Residual Shared Parameter Model ----
growth_im_spr <- rblimp(
  data = growth_i,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (missing)',
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
    m ~ intercept group alpha_res beta_res | intercept;
    { t in 1:4 } : m ~ (time == [t]) (time == [t])*group;',
  parameters = '
    diff = (((g0a+g1a)  + 4*(g0b+g1b)) - (g0a + 4*g0b)); 
    d_diff = diff / sqrt(y.totalvar + alpha.totalvar);',
  seed = 90291,
  chains = 4,
  burn = 25000,
  iter = 25000)

# print output
output(growth_im_spr)

#------------------------------------------------------------------------------#
# SELECTION MODEL ----
#------------------------------------------------------------------------------#

# MODEL 5: Diggle-Kenward Model ----
growth_im_dk <- rblimp(
  data = growth_i,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (missing)',
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
    m ~ intercept group y y.lag | intercept;
    { t in 1:4 } : m ~ (time == [t]) (time == [t])*group;',
  parameters = '
    diff = (((g0a+g1a)  + 4*(g0b+g1b)) - (g0a + 4*g0b)); 
    d_diff = diff / sqrt(y.totalvar + alpha.totalvar);',
  seed = 90291,
  chains = 4,
  burn = 25000,
  iter = 25000)

# print output
output(growth_im_dk)

# MODEL 6: Quadratic Diggle-Kenward Model ----
growth_im_dkq <- rblimp(
  data = growth_i,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (missing)',
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
    m ~ intercept group y y^2 y.lag | intercept;
    { t in 1:4 } : m ~ (time == [t]) (time == [t])*group;',
  parameters = '
    diff = (((g0a+g1a)  + 4*(g0b+g1b)) - (g0a + 4*g0b)); 
    d_diff = diff / sqrt(y.totalvar + alpha.totalvar);',
  seed = 90291,
  chains = 4,
  burn = 25000,
  iter = 25000)

# print output
output(growth_im_dkq)

# MODEL 7: Residual Diggle-Kenward Model ----
growth_im_dkd <- rblimp(
  data = growth_i,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (missing)',
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
    yw = y - (alpha + beta*time);
    lagyw = y.lag - (alpha + beta*(time - 1));
    m ~ intercept group yw lagyw | intercept;
    { t in 1:4 } : m ~ (time == [t]) (time == [t])*group;',
  parameters = '
    diff = (((g0a+g1a)  + 4*(g0b+g1b)) - (g0a + 4*g0b)); 
    d_diff = diff / sqrt(y.totalvar + alpha.totalvar);',
  seed = 90291,
  chains = 4,
  burn = 200000,
  iter = 200000)

# print output
output(growth_im_dkd)

#------------------------------------------------------------------------------#
# DISAGGREGATED MODEL ----
#------------------------------------------------------------------------------#

# MODEL 8: Disaggregated Model ----
growth_im_dis <- rblimp(
  data = growth_i,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (missing)',
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
    yw = y - (alpha + beta*time);
    lagyw = y.lag - (alpha + beta*(time - 1));
    m ~ intercept group yw lagyw alpha beta | intercept;
    { t in 1:4 } : m ~ (time == [t]) (time == [t])*group;',
  parameters = '
    diff = (((g0a+g1a)  + 4*(g0b+g1b)) - (g0a + 4*g0b)); 
    d_diff = diff / sqrt(y.totalvar + alpha.totalvar);',
  seed = 90291,
  chains = 4,
  burn = 200000,
  iter = 200000)

# print output
output(growth_im_dis)

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

mar_tab <- extract_growth_params(growth_im_mar, "MAR")
sp_tab  <- extract_growth_params(growth_im_sp, "SP")
spq_tab <- extract_growth_params(growth_im_spq, "SPQ")
spr_tab <- extract_growth_params(growth_im_spr, "SPR")
dk_tab  <- extract_growth_params(growth_im_dk, "DK")
dkq_tab <- extract_growth_params(growth_im_dkq, "DKQ")
dkd_tab <- extract_growth_params(growth_im_dkd, "DKD")
dis_tab <- extract_growth_params(growth_im_dis, "DIS")

tab <- cbind(mar_tab,sp_tab,spq_tab,spr_tab,dk_tab,dkq_tab,dkd_tab,dis_tab)
tab_growth_im <- tab
tab_growth_im

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
  MAR = nrow(growth_im_mar@iterations),
  SP  = nrow(growth_im_sp@iterations),
  SPQ = nrow(growth_im_spq@iterations),
  SPR = nrow(growth_im_spr@iterations),
  DK  = nrow(growth_im_dk@iterations),
  DKQ = nrow(growth_im_dkq@iterations),
  DKD = nrow(growth_im_dkd@iterations),
  DIS = nrow(growth_im_dis@iterations)
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

es_growth_im <- out
es_growth_im

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

conv_growth_im <- rbind(
  extract_convergence(growth_im_mar, "MAR"),
  extract_convergence(growth_im_sp, "SP"),
  extract_convergence(growth_im_spq, "SPQ"),
  extract_convergence(growth_im_spr, "SPR"),
  extract_convergence(growth_im_dk, "DK"),
  extract_convergence(growth_im_dkq, "DKQ"),
  extract_convergence(growth_im_dkd, "DKD"),
  extract_convergence(growth_im_dis, "DIS")
)

conv_growth_im
