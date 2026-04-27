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
growth_dropout <- read.csv(filepath1, stringsAsFactors = T)

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')
source('https://raw.githubusercontent.com/craigenders/mnar-mlm/main/mnar-plotting.R')

#------------------------------------------------------------------------------#
# CMAR ----
#------------------------------------------------------------------------------#

# MODEL 1: CMAR ----
model1 <- rblimp(
  data = growth_dropout,
  clusterid = 'l2id', 
  latent = 'l2id = alpha beta',
  fixed = 'group time',
  model = '
    level2:
    alpha ~ intercept@g0a group@g1a;
    beta ~ intercept@g0b group@g1b;
    alpha ~~ beta;
    level1:
    y ~ intercept@alpha time@beta;',
  parameters = '
    diff = (((g0a+g1a)  + 4*(g0b+g1b)) - (g0a + 4*g0b)); 
    d_diff = diff / sqrt(y.totalvar + alpha.totalvar);',
  seed = 90291,
  chains = 4,
  burn = 25000,
  iter = 25000)

# print output
output(model1)

#------------------------------------------------------------------------------#
# TIME-RELATED CHANGES ----
#------------------------------------------------------------------------------#

# MISSINGNESS PROBS: Linear Time ----
time_linear <- rblimp(
  data = growth_dropout,
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
output(time_linear)

# MISSINGNESS PROBS: Quadratic Time ----
time_quadratic <- rblimp(
  data = growth_dropout,
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
output(time_quadratic)

# MISSINGNESS PROBS: Dummy-Coded Time ----
time_dummy <- rblimp(
  data = growth_dropout,
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
output(time_dummy)

#------------------------------------------------------------------------------#
# PLOT MISSINGNESS PROBABILITIES ----
#------------------------------------------------------------------------------#

ymax <- .5
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
    title = "Dummy-Coded Time",
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
# SHARED PARAMETER MODEL ----
#------------------------------------------------------------------------------#

# MODEL 2: Shared Parameter Model ----
model2 <- rblimp(
  data = growth_dropout,
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
output(model2)

# MODEL 3: Quadratic Shared Parameter Model ----
model3 <- rblimp(
  data = growth_dropout,
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
output(model3)

# MODEL 4: Residual Shared Parameter Model ----
model4 <- rblimp(
  data = growth_dropout,
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
output(model4)

#------------------------------------------------------------------------------#
# SELECTION MODEL ----
#------------------------------------------------------------------------------#

# MODEL 5: Diggle-Kenward Selection Model ----
model5 <- rblimp(
  data = growth_dropout,
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
output(model5)

# MODEL 6: Quadratic Diggle-Kenward Model ----
model6 <- rblimp(
  data = growth_dropout,
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
output(model6)

# MODEL 7: Residual Diggle-Kenward Model ----
model7 <- rblimp(
  data = growth_dropout,
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
output(model7)

#------------------------------------------------------------------------------#
# DISAGGREGATED MODEL ----
#------------------------------------------------------------------------------#

# MODEL 8: Disaggregated Model ----
model8 <- rblimp(
  data = growth_dropout,
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
output(model8)

#------------------------------------------------------------------------------#
# EXTRACT ESTIMATES ----
#------------------------------------------------------------------------------#

# function to extract key estimates
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
  
  res <- do.call(rbind, lapply(rows, function(r) {
    if (r %in% rownames(tab)) {
      round(tab[r, c("Estimate", "StdDev"), drop = FALSE], 2)
    } else {
      data.frame(Estimate = NA_real_, StdDev = NA_real_, row.names = r)
    }
  }))
  
  rownames(res) <- c(
    "Icept (G = 0)",
    "Slope (G = 0)",
    "Icept Diff.",
    "Slope Diff.",
    "Var(Icept)",
    "Var(Slope)",
    "Cor(Icept, Slope)",
    "Var(Residual)",
    "Mean Diff.",
    "Std. Diff.",
    "Pseudo-Rsq"
  )
  
  colnames(res) <- c(
    paste0("Est_", method),
    paste0("SE_", method)
  )
  
  res
}

# main summary table ----
table_summary <- cbind(
  extract_growth_params(model1, "Mod1"),
  extract_growth_params(model2, "Mod2"),
  extract_growth_params(model3, "Mod3"),
  extract_growth_params(model4, "Mod4"),
  extract_growth_params(model5, "Mod5"),
  extract_growth_params(model6, "Mod6"),
  extract_growth_params(model7, "Mod7"),
  extract_growth_params(model8, "Mod8")
)
table_summary

# mean difference table ----

# extract rows
mean_row <- table_summary["Mean Diff.", ]
std_row  <- table_summary["Std. Diff.", ]

# method names
methods_short <- paste0("M", 1:8)
methods_long  <- paste0("Model ", 1:8)

# build table
table_diff <- do.call(rbind, lapply(seq_along(methods_short), function(i) {
  m <- methods_short[i]
  c(
    Mean_Diff     = unname(mean_row[paste0("Est_", m)]),
    SE_Mean_Diff  = unname(mean_row[paste0("SE_",  m)]),
    Std_Mean_Diff = unname(std_row[paste0("Est_",  m)]),
    SE_Std_Mean   = unname(std_row[paste0("SE_",   m)])
  )
}))
rownames(table_diff) <- methods_long
table_diff <- as.data.frame(table_diff)
table_diff

# changes in SE units ----

est_cmar <- table_summary[, 1]
se_cmar  <- table_summary[, 2]

compare_methods <- c("Mod2", "Mod3", "Mod5", "Mod6", "Mod7", "Mod8")

table_change <- sapply(compare_methods, function(m) {
  round((table_summary[, paste0("Est_", m)] - est_cmar) / se_cmar, 2)
})
table_change <- as.data.frame(table_change)
rownames(table_change) <- rownames(table_summary)
table_change

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
  extract_convergence(model1, "Model 1"),
  extract_convergence(model2, "Model 2"),
  extract_convergence(model3, "Model 3"),
  extract_convergence(model4, "Model 4"),
  extract_convergence(model5, "Model 5"),
  extract_convergence(model6, "Model 6"),
  extract_convergence(model7, "Model 7"),
  extract_convergence(model8, "Model 8")
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
  nrow(model8@iterations)
)
table_diag

