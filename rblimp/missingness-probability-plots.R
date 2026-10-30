# Test file: time-trend missingness models and probability plots -------------
# Growth data with intermittent missingness (no dropout switch variables).
# Fits the CMAR growth model with linear, quadratic, and dummy-coded
# missingness models, then plots observed and predicted missingness
# probabilities by occasion and group. Short chains for testing.

library(rblimp)
library(ggplot2)
source('https://raw.githubusercontent.com/craigenders/mnar-mlm/main/rblimp/mnar-plotting.R')   # plot_means()
# set_blimp('/applications/blimp/blimp-nightly')

github <- 'https://raw.githubusercontent.com/craigenders/mnar-mlm/main/'
growth_intermittent <- read.csv(paste0(github, 'data/growth-intermittent.csv'), stringsAsFactors = T)

burn <- 5000   # tutorial uses 25000
iter <- 5000

# Linear time ------------------------------------------------------------------

time_linear <- rblimp(
  data = growth_intermittent,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (missing)',
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
    m ~ intercept time group time*group | intercept;',
  seed = 90291, burn = burn, iter = iter)

# Quadratic time ---------------------------------------------------------------

time_quadratic <- rblimp(
  data = growth_intermittent,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (missing)',
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
    m ~ intercept time time^2 group time*group time^2*group | intercept;',
  seed = 90291, burn = burn, iter = iter)

# Dummy-coded time -------------------------------------------------------------

time_dummy <- rblimp(
  data = growth_intermittent,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (missing)',
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
    m ~ intercept group | intercept;
    { t in 1:4 } : m ~ (time == [t]) (time == [t])*group;',
  seed = 90291, burn = burn, iter = iter)

# Probability plots ------------------------------------------------------------
# plot_means() (from mnar-plotting.R) averages a column of model@average_imp by
# time and group and draws one line per group. m is the observed indicator, so
# its mean is the observed missing data rate. m.1.probability is the predicted
# probability that m = 1 for each row, so its mean is the average predicted
# probability.

ymin <- 0
ymax <- .35

plot_means(m ~ time | group, model = time_dummy,                                   # Figure 3A
    ylab = "Probability", title = "Observed Probabilities",
    group_labels = c("0" = "0", "1" = "1")) + ylim(ymin, ymax) +
    theme(legend.position = "top") + scale_linetype_manual(values = c("dashed", "solid"))

plot_means(m.1.probability ~ time | group, model = time_linear,                    # Figure 3C
    ylab = "Probability", title = "Linear Time",
    group_labels = c("0" = "0", "1" = "1")) + ylim(ymin, ymax) +
    theme(legend.position = "top") + scale_linetype_manual(values = c("dashed", "solid"))

plot_means(m.1.probability ~ time | group, model = time_quadratic,                 # Figure 3E
    ylab = "Probability", title = "Quadratic Time",
    group_labels = c("0" = "0", "1" = "1")) + ylim(ymin, ymax) +
    theme(legend.position = "top") + scale_linetype_manual(values = c("dashed", "solid"))

plot_means(m.1.probability ~ time | group, model = time_dummy,                     # Figure 3G
    ylab = "Probability", title = "Dummy-Coded Time",
    group_labels = c("0" = "0", "1" = "1")) + ylim(ymin, ymax) +
    theme(legend.position = "top") + scale_linetype_manual(values = c("dashed", "solid"))

# Fit summary: predicted minus observed, by specification ----------------------

obs  <- aggregate(m ~ time + group, data = time_dummy@average_imp, mean)
pred <- list(Linear    = aggregate(m.1.probability ~ time + group, data = time_linear@average_imp,    mean),
             Quadratic = aggregate(m.1.probability ~ time + group, data = time_quadratic@average_imp, mean),
             Dummy     = aggregate(m.1.probability ~ time + group, data = time_dummy@average_imp,     mean))
dev <- lapply(pred, function(p) p$m.1.probability - obs$m)
round(data.frame(RMSE = sapply(dev, function(x) sqrt(mean(x^2))),
                 Min = sapply(dev, min), Mean = sapply(dev, mean), Max = sapply(dev, max)), 3)
