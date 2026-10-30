#------------------------------------------------------------------------------#
# TROUBLESHOOT: y.totalvar UNDER THE DROPOUT COMMAND ----
#------------------------------------------------------------------------------#
#
# The growth-dropout CMAR model gives d_diff = -1.52 when specified with
# timeid/dropout (old Rmd version) and -1.41 without them (FIN version), even
# though the mean difference and all focal-model parameters are identical.
# Fit both versions and compare the pieces of the standardizer.
#

options(scipen = 999)
library(rblimp)
set_blimp('/applications/blimp/blimp-nightly')
fdir::set()   # working directory = this script's folder (test/)

filepath <- 'https://raw.githubusercontent.com/craigenders/mnar-mlm/main/growth-dropout.csv'
growth_dropout <- read.csv(filepath, stringsAsFactors = T)

params <- '
  diff   = (((g0a+g1a) + 4*(g0b+g1b)) - (g0a + 4*g0b));
  d_diff = diff / sqrt(y.totalvar + alpha.totalvar);
  ytv    = y.totalvar;
  atv    = alpha.totalvar;
  sdpool = sqrt(y.totalvar + alpha.totalvar);'

focal <- '
  level2:
  alpha ~ intercept@g0a group@g1a;
  beta ~ intercept@g0b group@g1b;
  alpha ~~ beta;
  level1:
  y ~ intercept@alpha time@beta;'

# VERSION A: CMAR without timeid/dropout (FIN script) ----
cmar_A <- rblimp(
  data = growth_dropout,
  clusterid = 'l2id',
  latent = 'l2id = alpha beta',
  fixed = 'group time',
  model = focal,
  parameters = params,
  seed = 90291,
  burn = 25000,
  iter = 25000)

# VERSION B: same model with timeid/dropout added (old Rmd version) ----
cmar_B <- rblimp(
  data = growth_dropout,
  clusterid = 'l2id',
  timeid = 'time',
  dropout = 'm = y (monotone)',
  latent = 'l2id = alpha beta',
  fixed = 'group time',
  model = focal,
  parameters = params,
  seed = 90291,
  burn = 25000,
  iter = 25000)

# FULL BLIMP OUTPUTS TO TEXT ----
writeLines(output(cmar_A), "cmar_A_noDropout_output.txt")
writeLines(output(cmar_B), "cmar_B_withDropout_output.txt")

# COMPARISON TABLE ----
rows <- c("Parameter: diff", "Parameter: d_diff", "Parameter: ytv",
          "Parameter: atv", "Parameter: sdpool",
          "alpha ~ Intercept", "alpha ~ group", "beta ~ Intercept", "beta ~ group",
          "alpha residual variance", "beta residual variance", "y residual variance")
cmp <- data.frame(
  A_noDropout   = round(cmar_A@estimates[rows, "Estimate"], 3),
  B_withDropout = round(cmar_B@estimates[rows, "Estimate"], 3),
  rows_A = nrow(cmar_A@average_imp),
  rows_B = nrow(cmar_B@average_imp),
  row.names = rows)
write.csv(cmp, "cmar_totalvar_comparison.csv")
cmp
