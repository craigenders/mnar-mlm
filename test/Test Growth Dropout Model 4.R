#------------------------------------------------------------------------------#
# TEST: GROWTH DROPOUT MODEL 4 (RESIDUAL SHARED PARAMETER) AT 200K ----
#------------------------------------------------------------------------------#
#
# Table 2 and the FIN script list 100,000 iterations for Model 4, but the saved
# diagnostics from the FIN run (conv_growth_do_fi.csv) show 200,000. This fits
# the model once at 200,000 so the table row (mean diff -3.11, d -1.45, R2 .56)
# and the diagnostics can be checked directly.
#

options(scipen = 999)
library(rblimp)
set_blimp('/applications/blimp/blimp-nightly')
fdir::set()   # working directory = this script's folder (test/)

filepath <- 'https://raw.githubusercontent.com/craigenders/mnar-mlm/main/growth-dropout.csv'
growth_dropout <- read.csv(filepath, stringsAsFactors = T)

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
  burn = 100000,
  iter = 100000)

output(model4)

writeLines(output(model4), "growth_dropout_model4_200k_output.txt")

# key quantities for Table 2 and the convergence table
rows <- c("Parameter: diff", "Parameter: d_diff", "m R2: Coefficients")
tab2 <- round(model4@estimates[rows, c("Estimate", "StdDev")], 2)
neff <- model4@estimates[, "N_Eff"]; neff <- neff[is.finite(neff)]
psr  <- as.numeric(model4@psr[20, ]); psr <- psr[is.finite(psr)]
diag <- data.frame(Min_Neff = round(min(neff), 1), Max_PSR = round(max(psr), 3),
                   Iterations = nrow(model4@iterations))
print(tab2); print(diag)
write.csv(rbind(tab2, c(NA, NA)), "growth_dropout_model4_200k_table2.csv")
write.csv(diag, "growth_dropout_model4_200k_diag.csv", row.names = FALSE)
