# dummy-coded time ----
cursio_2pl <- rblimp(
  data = intensive_i,
  clusterid = 'l2id', 
  # transform = 'm = ismissing(y)',
  ordinal = 'm',
  # timeid = 'time',
  # dropout = 'm = y (missing)',
  latent = 'l2id = alpha beta omega u',
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
    u ~ intercept;
    m ~ intercept@u;
    { t in 1:19 } : m ~ (time == [t]) (time == [t])*u;',
  seed = 90291,
  chains = 4,
  burn = 50000,
  iter = 50000)

# print output
output(cursio_2pl)