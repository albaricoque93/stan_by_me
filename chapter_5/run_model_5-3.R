library(cmdstanr)
library(here)
library(dplyr)
library(ggplot2)


set.seed(123)

d <- read.csv(file=here("chapter_5/data-shopping-1.csv"))

d$Income <- d$Income/100


# simulate data -----------------------------------------------------------


N <- nrow(d)
Y_sim_mean <- 0.2 + 0.15*d$Sex + 0.4*d$Income
Y_sim <- rnorm(N, mean=Y_sim_mean, sd=0.1)


data_sim <- list(N=N, Sex=d$Sex, Income=d$Income, Y=Y_sim)

#real data
data     <- list(N=N, Sex=d$Sex, Income=d$Income, Y=d$Y)


# fit model ---------------------------------------------------------------


model <- cmdstan_model(stan_file='chapter_5/model5-3.stan')



fit_sim <- model$sample(data=data_sim, seed=123, parallel_chains=4)
fit     <- model$sample(data=data,     seed=123, parallel_chains=4)
fit$save_object(file='chapter_5/result-model5-3.RDS')

pdf("chapter_5/plot.pdf")
plot(as_mcmc.list(fit_sim))
dev.off()


# posterior predictive checks ---------------------------------------------

yp_ms <- fit$draws('yp', format='matrix')

qua <- apply(yp_ms, 2, quantile, prob=c(0.1, 0.5, 0.9))


d_est <- data.frame(d, t(qua), check.names=FALSE) %>%
  mutate(Sex = as.factor(Sex))

p <- ggplot(data=d_est, aes(x=Y, y=`50%`, ymin=`10%`, ymax=`90%`, shape=Sex, fill=Sex)) +
  theme_bw(base_size=18) +
  theme(legend.key.height=grid::unit(2.5,'line')) +
  coord_fixed(ratio=1, xlim=c(0.28, .8), ylim=c(0.28, .8)) +
  geom_pointrange(size=0.8, color='gray5') +
  geom_abline(aes(slope=1, intercept=0), color='black', alpha=3/5, linetype='31') +
  scale_shape_manual(values=c(21, 24)) +
  scale_fill_manual(values=c('white', 'gray70')) +
  labs(x='Observed', y='Predicted') +
  scale_x_continuous(breaks=seq(from=0, to=1, by=0.1)) +
  scale_y_continuous(breaks=seq(from=0, to=1, by=0.1))


# posterior noise check ---------------------------------------------------

mu_ms <- fit$draws('mu', format='matrix')

N_ms <- nrow(mu_ms)

noise_ms <- t(replicate(N_ms, d$Y)) - mu_ms
# noise_ms <- einsum::einsum('n,m->mn', d$Y, rep(1, N_ms)) - mu_ms

d_est <- data.frame(noise_ms, check.names=FALSE) %>%
  tidyr::pivot_longer(cols=everything(), names_to='Parameter') %>%
  mutate(PersonID = readr::parse_number(Parameter))

d_mode <- apply(noise_ms, 2, function(x) {
  dens <- density(x)
  mode_i <- which.max(dens$y)
  mode_x <- dens$x[mode_i]
  mode_y <- dens$y[mode_i]
  c(mode_x, mode_y)
}) %>%
  t() %>%
  data.frame() %>%
  magrittr::set_colnames(c('X', 'Y'))

p <- ggplot() +
  theme_bw(base_size=18) +
  geom_line(data=d_est, aes(x=value, group=PersonID), stat='density', color='black', alpha=0.4) +
  geom_segment(data=d_mode, aes(x=X, xend=X, y=Y, yend=0), color='black', linetype='dashed', alpha=0.4) +
  geom_rug(data=d_mode, aes(x=X), sides='b') +
  labs(x='value', y='density')


s_dens <- density(fit$draws('sigma', format='matrix'))
s_MAP <- s_dens$x[which.max(s_dens$y)]
bw <- 0.01
p <- ggplot(data=d_mode, aes(x=X)) +
  theme_bw(base_size=18) +
  geom_histogram(aes(y=..density..), binwidth=bw, color='black', fill='white') +
  geom_density(color=NA, fill='gray20', alpha=0.5) +
  geom_rug(sides='b') +
  stat_function(fun=function(x) dnorm(x, mean=0, sd=s_MAP), linetype='dashed') +
  labs(x='value', y='density') +
  xlim(range(density(d_mode$X)$x))
