library(cmdstanr)
library(here)

Np <- 50

# Prediction predictor values (Xp)
Xp <- rnorm(Np, mean = 0, sd = 1)

d <- read.csv(file='chapter_4/data-salary.csv')
data <- list(N=nrow(d), X=d$X, Y=d$Y, Np=Np, Xp=Xp)

model <- cmdstan_model(stan_file= here("chapter_4/model_b.stan"))


fit <- model$sample(data=data, seed=123)

fit$save_object(file='./chapter_4/result-model4-4.RDS')


write.table(fit$summary(), file='./chapter_4/fit-summary.csv',
            sep=',', quote=TRUE, row.names=FALSE)

library(coda)

pdf(file='./chapter_4/fit-plot.pdf')
plot(as_mcmc.list(fit))
dev.off()

d_ms <- fit$draws(format = "df")

plot(d$X, d$Y)
points(15, quantile( d_ms$`y_base[15]`, probs = 0.9), col="red")
points(15, quantile( d_ms$`y_base[15]`, probs = 0.1), col="red")


N_ms <- nrow(d_ms)
y10_base = d_ms$a + d_ms$b * 15
y10_pred <- rnorm(n=N_ms, mean=y10_base, sd=d_ms$sigma)


plot(d$X, d$Y)
points(15, quantile(y10_pred, probs = 0.9), col="red")
points(15, quantile(y10_pred, probs = 0.1), col="red")

