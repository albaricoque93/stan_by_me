library(cmdstanr)
library(here)
set.seed(123)


N1 <- 30
N2 <- 20
Y1 <- rnorm(n=N1, mean=0, sd=5)
Y2 <- rnorm(n=N2, mean=1, sd=4)


# visualize the data ------------------------------------------------------

plot(density(Y2))
lines(density(Y1), col="red")

#assuming they have the same sd

data <- list(N=N1+N2, Y=c(Y1,Y2), X=c(rep(0,30), rep(1,20)))

model_equal <- cmdstan_model(stan_file= here("chapter_4/equal_model.stan"))

fit <- model_equal$sample(data=data, seed=123)

#obtain draws

draws <- fit$draws(format = "df")

#compute P(u1 < u2)

sum(draws$a < (draws$a + draws$b))/ nrow(draws)


# assuming they have a different mean -------------------------------------

data1 <- list(N=N1, Y=Y1)
data2 <- list(N=N2, Y=Y2)

model <- cmdstan_model(stan_file= here("chapter_4/different_model.stan"))

fit1 <- model$sample(data=data1, seed=123)
fit2 <- model$sample(data=data2, seed=123)

draws1 <- fit1$draws(format = "df")
draws2 <- fit2$draws(format = "df")


plot(density(draws$sigma))
lines(density(draws1$sigma), col="red")
lines(density(draws2$sigma), col="blue")

#compute P(u1 < u2)
diff1 <- sum(ifelse(draws1$a < draws2$a,1,0))/nrow(draws1)


