library(cmdstanr)
library(here)
library(dplyr)
library(ggplot2)


set.seed(123)

d <- read.csv(file=here("chapter_5/data-shopping-1.csv"))

d$Income <- d$Income/100

N <- nrow(d)
#real data
data     <- list(N=N, Sex=d$Sex, Income=d$Income, Y=d$Y)

model <- cmdstan_model(stan_file='chapter_5/model5-3_exercise.stan')




