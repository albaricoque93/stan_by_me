data {
  int N;
  vector[N] Y;
}

parameters {
  real a;
  real<lower=0> sigma;
}

model {
  Y[1:N] ~ normal(a, sigma);
}
