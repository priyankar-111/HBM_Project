
data {
  int<lower=1> N;
  int<lower=1> R;
  int<lower=1> K;
  int<lower=1> H;
  int<lower=1> J;
  int<lower=1> T;

  matrix[N,K] X;

  array[N] int<lower=1,upper=H> horse_id;
  array[N] int<lower=1,upper=J> jockey_id;
  array[N] int<lower=1,upper=T> trainer_id;

  array[R] int<lower=1,upper=N> start;
  array[R] int<lower=1,upper=N> end;
  array[R] int<lower=1,upper=N> winner;
}

parameters {
  vector[K] beta;

  vector[H] z_horse;
  vector[J] z_jockey;
  vector[T] z_trainer;

  real<lower=0> sigma_horse;
  real<lower=0> sigma_jockey;
  real<lower=0> sigma_trainer;
}

transformed parameters {
  vector[H] u_horse;
  vector[J] u_jockey;
  vector[T] u_trainer;

  vector[N] eta;

  u_horse = sigma_horse * z_horse;
  u_jockey = sigma_jockey * z_jockey;
  u_trainer = sigma_trainer * z_trainer;

  for (n in 1:N) {
    eta[n] =
      X[n] * beta
      + u_horse[horse_id[n]]
      + u_jockey[jockey_id[n]]
      + u_trainer[trainer_id[n]];
  }
}

model {
  // Priors
  beta ~ normal(0, 1);

  z_horse ~ std_normal();
  z_jockey ~ std_normal();
  z_trainer ~ std_normal();

  sigma_horse ~ normal(0, 1);
  sigma_jockey ~ normal(0, 1);
  sigma_trainer ~ normal(0, 1);

  // Race-wise softmax likelihood
  for (r in 1:R) {
    target +=
      eta[winner[r]]
      - log_sum_exp(eta[start[r]:end[r]]);
  }
}

generated quantities {
  vector[R] log_lik;

  for (r in 1:R) {
    log_lik[r] =
      eta[winner[r]]
      - log_sum_exp(eta[start[r]:end[r]]);
  }
}
