
data {
  int<lower=1> N;       // Horse-race observations
  int<lower=1> R;       // Number of races
  int<lower=1> K;       // Number of fixed effects

  int<lower=1> H;       // Number of horses
  int<lower=1> J;       // Number of jockeys
  int<lower=1> T;       // Number of trainers

  matrix[N, K] X;

  array[N] int<lower=1, upper=H> horse_id;
  array[N] int<lower=1, upper=J> jockey_id;
  array[N] int<lower=1, upper=T> trainer_id;

  array[R] int<lower=1, upper=N> start;
  array[R] int<lower=1, upper=N> end;
  array[R] int<lower=1, upper=N> winner;
}

parameters {
  vector[K] beta;

  // Non-centred random effects
  vector[H] z_horse;
  vector[J] z_jockey;
  vector[T] z_trainer;

  // Random-effect standard deviations
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
  // Priors for fixed effects
  beta ~ normal(0, 1);

  // Standard normal latent random effects
  z_horse ~ std_normal();
  z_jockey ~ std_normal();
  z_trainer ~ std_normal();

  // Half-normal priors on standard deviations
  sigma_horse ~ normal(0, 1);
  sigma_jockey ~ normal(0, 1);
  sigma_trainer ~ normal(0, 1);

  // Race-level conditional-choice likelihood
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

