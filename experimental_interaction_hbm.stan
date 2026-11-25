  
data {
  int<lower=1> N;       // Horse-race observations
  int<lower=1> R;       // Number of races
  int<lower=1> K;       // Number of fixed effects

  int<lower=1> H;       // Horses
  int<lower=1> J;       // Jockeys
  int<lower=1> T;       // Trainers

  int<lower=1> NHJ;     // Observed horse-jockey pairs
  int<lower=1> NHT;     // Observed horse-trainer pairs
  int<lower=1> NJT;     // Observed jockey-trainer pairs

  matrix[N, K] X;

  array[N] int<lower=1, upper=H> horse_id;
  array[N] int<lower=1, upper=J> jockey_id;
  array[N] int<lower=1, upper=T> trainer_id;

  // Pair indices
  array[N] int<lower=1, upper=NHJ> hj_id;
  array[N] int<lower=1, upper=NHT> ht_id;
  array[N] int<lower=1, upper=NJT> jt_id;

  array[R] int<lower=1, upper=N> start;
  array[R] int<lower=1, upper=N> end;
  array[R] int<lower=1, upper=N> winner;
}

parameters {
  // Correlated fixed effects
  vector[K] beta_raw;
  cholesky_factor_corr[K] L_beta;

  // Main participant effects: non-centred
  vector[H] z_horse;
  vector[J] z_jockey;
  vector[T] z_trainer;

  // Pairwise interaction effects: non-centred
  vector[NHJ] z_hj;
  vector[NHT] z_ht;
  vector[NJT] z_jt;

  // Variance parameters
  real<lower=0> var_horse;
  real<lower=0> var_jockey;
  real<lower=0> var_trainer;

  real<lower=0> var_hj;
  real<lower=0> var_ht;
  real<lower=0> var_jt;
}

transformed parameters {
  vector[K] beta;

  vector[H] u_horse;
  vector[J] u_jockey;
  vector[T] u_trainer;

  vector[NHJ] w_hj;
  vector[NHT] a_ht;
  vector[NJT] b_jt;

  vector[N] eta;

  // Correlated multivariate Normal prior structure
  beta = L_beta * beta_raw;

  // Main participant effects
  u_horse = sqrt(var_horse) * z_horse;
  u_jockey = sqrt(var_jockey) * z_jockey;
  u_trainer = sqrt(var_trainer) * z_trainer;

  // Pairwise interactions
  w_hj = sqrt(var_hj) * z_hj;
  a_ht = sqrt(var_ht) * z_ht;
  b_jt = sqrt(var_jt) * z_jt;

  // Linear predictor
  for (n in 1:N) {
    eta[n] =
      X[n] * beta
      + u_horse[horse_id[n]]
      + u_jockey[jockey_id[n]]
      + u_trainer[trainer_id[n]]
      + w_hj[hj_id[n]]
      + a_ht[ht_id[n]]
      + b_jt[jt_id[n]];
  }
}

model {
  // Fixed-effect prior
  beta_raw ~ std_normal();
  L_beta ~ lkj_corr_cholesky(2);

  // Main participant latent effects
  z_horse ~ std_normal();
  z_jockey ~ std_normal();
  z_trainer ~ std_normal();

  // Interaction latent effects
  z_hj ~ std_normal();
  z_ht ~ std_normal();
  z_jt ~ std_normal();

  // Inverse-Gamma priors on variances
  // Provisional calibration: E[var] = 1
  var_horse ~ inv_gamma(3, 2);
  var_jockey ~ inv_gamma(3, 2);
  var_trainer ~ inv_gamma(3, 2);

  var_hj ~ inv_gamma(3, 2);
  var_ht ~ inv_gamma(3, 2);
  var_jt ~ inv_gamma(3, 2);

  // Race-level conditional-choice likelihood
  for (r in 1:R) {
    target +=
      eta[winner[r]]
      - log_sum_exp(eta[start[r]:end[r]]);
  }
}

generated quantities {
  vector[R] log_lik;
  
  vector[6] sigma_group;

  sigma_group[1] = sqrt(var_horse);
  sigma_group[2] = sqrt(var_jockey);
  sigma_group[3] = sqrt(var_trainer);
  sigma_group[4] = sqrt(var_hj);
  sigma_group[5] = sqrt(var_ht);
  sigma_group[6] = sqrt(var_jt);

  for (r in 1:R) {
    log_lik[r] =
      eta[winner[r]]
      - log_sum_exp(eta[start[r]:end[r]]);
  }
}


