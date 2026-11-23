install.packages(c("tidyverse", "janitor"))
library(tidyverse)
library(janitor)

runs <- read.csv("C:/Users/priya/Downloads/runs.csv")

# Basic structure
dim(runs)
names(runs)
glimpse(runs)
summary(runs)
#Checking data structure
race_audit <- runs %>%
  group_by(race_id) %>%
  summarise(
    n_horses = n(),
    n_winners = sum(won == 1, na.rm = TRUE),
    n_first = sum(result == 1, na.rm = TRUE),
    .groups = "drop"
  )

# Distribution of horses per race
race_audit %>%
  count(n_horses, sort = TRUE)

# Check winner counts
race_audit %>%
  count(n_winners)

# Races with unusual winner counts
race_audit %>%
  filter(n_winners != 1)
#Checking repeated observation and identifiers

# Number of unique entities
runs %>%
  summarise(
    races = n_distinct(race_id),
    horses = n_distinct(horse_id),
    jockeys = n_distinct(jockey_id),
    trainers = n_distinct(trainer_id)
  )

# How many races has each horse participated in?
horse_frequency <- runs %>%
  count(horse_id, name = "n_races")
horse_frequency

summary(horse_frequency$n_races)

# Jockey and trainer participation
jockey_frequency <- runs %>%
  count(jockey_id, name = "n_races")

trainer_frequency <- runs %>%
  count(trainer_id, name = "n_races")

summary(jockey_frequency$n_races)
summary(trainer_frequency$n_races)
#missing values
missing_summary <- runs %>%
  summarise(
    across(
      everything(),
      ~ sum(is.na(.))
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing_count"
  ) %>%
  mutate(
    missing_percent = 100 * missing_count / nrow(runs)
  ) %>%
  arrange(desc(missing_percent))

missing_summary %>%
  filter(missing_count > 0)
# Outcome distribution
runs %>%
  count(won)

# Finishing position
runs %>%
  count(result, sort = TRUE)

# Horse rating
ggplot(runs, aes(x = horse_rating)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribution of Horse Ratings",
    x = "Horse rating",
    y = "Number of observations"
  ) +
  theme_minimal()

# Win odds
ggplot(runs, aes(x = win_odds)) +
  geom_histogram(bins = 40) +
  labs(
    title = "Distribution of Win Odds",
    x = "Win odds",
    y = "Number of observations"
  ) +
  theme_minimal()

# Jockey frequency distribution
ggplot(jockey_frequency, aes(x = n_races)) +
  geom_histogram(bins = 40) +
  labs(
    title = "Distribution of Jockey Participation",
    x = "Number of race appearances",
    y = "Number of jockeys"
  ) +
  theme_minimal()

# Trainer frequency distribution
ggplot(trainer_frequency, aes(x = n_races)) +
  geom_histogram(bins = 40) +
  labs(
    title = "Distribution of Trainer Participation",
    x = "Number of race appearances",
    y = "Number of trainers"
  ) +
  theme_minimal()
#log transforms
ggplot(jockey_frequency, aes(x = log1p(n_races))) +
  geom_histogram(bins = 30) +
  labs(
    title = "Log Participation Frequency: Jockeys",
    x = "log(1 + number of appearances)",
    y = "Number of jockeys"
  ) +
  theme_minimal()
ggplot(trainer_frequency, aes(x = log1p(n_races))) +
  geom_histogram(bins = 30) +
  labs(
    title = "Log Participation Frequency: Jockeys",
    x = "log(1 + number of appearances)",
    y = "Number of trainerss"
  ) +
  theme_minimal()

jockey_frequency %>%
  summarise(
    total = n(),
    appearances_1_to_5 = sum(n_races <= 5),
    percentage_1_to_5 = mean(n_races <= 5) * 100
  )

trainer_frequency %>%
  summarise(
    total = n(),
    appearances_1_to_5 = sum(n_races <= 5),
    percentage_1_to_5 = mean(n_races <= 5) * 100
  )

section_audit <- runs %>%
  group_by(race_id) %>%
  summarise(
    horses = n(),
    missing_time4 = sum(is.na(time4)),
    missing_time5 = sum(is.na(time5)),
    missing_time6 = sum(is.na(time6)),
    .groups = "drop"
  )

section_audit %>%
  summarise(
    time4_inconsistent = sum(
      missing_time4 > 0 & missing_time4 < horses
    ),
    time5_inconsistent = sum(
      missing_time5 > 0 & missing_time5 < horses
    ),
    time6_inconsistent = sum(
      missing_time6 > 0 & missing_time6 < horses
    )
  )

runs %>%
  mutate(
    place_odds_missing = is.na(place_odds)
  ) %>%
  group_by(won) %>%
  summarise(
    total = n(),
    missing = sum(place_odds_missing),
    missing_percentage = mean(place_odds_missing) * 100,
    .groups = "drop"
  )
# Categorical variables
runs %>%
  count(horse_age, sort = TRUE)

runs %>%
  count(horse_country, sort = TRUE)

runs %>%
  count(horse_type, sort = TRUE)

# Numerical variables
runs %>%
  select(
    horse_rating,
    declared_weight,
    actual_weight,
    draw,
    win_odds
  ) %>%
  summary()

runs %>%
  group_by(horse_rating) %>%
  summarise(
    total = n(),
    wins = sum(won == 1),
    win_rate = mean(won),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = horse_rating, y = win_rate)) +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    title = "Horse Rating and Observed Winning Rate",
    x = "Horse rating",
    y = "Observed winning rate"
  ) +
  theme_minimal()
runs %>%
  mutate(
    odds_group = cut(
      win_odds,
      breaks = c(0, 5, 10, 20, 40, 60, 100),
      include.lowest = TRUE
    )
  ) %>%
  group_by(odds_group) %>%
  summarise(
    horses = n(),
    wins = sum(won),
    win_rate = mean(won),
    .groups = "drop"
  )
#correlation matrix
numeric_data <- runs %>%
  select(
    horse_rating,
    horse_age,
    declared_weight,
    actual_weight,
    draw,
    win_odds
  )

cor(numeric_data, use = "pairwise.complete.obs")

ggplot(runs, aes(x = factor(horse_age),
                 y = horse_rating)) +
  geom_boxplot() +
  labs(
    x = "Horse age",
    y = "Horse rating",
    title = "Horse Rating by Age"
  ) +
  theme_minimal()
ggplot(runs, aes(x = declared_weight,
                 y = actual_weight)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm") +
  theme_minimal()
ggplot(runs, aes(x = horse_rating,
                 y = log(win_odds))) +
  geom_point(alpha = 0.15) +
  geom_smooth(method = "loess") +
  labs(
    x = "Horse rating",
    y = "Log win odds",
    title = "Horse Rating and Betting Odds"
  ) +
  theme_minimal()

#Start of model

library(tidyverse)

model_data <- runs %>%
  mutate(
    age_group = case_when(
      horse_age == 2 ~ "2",
      horse_age == 3 ~ "3",
      horse_age == 4 ~ "4",
      horse_age == 5 ~ "5",
      horse_age >= 6 ~ "6+"
    ),
    age_group = factor(age_group),
    horse_country = fct_lump_min(
      factor(horse_country),
      min = 100
    ),
    draw = factor(draw)
  ) %>%
  filter(
    !is.na(won),
    !is.na(horse_rating),
    !is.na(age_group),
    !is.na(horse_country),
    !is.na(declared_weight),
    !is.na(actual_weight),
    !is.na(draw)
  )
#M1
M1 <- glm(
  won ~ horse_rating +
    age_group +
    horse_country +
    declared_weight +
    actual_weight +
    draw,
  data = model_data,
  family = binomial()
)

summary(M1)
#M2
M2 <- glm(
  won ~ horse_rating +
    age_group +
    horse_country +
    declared_weight +
    actual_weight +
    draw +
    log(win_odds),
  data = model_data,
  family = binomial()
)
summary(M2)

model_data %>%
  group_by(draw) %>%
  summarise(
    appearances = n(),
    wins = sum(won),
    win_rate = mean(won),
    .groups = "drop"
  )
exp(cbind(
  OR = coef(M1),
  confint.default(M1)
))
exp(cbind(
  OR = coef(M2),
  confint.default(M2)
))
#Comparison of models
# Confirm both models use the same observations
nobs(M1)
nobs(M2)

# Compare nested models
anova(M1, M2, test = "Chisq")
#Checking for draw15
runs %>%
  filter(draw == 15) %>%
  select(
    race_id, horse_id, horse_no,
    result, won, draw,
    horse_rating, win_odds
  )
#
runs %>%
  filter(race_id == 5188) %>%
  arrange(draw) %>%
  select(
    race_id, horse_id, horse_no,
    result, draw, horse_rating, win_odds
  )
#Prepare data for random effects model
hb_data <- runs %>%
  mutate(
    age_group = case_when(
      horse_age == 2 ~ "2",
      horse_age == 3 ~ "3",
      horse_age == 4 ~ "4",
      horse_age == 5 ~ "5",
      horse_age >= 6 ~ "6+"
    ),
    
    age_group = factor(age_group),
    
    horse_country = forcats::fct_lump_min(
      factor(horse_country),
      min = 100
    ),
    
    draw_group = if_else(draw >= 14, "14+", as.character(draw)),
    draw_group = factor(draw_group)
  ) %>%
  filter(
    !is.na(won),
    !is.na(horse_rating),
    !is.na(age_group),
    !is.na(horse_country),
    !is.na(declared_weight),
    !is.na(actual_weight),
    !is.na(draw_group),
    !is.na(horse_id),
    !is.na(jockey_id),
    !is.na(trainer_id)
  ) %>%
  mutate(
    horse_id = factor(horse_id),
    jockey_id = factor(jockey_id),
    trainer_id = factor(trainer_id)
  )
install.packages("lme4")
library(lme4)
library(dplyr)
#Horse Random Effects
H1 <- glmer(
  won ~ horse_rating +
    age_group +
    horse_country +
    declared_weight +
    actual_weight +
    draw_group +
    (1 | horse_id),
  data = hb_data,
  family = binomial(),
  control = glmerControl(
    optimizer = "bobyqa"
  )
)
#Horse and Jockey Random effects
H2 <- glmer(
  won ~ horse_rating +
    age_group +
    horse_country +
    declared_weight +
    actual_weight +
    draw_group +
    (1 | horse_id) +
    (1 | jockey_id),
  data = hb_data,
  family = binomial(),
  control = glmerControl(
    optimizer = "bobyqa"
  )
)
#Horse,Jockey and Trainer Random effects
H3 <- glmer(
  won ~ horse_rating +
    age_group +
    horse_country +
    declared_weight +
    actual_weight +
    draw_group +
    (1 | horse_id) +
    (1 | jockey_id) +
    (1 | trainer_id),
  data = hb_data,
  family = binomial(),
  control = glmerControl(
    optimizer = "bobyqa"
  )
)
summary(H1)
summary(H2)
summary(H3)

VarCorr(H1)
VarCorr(H2)
VarCorr(H3)

AIC(H1, H2, H3)
#Refit H3 for convergence
ctrl <- glmerControl(
  optimizer = "bobyqa",
  optCtrl = list(maxfun = 200000)
)
H3_refit <- glmer(
  won ~ horse_rating +
    age_group +
    horse_country +
    declared_weight +
    actual_weight +
    draw_group +
    (1 | horse_id) +
    (1 | jockey_id) +
    (1 | trainer_id),
  data = hb_data,
  family = binomial(),
  control = ctrl
)

summary(H3_refit)
isSingular(H3_refit)
VarCorr(H3_refit)
#Same result as H3
#H4

library(dplyr)

hb_data <- hb_data %>%
  group_by(horse_id) %>%
  mutate(
    rating_mean = mean(horse_rating, na.rm = TRUE),
    rating_within = horse_rating - rating_mean
  ) %>%
  ungroup()

H4 <- glmer(
  won ~ rating_within + rating_mean +
    age_group + horse_country +
    declared_weight + actual_weight + draw_group +
    (1 | horse_id) +
    (1 | jockey_id) +
    (1 | trainer_id),
  data = hb_data,
  family = binomial(),
  control = ctrl
)

summary(H4)
isSingular(H4)
VarCorr(H4)

hb_data %>%
  summarise(
    mean_rating_within = mean(rating_within),
    sd_rating_within = sd(rating_within),
    mean_rating = mean(horse_rating),
    sd_rating = sd(horse_rating)
  )

hb_data %>%
  group_by(won) %>%
  summarise(
    mean_within = mean(rating_within),
    median_within = median(rating_within),
    mean_rating = mean(horse_rating),
    n = n()
  )
anova(H3_refit, H4, test = "Chisq")

library(dplyr)

rating_history <- hb_data %>%
  group_by(horse_id) %>%
  summarise(
    appearances = n(),
    mean_rating = mean(horse_rating),
    sd_rating = sd(horse_rating),
    min_rating = min(horse_rating),
    max_rating = max(horse_rating),
    wins = sum(won),
    win_rate = mean(won),
    .groups = "drop"
  )

summary(rating_history)

# Compare horses with different rating variability
rating_history %>%
  summarise(
    mean_sd = mean(sd_rating, na.rm = TRUE),
    median_sd = median(sd_rating, na.rm = TRUE),
    horses_with_constant_rating = sum(sd_rating == 0, na.rm = TRUE),
    horses_with_variable_rating = sum(sd_rating > 0, na.rm = TRUE)
  )
#Analysis on rating factor 

rating_history %>%
  mutate(
    rating_status = case_when(
      is.na(sd_rating) ~ "Missing",
      sd_rating == 0 ~ "Constant",
      sd_rating > 0 ~ "Variable"
    ),
    appearance_group = cut(
      appearances,
      breaks = c(0, 5, 10, 20, 40, Inf),
      labels = c("1-5", "6-10", "11-20",
                 "21-40", "40+")
    )
  ) %>%
  count(appearance_group, rating_status) %>%
  group_by(appearance_group) %>%
  mutate(percentage = 100 * n / sum(n)) %>%
  ungroup()

#Examine the within-rating relationship among variable-rating horses

variable_horses <- rating_history %>%
  filter(sd_rating > 0) %>%
  pull(horse_id)


hb_variable <- hb_data %>%
  filter(horse_id %in% variable_horses)

H4_variable <- glmer(
  won ~ rating_within + rating_mean +
    age_group + horse_country +
    declared_weight + actual_weight + draw_group +
    (1 | horse_id) +
    (1 | jockey_id) +
    (1 | trainer_id),
  data = hb_variable,
  family = binomial(),
  control = ctrl
)

summary(H4_variable)


hb_data %>%
  group_by(won) %>%
  summarise(
    mean_within = mean(rating_within),
    sd_within = sd(rating_within),
    mean_rating = mean(horse_rating),
    mean_rating_sd = mean(rating_mean),
    .groups = "drop"
  )
isSingular(H4_variable)
VarCorr(H4_variable)
hb_variable %>%
  summarise(
    horses = n_distinct(horse_id),
    jockeys = n_distinct(jockey_id),
    trainers = n_distinct(trainer_id),
    observations = n()
  )

hb_variable %>%
  count(trainer_id, sort = TRUE)
names(runs)
#examine the race structure before building the Bayesian model
race_structure <- runs %>%
  group_by(race_id) %>%
  summarise(
    field_size = n(),
    winners = sum(won),
    .groups = "drop"
  )

table(race_structure$field_size)
table(race_structure$winners)

dead_heat_races <- race_structure %>%
  filter(winners > 1)

dead_heat_races

runs %>%
  filter(race_id %in% dead_heat_races$race_id) %>%
  filter(won == 1) %>%
  select(race_id, horse_id, horse_no, result,
         horse_rating, win_odds)

runs %>%
  group_by(race_id) %>%
  summarise(
    horses = n_distinct(horse_id),
    observations = n(),
    .groups = "drop"
  ) %>%
  filter(horses != observations)
#Preparing data for Bayes incorporation
library(dplyr)

# Retain races with exactly one recorded winner
single_winner_races <- race_structure %>%
  filter(winners == 1) %>%
  pull(race_id)

choice_data <- runs %>%
  filter(race_id %in% single_winner_races) %>%
  arrange(race_id, horse_no) %>%
  mutate(global_row = row_number()) %>%
  group_by(race_id) %>%
  mutate(
    race_index = cur_group_id(),
    row_in_race = row_number(),
    winner_local = which(won == 1)
  ) %>%
  ungroup()

# One record per race: start, end and winner's row
race_index_data <- choice_data %>%
  group_by(race_index) %>%
  summarise(
    race_id = first(race_id),
    start = min(global_row),
    end = max(global_row),
    winner_row = global_row[which(won == 1)],
    field_size = n(),
    .groups = "drop"
  )

# Verify
nrow(race_index_data)
sum(race_index_data$field_size)
table(race_index_data$field_size)

stopifnot(
  all(race_index_data$winner_row >= race_index_data$start),
  all(race_index_data$winner_row <= race_index_data$end),
  all(choice_data$won %in% c(0, 1))
)
#Preparation of model variables
choice_data <- choice_data %>%
  mutate(
    age_group = case_when(
      horse_age == 2 ~ "2",
      horse_age == 3 ~ "3",
      horse_age == 4 ~ "4",
      horse_age == 5 ~ "5",
      horse_age >= 6 ~ "6+",
      TRUE ~ NA_character_
    ),
    
    horse_country = forcats::fct_lump_min(
      factor(horse_country), min = 100
    ),
    
    draw_group = if_else(
      draw >= 14, "14+", as.character(draw)
    ),
    
    horse_id = as.integer(factor(horse_id)),
    jockey_id = as.integer(factor(jockey_id)),
    trainer_id = as.integer(factor(trainer_id)),
    
    age_group = factor(age_group),
    draw_group = factor(draw_group)
  )

# Check missing values
colSums(is.na(choice_data %>%
                select(won, horse_rating, age_group, horse_country,
                       declared_weight, actual_weight, draw_group,
                       horse_id, jockey_id, trainer_id)))

# Number of unique groups
choice_data %>%
  summarise(
    horses = n_distinct(horse_id),
    jockeys = n_distinct(jockey_id),
    trainers = n_distinct(trainer_id),
    races = n_distinct(race_id)
  )
#prepare the Stan data
choice_data <- choice_data %>%
  mutate(
    rating_z = as.numeric(scale(horse_rating)),
    declared_weight_z = as.numeric(scale(declared_weight)),
    actual_weight_z = as.numeric(scale(actual_weight))
  )
X <- model.matrix(
  ~ rating_z + age_group + horse_country +
    declared_weight_z + actual_weight_z + draw_group,
  data = choice_data
)[, -1, drop = FALSE]

dim(X)
colnames(X)
stan_data <- list(
  N = nrow(choice_data),
  R = nrow(race_index_data),
  K = ncol(X),
  
  H = max(choice_data$horse_id),
  J = max(choice_data$jockey_id),
  T = max(choice_data$trainer_id),
  
  X = X,
  
  horse_id = as.integer(choice_data$horse_id),
  jockey_id = as.integer(choice_data$jockey_id),
  trainer_id = as.integer(choice_data$trainer_id),
  
  start = as.integer(race_index_data$start),
  end = as.integer(race_index_data$end),
  winner = as.integer(race_index_data$winner_row)
)
str(stan_data)

stopifnot(
  stan_data$N == 79295,
  stan_data$R == 6336,
  stan_data$H == 4405,
  stan_data$J == 186,
  stan_data$T == 176,
  
  all(stan_data$start >= 1),
  all(stan_data$end <= stan_data$N),
  all(stan_data$winner >= stan_data$start),
  all(stan_data$winner <= stan_data$end),
  
  all(stan_data$end >= stan_data$start),
  all(diff(stan_data$start) > 0)
)
stopifnot(
  stan_data$start[1] == 1,
  stan_data$end[length(stan_data$end)] == stan_data$N,
  all(stan_data$start[-1] == stan_data$end[-length(stan_data$end)] + 1)
)
levels(choice_data$age_group)
levels(choice_data$horse_country)
levels(choice_data$draw_group)

install.packages("cmdstanr",
                 repos = c("https://stan-dev.r-universe.dev",
                           getOption("repos")))

library(cmdstanr)

check_cmdstan_toolchain()
set_cmdstan_path(
  "C:/Users/priya/Downloads/cmdstan_test/cmdstan-2.40.0"
)

cmdstan_path()
cmdstan_version()
cmdstan_path()
getwd()
file.exists("horse_racing_hbm.stan")
mod <- cmdstan_model("horse_racing_hbm.stan")

fit <- mod$sample(
  data = stan_data,
  seed = 1234,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  adapt_delta = 0.95,
  max_treedepth = 12
)
fit$diagnostic_summary()
fit$sampler_diagnostics()
fit$save_object(
  file = "horse_racing_fit.rds"
)
fit <- readRDS("horse_racing_fit.rds")
file.exists("horse_racing_fit.rds")
file.info("horse_racing_fit.rds")$size / 1024^2
fit$summary(
  variables = c(
    "beta",
    "sigma_horse",
    "sigma_jockey",
    "sigma_trainer"
  )
)
print(
  fit$summary(
    variables = c(
      "beta",
      "sigma_horse",
      "sigma_jockey",
      "sigma_trainer"
    )
  ),
  n = 32
)
fit$diagnostic_summary()
#Posterior random effects
draws <- posterior::as_draws_df(
  fit$draws(
    variables = c(
      "sigma_horse",
      "sigma_jockey",
      "sigma_trainer"
    )
  )
)
mean(draws$sigma_horse > draws$sigma_jockey)
mean(draws$sigma_jockey > draws$sigma_trainer)
mean(draws$sigma_horse > draws$sigma_trainer)
mean(
  draws$sigma_horse > draws$sigma_jockey &
    draws$sigma_jockey > draws$sigma_trainer
)
re_diag <- fit$summary(
  variables = c("z_horse", "z_jockey", "z_trainer")
)

c(
  max_Rhat = max(re_diag$rhat, na.rm = TRUE),
  min_ESS_bulk = min(re_diag$ess_bulk, na.rm = TRUE),
  min_ESS_tail = min(re_diag$ess_tail, na.rm = TRUE)
)
library(dplyr)
re_diag %>%
  arrange(desc(rhat)) %>%
  select(variable, mean, sd, rhat, ess_bulk, ess_tail) %>%
  head(10)
jockey_frequency <- choice_data %>%
  count(jockey_id, name = "appearances") %>%
  arrange(appearances)

jockey_frequency %>%
  filter(jockey_id %in% c(65, 3, 19, 92, 139, 111, 96, 58, 163))

jockey_draws <- posterior::as_draws_df(
  fit$draws(
    variables = c("u_jockey", "sigma_jockey")
  )
)
jockey_summary <- fit$summary(
  variables = c(
    "u_jockey[3]",
    "u_jockey[19]",
    "u_jockey[58]",
    "u_jockey[65]",
    "u_jockey[92]",
    "u_jockey[96]",
    "u_jockey[111]",
    "u_jockey[139]",
    "u_jockey[163]"
  )
)

jockey_summary %>%
  select(variable, mean, sd, q5, q95, rhat, ess_bulk)
#Prior predictive checking
library(cmdstanr)

prior_mod <- cmdstan_model(
  "horse_racing_prior.stan"
)
prior_fit <- prior_mod$sample(
  data = stan_data,
  seed = 2026,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 500,
  iter_sampling = 1000
)
prior_fit$save_object(
  "horse_racing_prior_draws.rds"
)
prior_fit<-readRDS("horse_racing_prior_draws.rds")
file.exists("horse_racing_prior_draws.rds")
prior_fit$metadata()$model_name
prior_fit$summary(
  variables = c(
    "beta",
    "sigma_horse",
    "sigma_jockey",
    "sigma_trainer"
  )
)
prior_fit$summary(
  variables = c(
    "beta",
    "sigma_horse",
    "sigma_jockey",
    "sigma_trainer"
  )
) %>%
  print(n = 32)
set.seed(2026)

sigma_test <- abs(rnorm(100000, 0, 1))

set.seed(2026)

S <- 500
prior_selected_prob <- matrix(
  NA_real_,
  nrow = S,
  ncol = stan_data$R
)
prior_winners <- matrix(
  NA_integer_,
  nrow = S,
  ncol = stan_data$R
)

prior_max_prob <- matrix(
  NA_real_,
  nrow = S,
  ncol = stan_data$R
)

prior_entropy <- matrix(
  NA_real_,
  nrow = S,
  ncol = stan_data$R
)

for (s in 1:S) {
  
  # Regression coefficients
  beta <- rnorm(stan_data$K, 0, 1)
  
  # Random-effect standard deviations
  sigma_h <- abs(rnorm(1))
  sigma_j <- abs(rnorm(1))
  sigma_t <- abs(rnorm(1))
  
  # Standardized random effects
  z_h <- rnorm(stan_data$H)
  z_j <- rnorm(stan_data$J)
  z_t <- rnorm(stan_data$T)
  
  # Actual random effects
  u_h <- sigma_h * z_h
  u_j <- sigma_j * z_j
  u_t <- sigma_t * z_t
  
  # Linear predictor
  eta <- drop(X %*% beta) +
    u_h[stan_data$horse_id] +
    u_j[stan_data$jockey_id] +
    u_t[stan_data$trainer_id]
  
  # Race-wise winning probabilities
  for (r in 1:stan_data$R) {
    
    idx <- stan_data$start[r]:stan_data$end[r]
    
    e <- eta[idx]
    p <- exp(e - max(e))
    p <- p / sum(p)
    
    prior_winners[s, r] <- sample(
      seq_along(p),
      size = 1,
      prob = p
    )
    prior_selected_prob[s, r] <-
      p[prior_winners[s, r]]
    
    prior_max_prob[s, r] <- max(p)
    
    prior_entropy[s, r] <- -sum(p * log(p))
  }
 
  
  
}


quantile(
  as.vector(prior_selected_prob),
  c(0, 0.05, 0.25, 0.50, 0.75, 0.95, 1)
)
quantile(
  as.vector(prior_max_prob),
  c(0, 0.05, 0.25, 0.50, 0.75, 0.95, 1)
)
# Every race should have exactly one simulated winner
table(rowSums(!is.na(prior_winners)))

# Check dimensions
dim(prior_winners)
mean(prior_selected_prob > 0.50)
mean(prior_selected_prob > 0.75)
mean(prior_selected_prob > 0.90)
# Field size for every race
field_size <- stan_data$end - stan_data$start + 1

# Normalized entropy
prior_entropy_normalized <- prior_entropy /
  matrix(
    log(field_size),
    nrow = nrow(prior_entropy),
    ncol = length(field_size),
    byrow = TRUE
  )

quantile(
  as.vector(prior_entropy_normalized),
  c(0, 0.05, 0.25, 0.50, 0.75, 0.95, 1)
)
mean(prior_entropy_normalized < 0.25)
mean(prior_entropy_normalized < 0.50)
mean(prior_entropy_normalized > 0.75)

#From our model fit
# Verify that the fitted model is still available
exists("fit")

# Check the generated log-likelihood variable
fit$summary(variables = "log_lik[1]")
#Check again
fit$summary(
  variables = c(
    "beta",
    "sigma_horse",
    "sigma_jockey",
    "sigma_trainer"
  )
)
library(posterior)

set.seed(123)

posterior_draws <- as_draws_matrix(
  fit$draws(
    variables = c(
      "beta",
      "sigma_horse",
      "sigma_jockey",
      "sigma_trainer",
      "z_horse",
      "z_jockey",
      "z_trainer"
    )
  )
)

selected_draws <- sample(
  seq_len(nrow(posterior_draws)),
  size = 500
)

posterior_draws <- posterior_draws[
  selected_draws,
  ,
  drop = FALSE
]

dim(posterior_draws)
colnames(posterior_draws)[1:40]
c(
  beta = sum(grepl("^beta\\[", colnames(posterior_draws))),
  z_horse = sum(grepl("^z_horse\\[", colnames(posterior_draws))),
  z_jockey = sum(grepl("^z_jockey\\[", colnames(posterior_draws))),
  z_trainer = sum(grepl("^z_trainer\\[", colnames(posterior_draws)))
)
# Extract standardized random effects
z_horse <- posterior_draws[
  , grep("^z_horse\\[", colnames(posterior_draws)),
  drop = FALSE
]

z_jockey <- posterior_draws[
  , grep("^z_jockey\\[", colnames(posterior_draws)),
  drop = FALSE
]

z_trainer <- posterior_draws[
  , grep("^z_trainer\\[", colnames(posterior_draws)),
  drop = FALSE
]

# Extract standard deviations
sigma_horse <- posterior_draws[, "sigma_horse"]
sigma_jockey <- posterior_draws[, "sigma_jockey"]
sigma_trainer <- posterior_draws[, "sigma_trainer"]

# Reconstruct actual random effects
u_horse <- sweep(
  z_horse,
  MARGIN = 1,
  STATS = sigma_horse,
  FUN = "*"
)

u_jockey <- sweep(
  z_jockey,
  MARGIN = 1,
  STATS = sigma_jockey,
  FUN = "*"
)

u_trainer <- sweep(
  z_trainer,
  MARGIN = 1,
  STATS = sigma_trainer,
  FUN = "*"
)
dim(u_horse)
dim(u_jockey)
dim(u_trainer)
#beta_draws
beta_draws <- posterior_draws[
  ,
  grep("^beta\\[", colnames(posterior_draws)),
  drop = FALSE
]

dim(beta_draws)
beta_draws <- matrix(
  as.numeric(beta_draws),
  nrow = 500,
  ncol = 29
)

class(beta_draws)
dim(beta_draws)
N <- stan_data$N

posterior_eta <- matrix(
  NA_real_,
  nrow = 500,
  ncol = N
)

for (s in 1:500) {
  
  posterior_eta[s, ] <-
    drop(stan_data$X %*% beta_draws[s, ]) +
    u_horse[s, stan_data$horse_id] +
    u_jockey[s, stan_data$jockey_id] +
    u_trainer[s, stan_data$trainer_id]
  
}
dim(posterior_eta)
#race wise winner
S <- 500
R <- stan_data$R

posterior_max_prob <- matrix(NA_real_, S, R)
posterior_entropy <- matrix(NA_real_, S, R)
posterior_winners <- matrix(NA_integer_, S, R)
posterior_selected_prob <- matrix(NA_real_, S, R)
posterior_fav_wins <- matrix(NA_integer_, S, R)

set.seed(456)

for (s in 1:S) {
  
  for (r in 1:R) {
    
    idx <- stan_data$start[r]:stan_data$end[r]
    
    # Utilities for horses participating in this race
    e <- posterior_eta[s, idx]
    
    # Softmax probabilities
    p <- exp(e - max(e))
    p <- p / sum(p)
    
    # Highest winning probability
    posterior_max_prob[s, r] <- max(p)
    
    # Predictive entropy
    posterior_entropy[s, r] <-
      -sum(p[p > 0] * log(p[p > 0]))
    
    # Simulate one winner
    simulated_winner <- sample.int(
      length(p), size = 1, prob = p
    )
    
    posterior_winners[s, r] <- simulated_winner
    
    # Probability assigned to the simulated winner
    posterior_selected_prob[s, r] <- p[simulated_winner]
    
    # Did the most probable horse win the simulation?
    posterior_fav_wins[s, r] <-
      as.integer(simulated_winner == which.max(p))
  }
}
dim(posterior_winners)

table(rowSums(!is.na(posterior_winners)))

range(posterior_max_prob, na.rm = TRUE)

range(posterior_selected_prob, na.rm = TRUE)

#Calculate the observed and replicated favourite wins

observed_fav_wins <- matrix(
  NA_integer_,
  nrow = S,
  ncol = R
)

for (s in 1:S) {

  for (r in 1:R) {

    idx <- stan_data$start[r]:stan_data$end[r]

    e <- posterior_eta[s, idx]

    p <- exp(e - max(e))
    p <- p / sum(p)

    # Actual winner's position within this race
    actual_winner_local <-
      stan_data$winner[r] - stan_data$start[r] + 1

    # Predicted favourite's position
    favourite_local <- which.max(p)

    # Did the actual winner equal the favourite?
    observed_fav_wins[s, r] <-
      as.integer(actual_winner_local == favourite_local)
  }
}
observed_fav_rate <- rowMeans(observed_fav_wins)

replicated_fav_rate <- rowMeans(posterior_fav_wins)

quantile(
  observed_fav_rate,
  c(0.05, 0.25, 0.50, 0.75, 0.95)
)

quantile(
  replicated_fav_rate,
  c(0.05, 0.25, 0.50, 0.75, 0.95)
)
#we'll compare the horse_rating distribution among actual winners with the distribution among simulated winners.
observed_winner_rating <-
  choice_data$horse_rating[stan_data$winner]
summary(observed_winner_rating)

replicated_winner_rating <- matrix(
  NA_real_,
  nrow = S,
  ncol = R
)

for (s in 1:S) {
  
  for (r in 1:R) {
    
    global_winner_row <-
      stan_data$start[r] +
      posterior_winners[s, r] - 1
    
    replicated_winner_rating[s, r] <-
      choice_data$horse_rating[global_winner_row]
  }
}
observed_mean_rating <-
  mean(observed_winner_rating)

observed_sd_rating <-
  sd(observed_winner_rating)

replicated_mean_rating <-
  rowMeans(replicated_winner_rating)

replicated_sd_rating <-
  apply(replicated_winner_rating, 1, sd)

observed_mean_rating

quantile(
  replicated_mean_rating,
  c(0.05, 0.25, 0.50, 0.75, 0.95)
)

observed_sd_rating

quantile(
  replicated_sd_rating,
  c(0.05, 0.25, 0.50, 0.75, 0.95)
)
mean(
  replicated_sd_rating >= observed_sd_rating
)

range(replicated_sd_rating)
#Model misfit on sd 
#Draw
observed_winner_draw <-
  choice_data$draw[stan_data$winner]

table(observed_winner_draw)

prop.table(table(observed_winner_draw))
replicated_winner_draw <- matrix(
  NA_integer_,
  nrow = S,
  ncol = R
)

for (s in 1:S) {
  
  for (r in 1:R) {
    
    global_winner_row <-
      stan_data$start[r] +
      posterior_winners[s, r] - 1
    
    replicated_winner_draw[s, r] <-
      choice_data$draw[global_winner_row]
  }
}
draw_levels <- 1:14

observed_draw_prop <- prop.table(
  table(factor(observed_winner_draw,
               levels = draw_levels))
)

replicated_draw_prop <- matrix(
  NA_real_,
  nrow = S,
  ncol = length(draw_levels)
)

for (s in 1:S) {
  
  replicated_draw_prop[s, ] <-
    prop.table(
      table(factor(
        replicated_winner_draw[s, ],
        levels = draw_levels
      ))
    )
}

colnames(replicated_draw_prop) <- draw_levels
draw_comparison <- data.frame(
  draw = draw_levels,
  observed = as.numeric(observed_draw_prop),
  replicated_median = apply(
    replicated_draw_prop, 2, median
  ),
  replicated_5 = apply(
    replicated_draw_prop, 2, quantile, 0.05
  ),
  replicated_95 = apply(
    replicated_draw_prop, 2, quantile, 0.95
  )
)

draw_comparison
#Age
observed_winner_age <-
  choice_data$horse_age[stan_data$winner]

table(observed_winner_age)
prop.table(table(observed_winner_age))
replicated_winner_age <- matrix(
  NA_integer_,
  nrow = S,
  ncol = R
)

for (s in 1:S) {
  
  for (r in 1:R) {
    
    global_winner_row <-
      stan_data$start[r] +
      posterior_winners[s, r] - 1
    
    replicated_winner_age[s, r] <-
      choice_data$horse_age[global_winner_row]
  }
}
age_levels <- c("2", "3", "4", "5", "6+")

observed_age_group <- ifelse(
  observed_winner_age >= 6,
  "6+",
  as.character(observed_winner_age)
)

replicated_age_group <- ifelse(
  replicated_winner_age >= 6,
  "6+",
  as.character(replicated_winner_age)
)
observed_age_prop <- prop.table(
  table(factor(
    observed_age_group,
    levels = age_levels
  ))
)

replicated_age_prop <- matrix(
  NA_real_,
  nrow = S,
  ncol = length(age_levels)
)

for (s in 1:S) {
  
  replicated_age_prop[s, ] <-
    prop.table(
      table(factor(
        replicated_age_group[s, ],
        levels = age_levels
      ))
    )
}

colnames(replicated_age_prop) <- age_levels
age_comparison <- data.frame(
  age_group = age_levels,
  observed = as.numeric(observed_age_prop),
  replicated_median = apply(
    replicated_age_prop, 2, median
  ),
  replicated_5 = apply(
    replicated_age_prop, 2, quantile, 0.05
  ),
  replicated_95 = apply(
    replicated_age_prop, 2, quantile, 0.95
  )
)

age_comparison
#wins by horses
observed_horse_wins <- table(
  factor(
    choice_data$horse_id[stan_data$winner],
    levels = 1:stan_data$H
  )
)

summary(as.numeric(observed_horse_wins))

sum(observed_horse_wins > 0)

max(observed_horse_wins)
#Win Per Horse
H <- stan_data$H

replicated_horse_wins <- matrix(
  0L,
  nrow = S,
  ncol = H
)

for (s in 1:S) {
  
  for (r in 1:R) {
    
    global_winner_row <-
      stan_data$start[r] +
      posterior_winners[s, r] - 1
    
    winning_horse_id <-
      choice_data$horse_id[global_winner_row]
    
    replicated_horse_wins[s, winning_horse_id] <-
      replicated_horse_wins[s, winning_horse_id] + 1L
  }
}
rep_horses_with_wins <- rowSums(
  replicated_horse_wins > 0
)

rep_max_horse_wins <- apply(
  replicated_horse_wins, 1, max
)

rep_mean_horse_wins <- rowMeans(
  replicated_horse_wins
)

rep_sd_horse_wins <- apply(
  replicated_horse_wins, 1, sd
)
observed_horses_with_wins <-
  sum(observed_horse_wins > 0)

observed_max_horse_wins <-
  max(observed_horse_wins)

observed_sd_horse_wins <-
  sd(as.numeric(observed_horse_wins))
rbind(
  horses_with_wins = quantile(
    rep_horses_with_wins,
    c(0.05, 0.50, 0.95)
  ),
  
  maximum_wins = quantile(
    rep_max_horse_wins,
    c(0.05, 0.50, 0.95)
  ),
  
  mean_wins = quantile(
    rep_mean_horse_wins,
    c(0.05, 0.50, 0.95)
  ),
  
  sd_wins = quantile(
    rep_sd_horse_wins,
    c(0.05, 0.50, 0.95)
  )
)
c(
  observed_horses_with_wins = observed_horses_with_wins,
  observed_max_wins = observed_max_horse_wins,
  observed_mean_wins = mean(as.numeric(observed_horse_wins)),
  observed_sd_wins = observed_sd_horse_wins
)
# Group win counts into categories
observed_win_category <- cut(
  as.numeric(observed_horse_wins),
  breaks = c(-1, 0, 1, 2, Inf),
  labels = c("0", "1", "2", "3+")
)

observed_win_prop <- prop.table(
  table(observed_win_category)
)

replicated_win_prop <- matrix(
  NA_real_,
  nrow = S,
  ncol = 4
)

for (s in 1:S) {
  
  category <- cut(
    replicated_horse_wins[s, ],
    breaks = c(-1, 0, 1, 2, Inf),
    labels = c("0", "1", "2", "3+")
  )
  
  replicated_win_prop[s, ] <-
    as.numeric(prop.table(
      table(factor(category, levels = c("0", "1", "2", "3+"))
      )))
}

colnames(replicated_win_prop) <- c("0", "1", "2", "3+")

data.frame(
  category = c("0", "1", "2", "3+"),
  observed = as.numeric(observed_win_prop),
  replicated_median = apply(replicated_win_prop, 2, median),
  replicated_5 = apply(replicated_win_prop, 2, quantile, 0.05),
  replicated_95 = apply(replicated_win_prop, 2, quantile, 0.95)
)
#INSPECT HORSE APPEARENCE
observed_horse_appearances <- table(
  factor(choice_data$horse_id, levels = 1:stan_data$H)
)

horse_performance <- data.frame(
  horse_id = 1:stan_data$H,
  appearances = as.numeric(observed_horse_appearances),
  wins = as.numeric(observed_horse_wins)
)

horse_performance$win_rate <-
  with(horse_performance,
       ifelse(appearances > 0, wins / appearances, NA_real_))

summary(horse_performance$appearances)

summary(horse_performance$win_rate)
cor(
  horse_performance$appearances,
  horse_performance$wins
)
table(
  cut(
    horse_performance$appearances,
    breaks = c(0, 1, 5, 10, 20, Inf),
    include.lowest = TRUE
  ),
  horse_performance$wins > 0
)
#appearence
horse_performance$appearance_group <- cut(
  horse_performance$appearances,
  breaks = c(0, 1, 5, 10, 20, Inf),
  labels = c("1", "2-5", "6-10", "11-20", "20+")
)

appearance_summary <- aggregate(
  cbind(appearances, wins) ~ appearance_group,
  data = horse_performance,
  FUN = sum
)

appearance_summary$win_rate <-
  appearance_summary$wins /
  appearance_summary$appearances

appearance_summary
#
appearance_levels <- levels(horse_performance$appearance_group)

rep_appearance_winrate <- matrix(
  NA_real_,
  nrow = S,
  ncol = length(appearance_levels)
)

for (s in 1:S) {
  
  for (g in seq_along(appearance_levels)) {
    
    ids <- which(
      horse_performance$appearance_group == appearance_levels[g]
    )
    
    rep_appearance_winrate[s, g] <-
      sum(replicated_horse_wins[s, ids]) /
      sum(horse_performance$appearances[ids])
  }
}
appearance_ppc <- data.frame(
  appearance_group = appearance_levels,
  observed = appearance_summary$win_rate,
  replicated_median = apply(
    rep_appearance_winrate, 2, median
  ),
  lower_90 = apply(
    rep_appearance_winrate, 2, quantile, 0.05
  ),
  upper_90 = apply(
    rep_appearance_winrate, 2, quantile, 0.95
  )
)

appearance_ppc
horse_performance$u_horse_mean <- colMeans(u_horse)

cor(
  horse_performance$appearances,
  horse_performance$u_horse_mean
)
summary(
  lm(
    u_horse_mean ~ log(appearances),
    data = horse_performance
  )
)
#By age

horse_age_summary <- choice_data %>%
  group_by(horse_id) %>%
  summarise(
    appearances = n(),
    mean_age = mean(horse_age),
    first_age = min(horse_age),
    last_age = max(horse_age),
    .groups = "drop"
  )

cor(
  horse_age_summary$appearances,
  horse_age_summary$mean_age
)
age_win_summary <- choice_data %>%
  group_by(horse_age) %>%
  summarise(
    appearances = n(),
    wins = sum(won),
    win_rate = wins / appearances,
    .groups = "drop"
  )

age_win_summary
age_levels <- sort(unique(choice_data$horse_age))

age_win_ppc <- data.frame(
  horse_age = age_levels,
  
  observed = sapply(age_levels, function(a) {
    mean(observed_winner_age == a)
  }),
  
  replicated_median = sapply(age_levels, function(a) {
    median(rowMeans(replicated_winner_age == a))
  }),
  
  lower_90 = sapply(age_levels, function(a) {
    quantile(rowMeans(replicated_winner_age == a), 0.05)
  }),
  
  upper_90 = sapply(age_levels, function(a) {
    quantile(rowMeans(replicated_winner_age == a), 0.95)
  })
)

age_win_ppc
#Age specific win rates
observed_age_rate <- choice_data %>%
  group_by(horse_age) %>%
  summarise(
    appearances = n(),
    wins = sum(won),
    win_rate = wins / appearances,
    .groups = "drop"
  )

observed_age_rate
replicated_age_rate <- matrix(
  NA_real_,
  nrow = S,
  ncol = length(age_levels)
)
replicated_winners_global <- matrix(
  NA_integer_,
  nrow = S,
  ncol = stan_data$R
)

for (s in 1:S) {
  for (r in 1:stan_data$R) {
    replicated_winners_global[s, r] <-
      stan_data$start[r] + posterior_winners[s, r] - 1
  }
}
for (s in 1:S) {
  
  for (g in seq_along(age_levels)) {
    
    a <- age_levels[g]
    
    ids <- which(choice_data$horse_age == a)
    
    replicated_age_rate[s, g] <-
      sum(replicated_winners_global[s, ] %in% ids) /
      length(ids)
  }
}
age_rate_ppc <- data.frame(
  horse_age = age_levels,
  
  observed = observed_age_rate$win_rate,
  
  replicated_median = apply(
    replicated_age_rate, 2, median
  ),
  
  lower_90 = apply(
    replicated_age_rate, 2, quantile, probs = 0.05
  ),
  
  upper_90 = apply(
    replicated_age_rate, 2, quantile, probs = 0.95
  )
)

age_rate_ppc
age_rate_ppc[-1] <- lapply(
  age_rate_ppc[-1],
  function(x) round(x * 100, 3)
)

age_rate_ppc
#Expected win for every horse
expected_horse_wins <- matrix(
  0,
  nrow = S,
  ncol = stan_data$H
)

for (s in 1:S) {
  
  for (r in 1:stan_data$R) {
    
    idx <- stan_data$start[r]:stan_data$end[r]
    
    e <- posterior_eta[s, idx]
    
    p <- exp(e - max(e))
    p <- p / sum(p)
    
    ids <- stan_data$horse_id[idx]
    
    expected_horse_wins[s, ids] <-
      expected_horse_wins[s, ids] + p
  }
}
horse_model_check <- data.frame(
  horse_id = 1:stan_data$H,
  
  appearances = as.numeric(
    table(factor(
      choice_data$horse_id,
      levels = 1:stan_data$H
    ))
  ),
  
  observed_wins = as.numeric(observed_horse_wins),
  
  expected_wins = colMeans(expected_horse_wins)
)

horse_model_check$observed_win_rate <-
  horse_model_check$observed_wins /
  horse_model_check$appearances

horse_model_check$expected_win_rate <-
  horse_model_check$expected_wins /
  horse_model_check$appearances
summary(horse_model_check)

horse_model_check$appearance_group <- cut(
  horse_model_check$appearances,
  breaks = c(0, 1, 5, 10, 20, Inf),
  labels = c("1", "2-5", "6-10", "11-20", "20+")
)

horse_calibration <- aggregate(
  cbind(
    appearances,
    observed_wins,
    expected_wins
  ) ~ appearance_group,
  data = horse_model_check,
  FUN = sum
)

horse_calibration$observed_rate <-
  horse_calibration$observed_wins /
  horse_calibration$appearances

horse_calibration$expected_rate <-
  horse_calibration$expected_wins /
  horse_calibration$appearances

horse_calibration
#identify how many individual horses the model fails to reproduce
horse_model_check$predicted_lower <- apply(
  replicated_horse_wins,
  2,
  quantile,
  probs = 0.05
)

horse_model_check$predicted_upper <- apply(
  replicated_horse_wins,
  2,
  quantile,
  probs = 0.95
)

horse_model_check$outside_interval <-
  horse_model_check$observed_wins <
  horse_model_check$predicted_lower |
  horse_model_check$observed_wins >
  horse_model_check$predicted_upper

mean(horse_model_check$outside_interval)

table(horse_model_check$outside_interval)
aggregate(
  outside_interval ~ appearance_group,
  data = horse_model_check,
  FUN = mean
)
#group-level discrepancy
appearance_tail_check <- data.frame(
  appearance_group = appearance_levels,
  
  observed = appearance_summary$win_rate,
  
  replicated_median = apply(
    rep_appearance_winrate, 2, median
  ),
  
  probability_replicated_ge_observed = apply(
    rep_appearance_winrate, 2,
    function(x) mean(x >= 0) # placeholder
  )
)
observed_rates <- appearance_summary$win_rate

tail_prob <- sapply(seq_along(appearance_levels), function(g) {
  
  mean(
    rep_appearance_winrate[, g] >= observed_rates[g]
  )
})

appearance_tail_check$probability_replicated_ge_observed <-
  tail_prob

appearance_tail_check
#examine the racing history of individual horses
race <- read.csv("C:/Users/priya/Downloads/races.csv")
dim(race)
names(race)
str(race)
head(race[, c("race_id", "date", "venue", "distance")])
class(race$date)

race$date_parsed <- as.Date(
  race$date,
  format = "%Y-%m-%d"
)

sum(is.na(race$date_parsed))

range(race$date_parsed, na.rm = TRUE)

length(unique(race$date_parsed))
#chronological ordering
race_chronological <- race %>%
  arrange(date_parsed)

head(
  race_chronological[, c("race_id", "date", "date_parsed")],
  10
)

tail(
  race_chronological[, c("race_id", "date", "date_parsed")],
  10
)
table(table(race$date_parsed))
c(
  race_dataset_rows = nrow(race),
  unique_race_ids = length(unique(race$race_id)),
  model_races = stan_data$R,
  matched_races = sum(
    race$race_id %in% unique(choice_data$race_id)
  )
)
runs_date_check <- runs %>%
  left_join(
    race %>% select(race_id, date_parsed, venue, race_no),
    by = "race_id"
  )

runs_date_check %>%
  count(horse_id, date_parsed) %>%
  filter(n > 1)
#Calculate previous starts
runs_chronological <- runs_date_check %>%
  arrange(horse_id, date_parsed, venue, race_no) %>%
  group_by(horse_id) %>%
  mutate(
    career_start = row_number(),
    previous_starts = career_start - 1L
  ) %>%
  ungroup()
runs_chronological %>%
  select(
    horse_id,
    race_id,
    date_parsed,
    venue,
    race_no,
    career_start,
    previous_starts
  ) %>%
  arrange(horse_id, career_start) %>%
  group_by(horse_id) %>%
  slice_head(n = 5) %>%
  ungroup() %>%
  head(20)
summary(runs_chronological$previous_starts)

runs_chronological %>%
  group_by(horse_id) %>%
  summarise(
    total_starts = n(),
    first_previous_starts = min(previous_starts),
    last_previous_starts = max(previous_starts),
    .groups = "drop"
  ) %>%
  summary()
#Adding experience to the model dataset
experience_data <- runs_chronological %>%
  select(race_id, horse_id, previous_starts)

runs_chronological <- runs_chronological %>%
  mutate(
    horse_id_model = as.integer(factor(horse_id))
  )
c(
  original_unique_horses = n_distinct(runs_chronological$horse_id),
  encoded_unique_horses = n_distinct(runs_chronological$horse_id_model),
  choice_unique_horses = n_distinct(choice_data$horse_id)
)

setequal(
  sort(unique(choice_data$horse_id)),
  1:4405
)
choice_data <- choice_data  %>%
  left_join(
    runs_chronological %>%
      select(race_id, horse_id_model, previous_starts),
    by = c(
      "race_id",
      "horse_id" = "horse_id_model"
    )
  )
c(
  observations = nrow(choice_data),
  missing_experience = sum(is.na(choice_data$previous_starts)),
  unique_horses = n_distinct(choice_data$horse_id),
  unique_races = n_distinct(choice_data$race_id)
)
#Let's understand how experience relates to winning

choice_data <- choice_data %>%
  mutate(
    experience_group = cut(
      previous_starts,
      breaks = c(-1, 0, 5, 10, 20, 40, Inf),
      labels = c(
        "0",
        "1-5",
        "6-10",
        "11-20",
        "21-40",
        "41+"
      )
    )
  )

experience_summary <- choice_data %>%
  group_by(experience_group) %>%
  summarise(
    appearances = n(),
    wins = sum(won),
    win_rate = mean(won),
    .groups = "drop"
  )

experience_summary
#Exp with age
experience_age <- choice_data %>%
  group_by(experience_group) %>%
  summarise(
    mean_age = mean(horse_age, na.rm = TRUE),
    median_age = median(horse_age, na.rm = TRUE),
    mean_rating = mean(horse_rating, na.rm = TRUE),
    median_rating = median(horse_rating, na.rm = TRUE),
    horses = n_distinct(horse_id),
    .groups = "drop"
  )

experience_age
#Separate within-horse and between-horse experience
choice_data <- choice_data %>%
  group_by(horse_id) %>%
  mutate(
    experience_mean = mean(previous_starts),
    experience_within = previous_starts - experience_mean
  ) %>%
  ungroup()
experience_decomposition <- choice_data %>%
  group_by(experience_group) %>%
  summarise(
    mean_within_experience = mean(experience_within),
    win_rate = mean(won),
    .groups = "drop"
  )

experience_decomposition
cor(
  choice_data$previous_starts,
  choice_data$horse_age,
  use = "complete.obs"
)
#Let's inspect the within-horse pattern directly
horse_experience_summary <- choice_data %>%
  group_by(horse_id, experience_group) %>%
  summarise(
    appearances = n(),
    wins = sum(won),
    win_rate = mean(won),
    .groups = "drop"
  )

horse_experience_summary %>%
  group_by(experience_group) %>%
  summarise(
    horses = n(),
    total_appearances = sum(appearances),
    total_wins = sum(wins),
    pooled_win_rate = total_wins / total_appearances,
    mean_horse_win_rate = mean(win_rate),
    .groups = "drop"
  )
#Let's calculate the win rate by experience group separately for horses with sufficient career history

eligible_horses <- choice_data %>%
  count(horse_id) %>%
  filter(n >= 20) %>%
  pull(horse_id)

choice_data %>%
  filter(horse_id %in% eligible_horses) %>%
  group_by(experience_group) %>%
  summarise(
    appearances = n(),
    wins = sum(won),
    pooled_win_rate = mean(won),
    horses = n_distinct(horse_id),
    .groups = "drop"
  )
#Let's first fit a diagnostic classical logistic model to check is there any non linear relationship with winning after accounting for horse age and rating
experience_glm <- glm(
  won ~
    rating_z +
    age_group +
    horse_country +
    declared_weight_z +
    actual_weight_z +
    draw_group +
    experience_group,
  data = choice_data,
  family = binomial()
)

summary(experience_glm)
#Accounting for trainer.... effects mixed effects model
library(lme4)

experience_glmer <- glmer(
  won ~
    rating_z +
    age_group +
    horse_country +
    declared_weight_z +
    actual_weight_z +
    draw_group +
    experience_group +
    (1 | horse_id) +
    (1 | jockey_id) +
    (1 | trainer_id),
  data = choice_data,
  family = binomial(link = "logit"),
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 200000)
  )
)
summary(experience_glmer)
isSingular(experience_glmer)

experience_glmer@optinfo$conv
AIC(experience_glm, experience_glmer)
#extension of HBM
# Check whether the important objects exist
objects_needed <- c(
  "choice_data",
  "race_index_data",
  "stan_data",
  "X",
  "fit"
)

sapply(objects_needed, exists)
# Check experience categories
levels(choice_data$experience_group)

# Number of observations in each category
table(choice_data$experience_group)

# Check missing values
sum(is.na(choice_data$experience_group))

# Check the number of unique horses
length(unique(choice_data$horse_id))
#updated design matrix
# Create experience design matrix
X_experience <- model.matrix(
  ~ experience_group,
  data = choice_data
)[, -1, drop = FALSE]

# Check dimensions
dim(X_experience)

# Check column names
colnames(X_experience)
X_extended <- cbind(
  X,
  X_experience
)

# Check dimensions
dim(X_extended)

# Check column names
colnames(X_extended)
# Copy the original Stan data
stan_data_extended <- stan_data

# Replace predictor matrix
stan_data_extended$X <- X_extended

# Update number of predictors
stan_data_extended$K <- ncol(X_extended)

# Verify
cat("N:", stan_data_extended$N, "\n")
cat("R:", stan_data_extended$R, "\n")
cat("K:", stan_data_extended$K, "\n")
cat("H:", stan_data_extended$H, "\n")
cat("J:", stan_data_extended$J, "\n")
cat("T:", stan_data_extended$T, "\n")

dim(stan_data_extended$X)
library(cmdstanr)

mod_experience <- cmdstan_model(
  "horse_racing_hbm_experience.stan"
)
mod_experience

# pilot_fit <- mod_experience$sample(
#   data = stan_data_extended,
#   seed = 2026,
#   chains = 4,
#   parallel_chains = 4,
#   iter_warmup = 200,
#   iter_sampling = 200,
#   adapt_delta = 0.95,
#   max_treedepth = 12
# )
# pilot_fit$summary()
# pilot_fit$time()
# pilot_fit$diagnostic_summary()
# pilot_summary <- pilot_fit$summary()
# warnings()
# 
# # Overall convergence
# max(pilot_summary$rhat, na.rm = TRUE)
# 
# # Minimum effective sample sizes
# min(pilot_summary$ess_bulk, na.rm = TRUE)
# min(pilot_summary$ess_tail, na.rm = TRUE)
# 
# # Experience coefficients
# pilot_summary[
#   grep("beta\\[", rownames(pilot_summary)),
#   c("mean", "sd", "rhat", "ess_bulk", "ess_tail")
# ]
# pilot_summary[
#   c("sigma_horse", "sigma_jockey", "sigma_trainer"),
#   c("mean", "sd", "rhat", "ess_bulk", "ess_tail")
# ]
# Parameter names and summary structure
# names(pilot_summary)

# First 30 parameter names
# head(pilot_summary$variable, 30)

# Worst-mixing parameters
pilot_summary[
  order(pilot_summary$rhat, decreasing = TRUE),
  c("variable", "mean", "sd", "rhat", "ess_bulk", "ess_tail")
][1:15, ]
# Experience effects
pilot_summary[
  pilot_summary$variable %in% paste0("beta[", 30:34, "]"),
  c("variable", "mean", "sd", "rhat", "ess_bulk", "ess_tail")
]

# Variance parameters
pilot_summary[
  pilot_summary$variable %in%
    c("sigma_horse", "sigma_jockey", "sigma_trainer"),
  c("variable", "mean", "sd", "rhat", "ess_bulk", "ess_tail")
]
#more iterations for adequate model result
output_dir <- "C:/Users/priya/Downloads/horse_experience_output"

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

normalizePath(output_dir)
experience_fit <- mod_experience$sample(
  data = stan_data_extended,
  seed = 20261117,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  adapt_delta = 0.95,
  max_treedepth = 12,
  output_dir = output_dir
)
# saveRDS(
#   experience_fit,
#   file = file.path(output_dir, "horse_racing_fit_experience.rds")
# )
experience_fit <- readRDS(
  "horse_racing_fit_experience.rds"
)
# Confirm all four CSV files exist
# csv_files <- experience_fit$output_files()
# data.frame(
#   file = basename(csv_files),
#   exists = file.exists(csv_files),
#   size_MB = round(file.info(csv_files)$size / 1024^2, 2)
# )
file.exists(
  "C:/Users/priya/Downloads/horse_experience_output/horse_racing_fit_experience.rds"
)
# experience_fit <- readRDS(
#   "C:/Users/priya/Downloads/horse_experience_output/horse_racing_fit_experience.rds"
# )

class(experience_fit)
list.files(
  "C:/Users/priya/Downloads/horse_experience_output",
  pattern = "\\.csv$",
  full.names = TRUE
)
experience_csv <- list.files(
  "C:/Users/priya/Downloads/horse_experience_output",
  pattern = "3150c0\\.csv$",
  full.names = TRUE
)

length(experience_csv)
basename(experience_csv)

experience_fit <- as_cmdstan_fit(
  files = experience_csv
)
# MCMC diagnostics
experience_fit$diagnostic_summary()

# Summary of experience coefficients and variance parameters
experience_fit$summary(
  variables = c(
    "beta",
    "sigma_horse",
    "sigma_jockey",
    "sigma_trainer"
  )
)
# Summarise only the five experience coefficients
experience_summary <- experience_fit$summary(
  variables = paste0("beta[", 30:34, "]")
)

print(experience_summary)

# Summarise the three variance parameters
variance_summary <- experience_fit$summary(
  variables = c(
    "sigma_horse",
    "sigma_jockey",
    "sigma_trainer"
  )
)

print(variance_summary)
#Prediction
#Extract log-likelihood
library(posterior)
library(loo)

log_lik_draws <- as_draws_matrix(
  experience_fit$draws("log_lik")
)

dim(log_lik_draws)
loo_experience <- loo(log_lik_draws)

print(loo_experience)
" log_lik" %in% variables(fit)
fit$metadata()$model_name
fit$draws(variables = "log_lik")
#Original model log liklihood
log_lik_original <- as_draws_matrix(
  fit$draws(variables = "log_lik")
)

dim(log_lik_original)
loo_original <- loo(log_lik_original)

loo_compare(
  loo_experience,
  loo_original
)
pareto_k_table(loo_original)
elpd_diff_race <- loo_experience$pointwise[, "elpd_loo"] -
  loo_original$pointwise[, "elpd_loo"]

summary(elpd_diff_race)
sum(elpd_diff_race > 0)
sum(elpd_diff_race < 0)
sort(elpd_diff_race, decreasing = TRUE)[1:10]

sort(elpd_diff_race)[1:10]
sum(elpd_diff_race[elpd_diff_race > 0])

sum(elpd_diff_race[elpd_diff_race < 0])

sum(elpd_diff_race)
positive_gains <- sort(
  elpd_diff_race[elpd_diff_race > 0],
  decreasing = TRUE
)
#positive improvement contributed by the top 10 races
sum(positive_gains[1:10]) / sum(positive_gains)
#lets make a train and test split then calculate accuaracy
class(race$date)

head(race$date)

summary(race$date)

length(unique(race$date))
race$date <- as.Date(race$date)

range(race$date)

sort(unique(race$date))[c(1, 342, 343, 683)]
#train-test split

race_dates <- choice_data %>%
  distinct(race_id) %>%
  left_join(
    race %>% select(race_id, date),
    by = "race_id"
  ) %>%
  count(date, name = "n_races") %>%
  arrange(date) %>%
  mutate(
    cumulative_races = cumsum(n_races),
    proportion = cumulative_races / sum(n_races)
  )

race_dates %>%
  slice(which.min(abs(proportion - 0.80)))

cutoff_date <- as.Date("2003-12-28")

race_lookup <- choice_data %>%
  distinct(race_id) %>%
  left_join(
    race %>% select(race_id, date),
    by = "race_id"
  )

train_race_ids <- race_lookup %>%
  filter(date <= cutoff_date) %>%
  pull(race_id)

test_race_ids <- race_lookup %>%
  filter(date > cutoff_date) %>%
  pull(race_id)

train_data <- choice_data %>%
  filter(race_id %in% train_race_ids)

test_data <- choice_data %>%
  filter(race_id %in% test_race_ids)
c(
  train_races = n_distinct(train_data$race_id),
  test_races = n_distinct(test_data$race_id),
  train_rows = nrow(train_data),
  test_rows = nrow(test_data)
)
range(race_lookup$date[race_lookup$race_id %in% train_race_ids])

range(race_lookup$date[race_lookup$race_id %in% test_race_ids])
#Check participant overlap
participant_overlap <- function(train, test, id_col) {
  train_ids <- unique(train[[id_col]])
  test_ids <- unique(test[[id_col]])
  
  c(
    test_total = length(test_ids),
    previously_seen = sum(test_ids %in% train_ids),
    unseen = sum(!test_ids %in% train_ids)
  )
}

participant_overlap(train_data, test_data, "horse_id")
participant_overlap(train_data, test_data, "jockey_id")
participant_overlap(train_data, test_data, "trainer_id")
colnames(X)
colnames(X_extended)
#Correct the standardisation
scaling_params <- list(
  rating_mean = mean(train_data$horse_rating, na.rm = TRUE),
  rating_sd = sd(train_data$horse_rating, na.rm = TRUE),
  
  declared_mean = mean(train_data$declared_weight, na.rm = TRUE),
  declared_sd = sd(train_data$declared_weight, na.rm = TRUE),
  
  actual_mean = mean(train_data$actual_weight, na.rm = TRUE),
  actual_sd = sd(train_data$actual_weight, na.rm = TRUE)
)

scaling_params
#Standardise both datasets
standardise_data <- function(data, params) {
  data %>%
    mutate(
      rating_z = (horse_rating - params$rating_mean) /
        params$rating_sd,
      
      declared_weight_z =
        (declared_weight - params$declared_mean) /
        params$declared_sd,
      
      actual_weight_z =
        (actual_weight - params$actual_mean) /
        params$actual_sd
    )
}

train_data <- standardise_data(train_data, scaling_params)
test_data <- standardise_data(test_data, scaling_params)
#verify
train_data %>%
  summarise(
    rating_mean = mean(rating_z, na.rm = TRUE),
    rating_sd = sd(rating_z, na.rm = TRUE),
    
    declared_mean = mean(declared_weight_z, na.rm = TRUE),
    declared_sd = sd(declared_weight_z, na.rm = TRUE),
    
    actual_mean = mean(actual_weight_z, na.rm = TRUE),
    actual_sd = sd(actual_weight_z, na.rm = TRUE)
  )
factor_vars <- c(
  "age_group",
  "horse_country",
  "draw_group",
  "experience_group"
)

lapply(train_data[factor_vars], levels)
colSums(is.na(train_data[
  c("horse_rating", "declared_weight", "actual_weight",
    factor_vars)
]))

colSums(is.na(test_data[
  c("horse_rating", "declared_weight", "actual_weight",
    factor_vars)
]))
for (v in factor_vars) {
  cat("\n", v, "\n")
  print(table(train_data[[v]]))
  print(table(test_data[[v]]))
}
#Construct the training and testing design matrices
# Original model
X_train <- model.matrix(
  ~ rating_z + age_group + horse_country +
    declared_weight_z + actual_weight_z + draw_group,
  data = train_data
)[, -1, drop = FALSE]

X_test <- model.matrix(
  ~ rating_z + age_group + horse_country +
    declared_weight_z + actual_weight_z + draw_group,
  data = test_data
)[, -1, drop = FALSE]


# Experience effects
X_exp_train <- model.matrix(
  ~ experience_group,
  data = train_data
)[, -1, drop = FALSE]

X_exp_test <- model.matrix(
  ~ experience_group,
  data = test_data
)[, -1, drop = FALSE]


# Extended model
X_train_extended <- cbind(X_train, X_exp_train)
X_test_extended <- cbind(X_test, X_exp_test)
dim(X_train)
dim(X_test)

dim(X_train_extended)
dim(X_test_extended)

identical(colnames(X_train), colnames(X_test))

identical(
  colnames(X_train_extended),
  colnames(X_test_extended)
)
names(stan_data)

str(stan_data, max.level = 1)
names(train_data)

head(
  train_data %>%
    select(
      race_id, horse_id, jockey_id, trainer_id,
      won, global_row, row_in_race
    ),
  10
)
library(dplyr)
#Build training-only Stan data
train_data_ordered <- train_data %>%
  arrange(race_index, row_in_race) %>%
  mutate(obs_id = row_number())

race_structure <- train_data_ordered %>%
  group_by(race_id) %>%
  summarise(
    start = min(obs_id),
    end = max(obs_id),
    n_winners = sum(won),
    winner = obs_id[which(won == 1)],
    .groups = "drop"
  )

table(race_structure$n_winners)

horse_lookup <- sort(unique(train_data_ordered$horse_id))
jockey_lookup <- sort(unique(train_data_ordered$jockey_id))
trainer_lookup <- sort(unique(train_data_ordered$trainer_id))

horse_train_id <- match(
  train_data_ordered$horse_id, horse_lookup
)

jockey_train_id <- match(
  train_data_ordered$jockey_id, jockey_lookup
)

trainer_train_id <- match(
  train_data_ordered$trainer_id, trainer_lookup
)
train_order_idx <- match(
  train_data_ordered$global_row,
  train_data$global_row
)

X_train_ordered <- X_train[
  train_order_idx,
  ,
  drop = FALSE
]
stan_train <- list(
  N = nrow(train_data_ordered),
  R = nrow(race_structure),
  K = ncol(X_train_ordered),
  H = length(horse_lookup),
  J = length(jockey_lookup),
  T = length(trainer_lookup),
  
  X = X_train_ordered,
  
  horse_id = as.integer(horse_train_id),
  jockey_id = as.integer(jockey_train_id),
  trainer_id = as.integer(trainer_train_id),
  
  start = as.integer(race_structure$start),
  end = as.integer(race_structure$end),
  winner = as.integer(race_structure$winner)
)
#verify
str(stan_train)

dim(stan_train$X)

c(
  N = stan_train$N,
  R = stan_train$R,
  K = stan_train$K,
  H = stan_train$H,
  J = stan_train$J,
  T = stan_train$T
)
all(stan_train$start <= stan_train$end)

stan_train$start[1]
stan_train$end[stan_train$R]

all(
  stan_train$winner >= stan_train$start &
    stan_train$winner <= stan_train$end
)
#Prepare the extended model's training data
X_train_extended_ordered <- X_train_extended[
  train_order_idx,
  ,
  drop = FALSE
]

stan_train_extended <- stan_train

stan_train_extended$K <- ncol(X_train_extended_ordered)
stan_train_extended$X <- X_train_extended_ordered

dim(stan_train_extended$X)
stan_train_extended$K
exists("mod")
exists("mod_experience")
dir.create("E:/Priyankar/train_original", recursive = TRUE, showWarnings = FALSE)
dir.create("E:/Priyankar/train_experience", recursive = TRUE, showWarnings = FALSE)
# fit_train_original <- mod$sample(
#   data = stan_train,
#   seed = 123,
#   chains = 4,
#   parallel_chains = 4,
#   iter_warmup = 1000,
#   iter_sampling = 1000,
#   adapt_delta = 0.95,
#   max_treedepth = 12,
#   output_dir = "E:/Priyankar/train_original"
# )
saveRDS(
  fit_train_original,
  "E:/Priyankar/fit_train_original.rds"
)
#rerun
dir.create(
  "E:/Priyankar/train_original_rerun",
  recursive = TRUE,
  showWarnings = FALSE
)

fit_train_original <- mod$sample(
  data = stan_train,
  seed = 456,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  adapt_delta = 0.95,
  max_treedepth = 12,
  output_dir = "E:/Priyankar/train_original_rerun"
)
fit_train_original$summary()
fit_train_original<-readRDS("E:/Priyankar/horse_racing_fit_train_original.rds")
saveRDS(
  fit_train_original,
  "E:/Priyankar/horse_racing_fit_train_original.rds"
)
file.exists(
  "E:/Priyankar/horse_racing_fit_train_original.rds"
)

original_summary <- fit_train_original$summary()

original_summary |>
  dplyr::summarise(
    max_Rhat = max(rhat, na.rm = TRUE),
    min_bulk_ESS = min(ess_bulk, na.rm = TRUE),
    min_tail_ESS = min(ess_tail, na.rm = TRUE),
    parameters_Rhat_above_1.01 =
      sum(rhat > 1.01, na.rm = TRUE)
  )
fit_train_original$diagnostic_summary()
#fit extended training model
stan_train_extended <- validation_data$stan_train_extended
dim(stan_train_extended$X)
#model_extended
fit_train_extended <- mod_experience$sample(
  data = stan_train_extended,
  seed = 456,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  adapt_delta = 0.95,
  max_treedepth = 12,
  output_dir = "E:/Priyankar/train_experience"
)
saveRDS(
  fit_train_extended,
  "E:/Priyankar/horse_racing_fit_train_extended.rds"
)
fit_train_extended<-readRDS( "E:/Priyankar/horse_racing_fit_train_extended.rds")
file.exists(
  "E:/Priyankar/horse_racing_fit_train_extended.rds"
)
extended_summary <- fit_train_extended$summary()
library(cmdstanr)

csv_files <- list.files(
  path = "E:/Priyankar/train_experience",
  pattern = "^horse_racing_hbm_experience-202611182010-.*\\.csv$",
  full.names = TRUE
)

csv_files
fit_train_extended <- as_cmdstan_fit(csv_files)
extended_summary |>
  dplyr::summarise(
    max_Rhat = max(rhat, na.rm = TRUE),
    min_bulk_ESS = min(ess_bulk, na.rm = TRUE),
    min_tail_ESS = min(ess_tail, na.rm = TRUE),
    parameters_Rhat_above_1.01 =
      sum(rhat > 1.01, na.rm = TRUE)
  )
fit_train_extended$diagnostic_summary()
extended_summary |>
  dplyr::filter(rhat > 1.01) |>
  dplyr::select(
    variable, mean, sd, rhat,
    ess_bulk, ess_tail
  )
extended_summary |>
  dplyr::filter(
    variable %in% c("z_horse[1084]", "u_horse[1084]")
  ) |>
  dplyr::mutate(rhat = sprintf("%.5f", rhat))
library(bayesplot)

horse_draws <- fit_train_extended$draws(
  variables = c("z_horse[1084]", "u_horse[1084]"),
  format = "draws_array"
)

bayesplot::mcmc_trace(
  as.array(horse_draws)
)
bayesplot::mcmc_dens_overlay(
  as.array(horse_draws)
)
fit_train_extended$summary(
  variables = c("z_horse[1084]", "u_horse[1084]")
)
horse_array <- as.array(horse_draws)

chain_means <- apply(
  horse_array,
  c(2, 3),
  mean
)

chain_means
chain_sds <- apply(
  horse_array,
  c(2, 3),
  sd
)

chain_sds
#Prediction
test_data <- validation_data$test_data

dim(test_data)

table(test_data$won)
names(test_data)

head(
  test_data[, c(
    "race_id",
    "horse_id",
    "jockey_id",
    "trainer_id",
    "global_row",
    "row_in_race"
  )]
)
horse_lookup <- validation_data$horse_lookup
jockey_lookup <- validation_data$jockey_lookup
trainer_lookup <- validation_data$trainer_lookup

test_data_ordered <- test_data %>%
  arrange(race_id, row_in_race) %>%
  mutate(obs_id = row_number())

test_horse_id <- match(
  test_data_ordered$horse_id,
  horse_lookup
)

test_jockey_id <- match(
  test_data_ordered$jockey_id,
  jockey_lookup
)

test_trainer_id <- match(
  test_data_ordered$trainer_id,
  trainer_lookup
)
data.frame(
  participant = c("Horse", "Jockey", "Trainer"),
  known = c(
    sum(!is.na(test_horse_id)),
    sum(!is.na(test_jockey_id)),
    sum(!is.na(test_trainer_id))
  ),
  unseen = c(
    sum(is.na(test_horse_id)),
    sum(is.na(test_jockey_id)),
    sum(is.na(test_trainer_id))
  )
)
data.frame(
  participant = c("Horse", "Jockey", "Trainer"),
  
  total_unique = c(
    dplyr::n_distinct(test_data_ordered$horse_id),
    dplyr::n_distinct(test_data_ordered$jockey_id),
    dplyr::n_distinct(test_data_ordered$trainer_id)
  ),
  
  known_unique = c(
    dplyr::n_distinct(test_data_ordered$horse_id[!is.na(test_horse_id)]),
    dplyr::n_distinct(test_data_ordered$jockey_id[!is.na(test_jockey_id)]),
    dplyr::n_distinct(test_data_ordered$trainer_id[!is.na(test_trainer_id)])
  ),
  
  unseen_unique = c(
    dplyr::n_distinct(test_data_ordered$horse_id[is.na(test_horse_id)]),
    dplyr::n_distinct(test_data_ordered$jockey_id[is.na(test_jockey_id)]),
    dplyr::n_distinct(test_data_ordered$trainer_id[is.na(test_trainer_id)])
  )
)
test_race_structure <- test_data_ordered %>%
  group_by(race_id) %>%
  summarise(
    start = min(obs_id),
    end = max(obs_id),
    n_winners = sum(won),
    winner = obs_id[which(won == 1)],
    field_size = n(),
    .groups = "drop"
  )

dim(test_race_structure)

table(test_race_structure$n_winners)

summary(test_race_structure$field_size)
library(posterior)

beta_original <- as_draws_matrix(
  fit_train_original$draws("beta")
)

beta_extended <- as_draws_matrix(
  fit_train_extended$draws("beta")
)

dim(beta_original)
dim(beta_extended)
horse_original <- as_draws_matrix(
  fit_train_original$draws("u_horse")
)

jockey_original <- as_draws_matrix(
  fit_train_original$draws("u_jockey")
)

trainer_original <- as_draws_matrix(
  fit_train_original$draws("u_trainer")
)

sigma_original <- as_draws_matrix(
  fit_train_original$draws(
    c("sigma_horse", "sigma_jockey", "sigma_trainer")
  )
)
horse_extended <- as_draws_matrix(
  fit_train_extended$draws("u_horse")
)
# Unique unseen participant IDs
new_horses <- sort(unique(
  test_data_ordered$horse_id[is.na(test_horse_id)]
))

new_jockeys <- sort(unique(
  test_data_ordered$jockey_id[is.na(test_jockey_id)]
))

new_trainers <- sort(unique(
  test_data_ordered$trainer_id[is.na(test_trainer_id)]
))

# Combined indices: known participants first, unseen afterwards
horse_test_index <- ifelse(
  is.na(test_horse_id),
  length(horse_lookup) +
    match(test_data_ordered$horse_id, new_horses),
  test_horse_id
)

jockey_test_index <- ifelse(
  is.na(test_jockey_id),
  length(jockey_lookup) +
    match(test_data_ordered$jockey_id, new_jockeys),
  test_jockey_id
)

trainer_test_index <- ifelse(
  is.na(test_trainer_id),
  length(trainer_lookup) +
    match(test_data_ordered$trainer_id, new_trainers),
  test_trainer_id
)
jockey_extended <- as_draws_matrix(
  fit_train_extended$draws("u_jockey")
)

trainer_extended <- as_draws_matrix(
  fit_train_extended$draws("u_trainer")
)

sigma_extended <- as_draws_matrix(
  fit_train_extended$draws(
    c("sigma_horse", "sigma_jockey", "sigma_trainer")
  )
)
dim(horse_original)
dim(jockey_original)
dim(trainer_original)

dim(horse_extended)
dim(jockey_extended)
dim(trainer_extended)

dim(sigma_original)
dim(sigma_extended)
c(
  horses = length(unique(horse_test_index)),
  jockeys = length(unique(jockey_test_index)),
  trainers = length(unique(trainer_test_index))
)
#preparing test data
X_test <- validation_data$X_test
X_test_extended <- validation_data$X_test_extended

test_order_idx <- match(
  test_data_ordered$global_row,
  test_data$global_row
)

X_test_ordered <- X_test[test_order_idx, , drop = FALSE]

X_test_extended_ordered <-
  X_test_extended[test_order_idx, , drop = FALSE]

dim(X_test_ordered)
dim(X_test_extended_ordered)

identical(
  colnames(X_test_ordered),
  colnames(X_test_extended_ordered)[1:29]
)
colnames(sigma_original)
colnames(sigma_extended)

head(colnames(horse_original))
head(colnames(jockey_original))
head(colnames(trainer_original))
#generate random effects for unseen coefficients
set.seed(20261118)

S <- nrow(beta_original)

# Generate common standard-normal draws for unseen participants
# These are shared between the two models for a fair comparison.

z_new_horse <- matrix(
  rnorm(S * length(new_horses)),
  nrow = S
)

z_new_jockey <- matrix(
  rnorm(S * length(new_jockeys)),
  nrow = S
)

z_new_trainer <- matrix(
  rnorm(S * length(new_trainers)),
  nrow = S
)

# Original model: scale by posterior standard deviations
new_horse_original <- sweep(
  z_new_horse, 1, sigma_original[, "sigma_horse"], "*"
)

new_jockey_original <- sweep(
  z_new_jockey, 1, sigma_original[, "sigma_jockey"], "*"
)

new_trainer_original <- sweep(
  z_new_trainer, 1, sigma_original[, "sigma_trainer"], "*"
)

# Extended model: scale by its own posterior standard deviations
new_horse_extended <- sweep(
  z_new_horse, 1, sigma_extended[, "sigma_horse"], "*"
)

new_jockey_extended <- sweep(
  z_new_jockey, 1, sigma_extended[, "sigma_jockey"], "*"
)

new_trainer_extended <- sweep(
  z_new_trainer, 1, sigma_extended[, "sigma_trainer"], "*"
)
# Original model
horse_effects_original <- cbind(
  horse_original,
  new_horse_original
)

jockey_effects_original <- cbind(
  jockey_original,
  new_jockey_original
)

trainer_effects_original <- cbind(
  trainer_original,
  new_trainer_original
)

# Extended model
horse_effects_extended <- cbind(
  horse_extended,
  new_horse_extended
)

jockey_effects_extended <- cbind(
  jockey_extended,
  new_jockey_extended
)

trainer_effects_extended <- cbind(
  trainer_extended,
  new_trainer_extended
)

# Verify dimensions
dim(horse_effects_original)
dim(jockey_effects_original)
dim(trainer_effects_original)

dim(horse_effects_extended)
dim(jockey_effects_extended)
dim(trainer_effects_extended)
#Calculate the posterior predictive probabilities
predict_race_probabilities <- function(
    X,
    beta,
    horse_effects,
    jockey_effects,
    trainer_effects,
    horse_index,
    jockey_index,
    trainer_index,
    race_structure,
    batch_size = 100
) {
  
  S <- nrow(beta)
  N <- nrow(X)
  
  # Store posterior mean probability for each runner
  probability_sum <- numeric(N)
  
  for (first in seq(1, S, by = batch_size)) {
    
    last <- min(first + batch_size - 1, S)
    draws <- first:last
    
    # Fixed effects: batch_size x N
    eta <- beta[draws, , drop = FALSE] %*% t(X)
    
    # Add participant random effects
    eta <- eta +
      horse_effects[draws, horse_index, drop = FALSE] +
      jockey_effects[draws, jockey_index, drop = FALSE] +
      trainer_effects[draws, trainer_index, drop = FALSE]
    
    # Race-wise softmax
    for (r in seq_len(nrow(race_structure))) {
      
      idx <- race_structure$start[r]:race_structure$end[r]
      
      race_eta <- eta[, idx, drop = FALSE]
      
      # Numerically stable softmax
      race_eta <- race_eta -
        matrix(
          apply(race_eta, 1, max),
          nrow = length(draws),
          ncol = length(idx)
        )
      
      exp_eta <- exp(race_eta)
      
      race_prob <- exp_eta / rowSums(exp_eta)
      
      probability_sum[idx] <-
        probability_sum[idx] + colSums(race_prob)
    }
    
    cat("Completed posterior draws:", last, "/", S, "\n")
  }
  
  probability_sum / S
}
test_prob_original <- predict_race_probabilities(
  X = X_test_ordered,
  beta = beta_original,
  horse_effects = horse_effects_original,
  jockey_effects = jockey_effects_original,
  trainer_effects = trainer_effects_original,
  horse_index = horse_test_index,
  jockey_index = jockey_test_index,
  trainer_index = trainer_test_index,
  race_structure = test_race_structure,
  batch_size = 100
)
summary(test_prob_original)
#extended_model
test_prob_extended <- predict_race_probabilities(
  X = X_test_extended_ordered,
  beta = beta_extended,
  horse_effects = horse_effects_extended,
  jockey_effects = jockey_effects_extended,
  trainer_effects = trainer_effects_extended,
  horse_index = horse_test_index,
  jockey_index = jockey_test_index,
  trainer_index = trainer_test_index,
  race_structure = test_race_structure,
  batch_size = 100
)
summary(test_prob_extended)
#Check Predictive Performance
evaluate_racing_model <- function(probabilities, test_data, race_structure) {
  
  actual <- test_data$won
  
  # Top-1 accuracy
  predicted_winners <- integer(nrow(race_structure))
  
  for (r in seq_len(nrow(race_structure))) {
    
    idx <- race_structure$start[r]:race_structure$end[r]
    
    predicted_winners[r] <- idx[which.max(probabilities[idx])]
  }
  
  top1_accuracy <- mean(actual[predicted_winners] == 1)
  
  # Log loss: negative log probability assigned to actual winner
  winner_indices <- race_structure$winner
  
  log_loss <- -mean(log(probabilities[winner_indices]))
  
  # Multiclass Brier score, averaged across races
  brier_per_race <- numeric(nrow(race_structure))
  
  for (r in seq_len(nrow(race_structure))) {
    
    idx <- race_structure$start[r]:race_structure$end[r]
    
    brier_per_race[r] <- sum(
      (probabilities[idx] - actual[idx])^2
    )
  }
  
  brier_score <- mean(brier_per_race)
  
  c(
    Top1_Accuracy = top1_accuracy,
    Log_Loss = log_loss,
    Brier_Score = brier_score
  )
}
results_original <- evaluate_racing_model(
  test_prob_original,
  test_data_ordered,
  test_race_structure
)

results_extended <- evaluate_racing_model(
  test_prob_extended,
  test_data_ordered,
  test_race_structure
)

rbind(
  Original = results_original,
  Extended = results_extended
)
#Check whether the improvement is statistically meaningful
race_comparison <- data.frame(
  race_id = test_race_structure$race_id,
  original_correct = logical(nrow(test_race_structure)),
  extended_correct = logical(nrow(test_race_structure))
)

for (r in seq_len(nrow(test_race_structure))) {
  
  idx <- test_race_structure$start[r]:
    test_race_structure$end[r]
  
  original_prediction <- idx[which.max(test_prob_original[idx])]
  extended_prediction <- idx[which.max(test_prob_extended[idx])]
  
  actual_winner <- test_race_structure$winner[r]
  
  race_comparison$original_correct[r] <-
    original_prediction == actual_winner
  
  race_comparison$extended_correct[r] <-
    extended_prediction == actual_winner
}

table(
  Original = race_comparison$original_correct,
  Extended = race_comparison$extended_correct
)
mcnemar.test(
  table(
    race_comparison$original_correct,
    race_comparison$extended_correct
  ),
  correct = TRUE
)
#Calculate race-level scores
race_scores <- data.frame(
  race_id = test_race_structure$race_id,
  logloss_original = NA_real_,
  logloss_extended = NA_real_,
  brier_original = NA_real_,
  brier_extended = NA_real_
)

for (r in seq_len(nrow(test_race_structure))) {
  
  idx <- test_race_structure$start[r]:
    test_race_structure$end[r]
  
  actual <- test_data_ordered$won[idx]
  
  # Actual winner's probability
  winner <- which(actual == 1)
  
  # Log loss
  race_scores$logloss_original[r] <-
    -log(test_prob_original[idx][winner])
  
  race_scores$logloss_extended[r] <-
    -log(test_prob_extended[idx][winner])
  
  # Multiclass Brier score
  race_scores$brier_original[r] <-
    sum((test_prob_original[idx] - actual)^2)
  
  race_scores$brier_extended[r] <-
    sum((test_prob_extended[idx] - actual)^2)
}

# Check results
head(race_scores)

colMeans(race_scores[, -1])

#bootstrap based result
set.seed(20261119)

B <- 2000
n_races <- nrow(race_scores)

bootstrap_results <- matrix(
  NA_real_,
  nrow = B,
  ncol = 2
)

colnames(bootstrap_results) <- c(
  "logloss_difference",
  "brier_difference"
)

for (b in seq_len(B)) {
  
  sampled_races <- sample(
    seq_len(n_races),
    size = n_races,
    replace = TRUE
  )
  
  sampled <- race_scores[sampled_races, ]
  
  bootstrap_results[b, "logloss_difference"] <-
    mean(sampled$logloss_extended -
           sampled$logloss_original)
  
  bootstrap_results[b, "brier_difference"] <-
    mean(sampled$brier_extended -
           sampled$brier_original)
}

# 95% percentile bootstrap confidence intervals
bootstrap_summary <- data.frame(
  Metric = c("Log loss", "Brier score"),
  
  Mean_difference = colMeans(bootstrap_results),
  
  CI_lower = apply(
    bootstrap_results, 2, quantile, probs = 0.025
  ),
  
  CI_upper = apply(
    bootstrap_results, 2, quantile, probs = 0.975
  )
)

bootstrap_summary

# Create calibration dataset
calibration_data <- data.frame(
  actual = test_data_ordered$won,
  original = test_prob_original,
  extended = test_prob_extended
)

# Define 10 probability bins
breaks <- seq(0, 1, by = 0.1)

calibration_data$original_bin <- cut(
  calibration_data$original,
  breaks = breaks,
  include.lowest = TRUE
)

calibration_data$extended_bin <- cut(
  calibration_data$extended,
  breaks = breaks,
  include.lowest = TRUE
)

# Calibration summary function
calibration_summary <- function(data, prediction, bin) {
  
  data %>%
    group_by(.data[[bin]]) %>%
    summarise(
      n = n(),
      mean_predicted = mean(.data[[prediction]]),
      observed_frequency = mean(actual),
      .groups = "drop"
    )
}

# Original model
calibration_original <- calibration_summary(
  calibration_data,
  "original",
  "original_bin"
)

# Extended model
calibration_extended <- calibration_summary(
  calibration_data,
  "extended",
  "extended_bin"
)

calibration_original
calibration_extended
#Reliability diagrams
library(ggplot2)
library(dplyr)

calibration_plot_data <- bind_rows(
  calibration_original %>%
    transmute(
      Model = "Original",
      mean_predicted,
      observed_frequency,
      n
    ),
  
  calibration_extended %>%
    transmute(
      Model = "Extended",
      mean_predicted,
      observed_frequency,
      n
    )
)

ggplot(
  calibration_plot_data,
  aes(
    x = mean_predicted,
    y = observed_frequency,
    color = Model
  )
) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    color = "black"
  ) +
  geom_point(aes(size = n)) +
  geom_line() +
  scale_x_continuous(limits = c(0, 0.6)) +
  scale_y_continuous(limits = c(0, 0.6)) +
  labs(
    title = "Calibration of Bayesian Horse Racing Models",
    subtitle = "Chronological test set: 2003–2005",
    x = "Mean predicted win probability",
    y = "Observed win frequency",
    color = "Model",
    size = "Number of runners"
  ) +
  theme_minimal()
#predicted horse rankings against actual finishing rankings.
class(test_data_ordered$result)

head(test_data_ordered$result, 20)

sort(table(test_data_ordered$result), decreasing = TRUE)[1:20]
table(
  result = test_data_ordered$result,
  won = test_data_ordered$won
)
ranking_check <- test_data_ordered %>%
  group_by(race_id) %>%
  summarise(
    field_size = n(),
    unique_positions = n_distinct(result),
    min_position = min(result, na.rm = TRUE),
    max_position = max(result, na.rm = TRUE),
    missing_positions = sum(is.na(result)),
    .groups = "drop"
  )

table(ranking_check$field_size == ranking_check$unique_positions)

table(
  ranking_check$min_position == 1 &
    ranking_check$max_position == ranking_check$field_size
)

sum(ranking_check$missing_positions)
# Identify races with duplicate finishing positions
races_with_ties <- ranking_check %>%
  filter(field_size != unique_positions) %>%
  pull(race_id)

length(races_with_ties)

# Inspect the first 5 affected races
test_data_ordered %>%
  filter(race_id %in% head(races_with_ties, 5)) %>%
  select(race_id, horse_id, result, won) %>%
  arrange(race_id, result)

tie_distribution <- test_data_ordered %>%
  group_by(race_id, result) %>%
  summarise(
    horses_at_position = n(),
    .groups = "drop"
  ) %>%
  filter(horses_at_position > 1) %>%
  count(result, horses_at_position, name = "number_of_groups") %>%
  arrange(result, horses_at_position)

tie_distribution

library(dplyr)

ranking_data <- test_data_ordered %>%
  mutate( actual_rank = NA_real_,
    predicted_rank_original = NA_real_,
    predicted_rank_extended = NA_real_
  )

for (r in seq_len(nrow(test_race_structure))) {
  
  idx <- test_race_structure$start[r]:
    test_race_structure$end[r]
  
  # Actual finishing rank within this race
  ranking_data$actual_rank[idx] <-
    rank(
      ranking_data$result[idx],
      ties.method = "average"
    )
  
  # Original model ranking
  ranking_data$predicted_rank_original[idx] <-
    rank(
      -test_prob_original[idx],
      ties.method = "average"
    )
  
  # Extended model ranking
  ranking_data$predicted_rank_extended[idx] <-
    rank(
      -test_prob_extended[idx],
      ties.method = "average"
    )
}

# Inspect the first race
ranking_data %>%
  filter(race_id == test_race_structure$race_id[1]) %>%
  select(
    race_id,
    horse_id,
    result,
    actual_rank,
    predicted_rank_original,
    predicted_rank_extended
  ) %>%
  arrange(predicted_rank_extended)
#Ranking performence
ranking_metrics <- ranking_data %>%
  group_by(race_id) %>%
  summarise(
    spearman_original = cor(
      actual_rank,
      predicted_rank_original,
      method = "spearman"
    ),
    
    spearman_extended = cor(
      actual_rank,
      predicted_rank_extended,
      method = "spearman"
    ),
    
    kendall_original = cor(
      actual_rank,
      predicted_rank_original,
      method = "kendall"
    ),
    
    kendall_extended = cor(
      actual_rank,
      predicted_rank_extended,
      method = "kendall"
    ),
    
    .groups = "drop"
  )

# Average ranking performance across races
ranking_metrics %>%
  summarise(
    across(
      everything(),
      mean,
      na.rm = TRUE
    )
  )
#95%CI to access whether there is any difference for both the models
set.seed(20261119)

B <- 2000
n_races <- nrow(ranking_metrics)

spearman_difference <- with(
  ranking_metrics,
  spearman_extended - spearman_original
)

bootstrap_spearman <- replicate(
  B,
  mean(
    sample(
      spearman_difference,
      size = n_races,
      replace = TRUE
    ),
    na.rm = TRUE
  )
)

c(
  Mean_difference = mean(spearman_difference, na.rm = TRUE),
  CI_lower = quantile(bootstrap_spearman, 0.025, na.rm = TRUE),
  CI_upper = quantile(bootstrap_spearman, 0.975, na.rm = TRUE)
)
#how highly each model ranks the actual winner on average.

winner_ranking_metrics <- ranking_data %>%
  filter(result == 1) %>%
  summarise(
    # Mean predicted rank of actual winner
    mean_rank_original = mean(predicted_rank_original),
    mean_rank_extended = mean(predicted_rank_extended),
    
    # Median predicted rank of actual winner
    median_rank_original = median(predicted_rank_original),
    median_rank_extended = median(predicted_rank_extended),
    
    # Percentage of actual winners ranked in top 3
    top3_original = mean(predicted_rank_original <= 3) * 100,
    top3_extended = mean(predicted_rank_extended <= 3) * 100
  )

winner_ranking_metrics
ranking_data %>%
  filter(result == 1) %>%
  summarise(
    top1_original = mean(predicted_rank_original == 1) * 100,
    top1_extended = mean(predicted_rank_extended == 1) * 100
  )
#Winner identification performance
library(ggplot2)
library(tidyr)
library(dplyr)

winner_accuracy <- data.frame(
  Model = c("Original", "Extended"),
  Top1 = c(19.1, 19.9),
  Top3 = c(42.4, 42.5)
)

winner_accuracy_long <- winner_accuracy %>%
  pivot_longer(
    cols = c(Top1, Top3),
    names_to = "Metric",
    values_to = "Accuracy"
  )

ggplot(
  winner_accuracy_long,
  aes(x = Metric, y = Accuracy, fill = Model)
) +
  geom_col(
    position = position_dodge(0.7),
    width = 0.6
  ) +
  geom_text(
    aes(label = sprintf("%.1f%%", Accuracy)),
    position = position_dodge(0.7),
    vjust = -0.5,
    size = 4
  ) +
  labs(
    title = "Winner Identification Performance",
    x = NULL,
    y = "Accuracy (%)",
    fill = "Model"
  ) +
  theme_minimal(base_size = 13)
#Mean predicted rank of actual winners
winner_rank_plot <- data.frame(
  Model = c("Original", "Extended"),
  Mean_Rank = c(5.04, 4.97)
)

ggplot(
  winner_rank_plot,
  aes(x = Model, y = Mean_Rank, fill = Model)
) +
  geom_col(width = 0.55) +
  geom_text(
    aes(label = sprintf("%.2f", Mean_Rank)),
    vjust = -0.5,
    size = 5
  ) +
  labs(
    title = "Average Predicted Rank of Actual Winners",
    x = NULL,
    y = "Mean predicted rank"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
#Full-field ranking correlation
ranking_comparison <- data.frame(
  Metric = c("Spearman", "Kendall"),
  Original = c(0.268, 0.195),
  Extended = c(0.264, 0.192)
)

ranking_long <- ranking_comparison %>%
  pivot_longer(
    cols = c(Original, Extended),
    names_to = "Model",
    values_to = "Correlation"
  )

ggplot(
  ranking_long,
  aes(x = Metric, y = Correlation, fill = Model)
) +
  geom_col(
    position = position_dodge(0.7),
    width = 0.6
  ) +
  geom_text(
    aes(label = sprintf("%.3f", Correlation)),
    position = position_dodge(0.7),
    vjust = -0.5,
    size = 4
  ) +
  labs(
    title = "Full-Field Ranking Performance",
    x = NULL,
    y = "Mean rank correlation",
    fill = "Model"
  ) +
  theme_minimal(base_size = 13)
#Interpret experience effects in the temporal training model
experience_posterior <- fit_train_extended$summary(
  variables = "beta"
)

experience_posterior %>%
  filter(
    variable %in% paste0("beta[", 30:34, "]")
  ) %>%
  select(
    variable,
    mean,
    median,
    sd,
    q5,
    q95,
    rhat,
    ess_bulk,
    ess_tail
  )
full_experience_summary <- experience_fit$summary(
  variables = "beta"
)

train_experience_summary <- fit_train_extended$summary(
  variables = "beta"
)

experience_comparison <- data.frame(
  Experience = c(
    "1-5 starts",
    "6-10 starts",
    "11-20 starts",
    "21-40 starts",
    "41+ starts"
  ),
  
  Full_Data = full_experience_summary$mean[30:34],
  
  Training_Data = train_experience_summary$mean[30:34]
)

experience_comparison
sigma_summary <- fit_train_extended$summary(
  variables = c(
    "sigma_horse",
    "sigma_jockey",
    "sigma_trainer"
  )
)

sigma_summary %>%
  select(
    variable,
    mean,
    median,
    sd,
    q5,
    q95,
    rhat,
    ess_bulk,
    ess_tail
  )
library(ggplot2)
library(dplyr)

sigma_plot_data <- sigma_summary %>%
  mutate(
    Group = c("Horse", "Jockey", "Trainer")
  )

ggplot(
  sigma_plot_data,
  aes(
    x = reorder(Group, mean),
    y = mean
  )
) +
  geom_point(size = 4) +
  geom_errorbar(
    aes(ymin = q5, ymax = q95),
    width = 0.15,
    linewidth = 0.8
  ) +
  coord_flip() +
  labs(
    title = "Posterior Random-Effect Standard Deviations",
    subtitle = "90% credible intervals",
    x = NULL,
    y = "Posterior standard deviation"
  ) +
  theme_minimal(base_size = 13)

fit_train_extended$variables()
class(fit_train_extended)

names(fit_train_extended)

fit_train_extended$metadata()
fit_train_extended$draws(format = "draws_df") %>%
  posterior::variables() %>%
  head(100)
all_variables <- fit_train_extended$draws(format = "draws_df") %>%
  posterior::variables()

grep(
  "y_rep|log_lik|prob|winner|eta",
  all_variables,
  value = TRUE
)
tail(all_variables, 40)
# Number of saved linear predictors and log-likelihood values
length(grep("^eta\\[", all_variables))

length(grep("^log_lik\\[", all_variables))

# Training data dimensions
dim(train_data_ordered)

# Training race structure
dim(race_structure)

head(race_structure)

# Confirm one winner per training race
table(race_structure$n_winners)
eta_draws_all <- fit_train_extended$draws(
  variables = "eta",
  format = "draws_array"
)

eta_draws_100 <- posterior::subset_draws(
  eta_draws_all,
  iteration = seq(1, 1000, by = 40)
)

dim(eta_draws_100)

set.seed(123)

n_draws <- 100
n_races <- nrow(race_structure)

y_rep <- matrix(
  NA_integer_,
  nrow = n_draws,
  ncol = n_races
)

for (d in seq_len(n_draws)) {
  
  eta_d <- as.numeric(
    eta_draws_100[
      (d - 1) %% 25 + 1,
      (d - 1) %/% 25 + 1,
    ]
  )
  
  for (r in seq_len(n_races)) {
    
    idx <- race_structure$start[r]:
      race_structure$end[r]
    
    eta_r <- eta_d[idx]
    
    # Numerically stable softmax
    exp_eta <- exp(eta_r - max(eta_r))
    prob_r <- exp_eta / sum(exp_eta)
    
    # Simulate exactly one winner
    y_rep[d, r] <- sample(
      seq_along(idx),
      size = 1,
      prob = prob_r
    )
  }
}
dim(y_rep)

head(y_rep[, 1:10])

all(y_rep >= 1)
all(y_rep <= race_structure$end - race_structure$start + 1)
#Calculate the proportion of simulated winners at each runner position
# Simulated winner-position distribution
simulated_position_distribution <- prop.table(
  table(factor(
    as.vector(y_rep),
    levels = 1:max(race_structure$end - race_structure$start + 1)
  ))
)
# Actual winner's posterior probability for each race
actual_winner_prob <- numeric(nrow(race_structure))

for (r in seq_len(nrow(race_structure))) {
  
  start_r <- race_structure$start[r]
  end_r   <- race_structure$end[r]
  
  winner_local <- race_structure$winner[r] - start_r + 1
  
  # Extract posterior eta draws for this race
  eta_r <- eta_draws_100[, , start_r:end_r, drop = FALSE]
  
  # Calculate softmax for each draw
  for (chain in 1:4) {
    for (iteration in 1:25) {
      
      eta_values <- as.numeric(
        eta_r[iteration, chain, ]
      )
      
      exp_eta <- exp(eta_values - max(eta_values))
      probabilities <- exp_eta / sum(exp_eta)
      
      if (iteration == 1 && chain == 1) {
        winner_prob_draws <- numeric(100)
      }
      
      draw_number <- (chain - 1) * 25 + iteration
      
      winner_prob_draws[draw_number] <-
        probabilities[winner_local]
    }
  }
  
  actual_winner_prob[r] <- mean(winner_prob_draws)
}
summary(actual_winner_prob)

head(actual_winner_prob)

length(actual_winner_prob)

all(is.finite(actual_winner_prob))
#compare observed winners with the model's most probable horses
top1_hits <- numeric(100)

for (d in seq_len(100)) {
  
  iteration <- (d - 1) %% 25 + 1
  chain <- (d - 1) %/% 25 + 1
  
  eta_d <- as.numeric(
    eta_draws_100[iteration, chain, ]
  )
  
  hits <- 0
  
  for (r in seq_len(nrow(race_structure))) {
    
    idx <- race_structure$start[r]:race_structure$end[r]
    
    eta_r <- eta_d[idx]
    
    predicted_winner_local <- which.max(eta_r)
    
    actual_winner_local <-
      race_structure$winner[r] -
      race_structure$start[r] + 1
    
    if (predicted_winner_local == actual_winner_local) {
      hits <- hits + 1
    }
  }
  
  top1_hits[d] <- hits / nrow(race_structure)
}

summary(top1_hits)
#Next: compare observed accuracy with replicated accuracy
replicated_top1 <- numeric(100)

for (d in seq_len(100)) {
  
  iteration <- (d - 1) %% 25 + 1
  chain <- (d - 1) %/% 25 + 1
  
  eta_d <- as.numeric(
    eta_draws_100[iteration, chain, ]
  )
  
  hits <- 0
  
  for (r in seq_len(nrow(race_structure))) {
    
    idx <- race_structure$start[r]:race_structure$end[r]
    
    predicted_winner_local <- which.max(eta_d[idx])
    
    if (y_rep[d, r] == predicted_winner_local) {
      hits <- hits + 1
    }
  }
  
  replicated_top1[d] <- hits / nrow(race_structure)
}

summary(replicated_top1)
#Let's perform a more informative PPC: winner probability
replicated_winner_prob <- matrix(
  NA_real_,
  nrow = 100,
  ncol = nrow(race_structure)
)

for (d in seq_len(100)) {
  
  iteration <- (d - 1) %% 25 + 1
  chain <- (d - 1) %/% 25 + 1
  
  eta_d <- as.numeric(
    eta_draws_100[iteration, chain, ]
  )
  
  for (r in seq_len(nrow(race_structure))) {
    
    idx <- race_structure$start[r]:race_structure$end[r]
    
    eta_r <- eta_d[idx]
    
    exp_eta <- exp(eta_r - max(eta_r))
    prob_r <- exp_eta / sum(exp_eta)
    
    simulated_winner_local <- y_rep[d, r]
    
    replicated_winner_prob[d, r] <-
      prob_r[simulated_winner_local]
  }
}

summary(as.vector(replicated_winner_prob))
#vizualize
library(ggplot2)
library(dplyr)
library(tidyr)

ppc_probability_data <- data.frame(
  actual = actual_winner_prob,
  replicated = colMeans(replicated_winner_prob)
) %>%
  pivot_longer(
    cols = everything(),
    names_to = "type",
    values_to = "probability"
  )

ggplot(
  ppc_probability_data,
  aes(x = probability, fill = type)
) +
  geom_density(alpha = 0.4) +
  labs(
    title = "Posterior Predictive Check: Winner Probabilities",
    x = "Probability assigned to the winner",
    y = "Density",
    fill = "Winner type"
  ) +
  theme_minimal()
#Calculate posterior mean win probabilities for every training horse
n_obs <- nrow(train_data_ordered)
n_races <- nrow(race_structure)

train_prob_draws <- matrix(
  NA_real_,
  nrow = 100,
  ncol = n_obs
)

for (d in seq_len(100)) {
  
  iteration <- (d - 1) %% 25 + 1
  chain <- (d - 1) %/% 25 + 1
  
  eta_d <- as.numeric(
    eta_draws_100[iteration, chain, ]
  )
  
  for (r in seq_len(n_races)) {
    
    idx <- race_structure$start[r]:race_structure$end[r]
    
    eta_r <- eta_d[idx]
    
    exp_eta <- exp(eta_r - max(eta_r))
    prob_r <- exp_eta / sum(exp_eta)
    
    train_prob_draws[d, idx] <- prob_r
  }
}

train_mean_prob <- colMeans(train_prob_draws)

summary(train_mean_prob)

#Create calibration groups

calibration_data <- data.frame(
  predicted_prob = train_mean_prob,
  actual_win = train_data_ordered$won
)

calibration_data <- calibration_data %>%
  mutate(
    probability_group = ntile(predicted_prob, 10)
  ) %>%
  group_by(probability_group) %>%
  summarise(
    horses = n(),
    mean_predicted = mean(predicted_prob),
    observed_win_rate = mean(actual_win),
    .groups = "drop"
  )

calibration_data
#quantify the calibration error
calibration_data <- calibration_data %>%
  mutate(
    calibration_error = abs(
      mean_predicted - observed_win_rate
    )
  )

ECE <- weighted.mean(
  calibration_data$calibration_error,
  calibration_data$horses
)

MCE <- max(calibration_data$calibration_error)

cat("Expected Calibration Error:", ECE, "\n")
cat("Maximum Calibration Error:", MCE, "\n")
#on test data
length(test_prob_extended)
nrow(test_data_ordered)

table(test_data_ordered$won)
library(dplyr)

test_calibration_data <- data.frame(
  predicted_prob = test_prob_extended,
  actual_win = test_data_ordered$won
) %>%
  mutate(
    probability_group = ntile(predicted_prob, 10)
  ) %>%
  group_by(probability_group) %>%
  summarise(
    horses = n(),
    mean_predicted = mean(predicted_prob),
    observed_win_rate = mean(actual_win),
    calibration_error = mean_predicted - observed_win_rate,
    .groups = "drop"
  )

test_calibration_data
# test_ECE <- weighted.mean(
#   abs(test_calibration_data$calibration_error),
#   test_calibration_data$horses
# )

# test_MCE <- max(
#   abs(test_calibration_data$calibration_error)
# )

# cat("Test ECE:", test_ECE, "\n")
# cat("Test MCE:", test_MCE, "\n")
# save.image(
#   file = "E:/Horse_Racing_Project/full_workspace_backup.RData"
# )
# file.info("E:/Horse_Racing_Project/full_workspace_backup.RData")$size / 1024^2
# load("E:/Horse_Racing_Project/full_workspace_backup.RData")

# ls()
# gc()
# exists("fit_train_original")
# exists("fit_train_extended")
# exists("test_prob_extended")


