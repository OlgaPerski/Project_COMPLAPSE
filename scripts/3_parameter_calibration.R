library(here)
library(tidyverse)
library(FME)

# prepare for sampling ----------------------------------------------------

# restrict the ranges of the parameter values
beta1_min <- 0.9
beta1_max <- 1
beta2_min <- 0
beta2_max <- 0.2
beta3_min <- 0
beta3_max <- 0.6
gamma1_min <- 0
gamma1_max <- 0.2
delta1_min <- 0.9
delta1_max <- 1
delta2_min <- 1
delta2_max <- 4
epsilon1_min <- 0
epsilon1_max <- 0.2
zeta1_min <- 0.9
zeta1_max <- 0.1
zeta2_min <- 1
zeta2_max <- 4
eta1_min <- 0.4
eta1_max <- 0.6
eta2_min <- 0.3
eta2_max <- 0.6
eta3_min <- 0.3
eta3_max <- 0.6
eta4_min <- 0.1
eta4_max <- 0.6
eta5_min <- 0.3
eta5_max <- 0.6
theta1_min <- 0.1
theta1_max <- 0.5
iota1_min <- 1.0015
iota1_max <- 1.2
iota2_min <- 0.1
iota2_max <- 0.6
kappa1_min <- 0.9
kappa1_max <- 1
kappa2_min <- 0.5
kappa2_max <- 1.5
lambda1_min <- 0.2
lambda1_max <- 0.6
lambda2_min <- 0.2
lambda2_max <- 0.6
lambda3_min <- 0.1
lambda3_max <- 0.3
lambda4_min <- 0.1
lambda4_max <- 0.3

# define parameter ranges as a data frame
parRange <- data.frame(
  min = c(beta1_min, beta2_min, beta3_min, gamma1_min, 
          delta1_min, delta2_min, epsilon1_min, 
          zeta1_min, zeta2_min,
          eta1_min, eta2_min, eta3_min, eta4_min, eta5_min, 
          theta1_min, iota1_min, iota2_min, 
          kappa1_min, kappa2_min,
          lambda1_min, lambda2_min, lambda3_min, lambda4_min), 
  max = c(beta1_max, beta2_max, beta3_max, gamma1_max, 
          delta1_max, delta2_max, epsilon1_max, 
          zeta1_max, zeta2_max,
          eta1_max, eta2_max, eta3_max, eta4_max, eta5_max, 
          theta1_max, iota1_max, iota2_max, 
          kappa1_max, kappa2_max,
          lambda1_max, lambda2_max, lambda3_max, lambda4_max))

rownames(parRange) <- c("beta1", "beta2", "beta3", "gamma1", 
                        "delta1", "delta2", "epsilon1",
                        "zeta1", "zeta2", 
                        "eta1", "eta2", "eta3", "eta4", "eta5", 
                        "theta1", "iota1", "iota2", 
                        "kappa1", "kappa2",
                        "lambda1", "lambda2", "lambda3", "lambda4")

# perform Latin hypercube sampling ----------------------------------------

lhs_samples <- Latinhyper(parRange, 1000)

# convert the matrix to a data frame
lhs_df <- as.data.frame(lhs_samples)

# set the column names
colnames(lhs_df) <- rownames(parRange)

# initial model checks to see if it can generate the phenomena ------------

source(here("scripts", "2_model_functions.R"))

a <- lhs_df %>%
  pivot_longer(cols = everything(), names_to = "parameter", values_to = "values") %>%
  mutate(sample = rep(1:1000, each = 23))

default_parameters <- list(
  nTime = 2880,
  alpha = 0.5,
  beta1 = 0.997,
  beta2 = 0.02,
  beta3 = 0.1,
  gamma1 = 0.005,
  delta1 = 0.98,
  delta2 = 4,
  epsilon1 = 0.1,
  zeta1 = 0.95,
  zeta2 = 4,
  eta1 = 0.4,
  eta2 = 0.3,
  eta3 = 0.3,
  eta4 = 0.1,
  eta5 = 0.3,
  theta1 = 0.5,
  iota1 = 1.0015,
  iota2 = 0.1,
  kappa1 = 0.9,
  kappa2 = 1.2,
  lambda1 = 0.1,
  lambda2 = 0.2,
  lambda3 = 0.3,
  lambda4 = 0.4,
  str_scenario = 0,
  NW_init = 8,
  ES_init = 3,
  CR_init = 3,
  SE_init = 6,
  St_init = matrix(0, nrow = 1, ncol = 3))

lhs_model_output <- list()

for(z in 1:1000) {
  
  b <- a %>%
    filter(sample == z)
  
  parameters <- setNames(b$values, b$parameter)
  
  # replace the values in the list with the ones from the named vector
  for (param_name in names(parameters)) {
    if (param_name %in% names(default_parameters)) {
      default_parameters[param_name] <- parameters[[param_name]]
    }
  }
  
  try(lhs_model_output[[z]] <- simulate_data(params = default_parameters))
  
}

# check if the model can generate the three phenomena of interest ('relapse', 'prolapse', 'abstinent')

counts <- sapply(lhs_model_output, function(df) sum(df[[6]] == 1))
summary(counts)

# find the indices of elements with many counts ('relapse')
many_count_indices <- which(counts > 10 & counts < 30)

# print the indices
print(many_count_indices)

# print the params

print(lhs_df[11,])

# find the indices of elements with a few counts ('prolapse')
few_count_indices <- which(counts == 3)

# print the indices
print(few_count_indices)

# print the params

print(lhs_df[12,])

# find the indices of elements with zero counts ('abstinent')
zero_count_indices <- which(counts == 0)

# print the indices
print(zero_count_indices)

# print the params

print(lhs_df[23,])

# parameter calibration ---------------------------------------------------

relapse_obs <- rbinom(2880, 1, 0.009)
prolapse_obs <- rbinom(2880, 1, 0.002)
abstinence_obs <- rbinom(2880, 1, 0)

predicted <- lapply(lhs_model_output, function(df) df[, 6])

sum_of_lapses <- sapply(predicted, sum)

# best fitting parameters ('relapse')

relapse_diff <- lapply(predicted, function(col) sum(abs(col - relapse_obs)))

relapse_valid_indices <- which(sum_of_lapses > 20)

relapse_min_valid_index <- relapse_valid_indices[which.min(unlist(relapse_diff)[relapse_valid_indices])]

parameter_series_to_use <- 11

relapse_best_model <- lhs_df %>%
  mutate(id = c(1:1000)) %>%
  filter(id == parameter_series_to_use)

relapse_params <- default_parameters

# replace values in the list with the ones from the named vector
for (param_name in names(relapse_best_model)) {
  if (param_name %in% names(relapse_params)) {
    relapse_params[param_name] <- relapse_best_model[[param_name]]
  }
}

n_runs <- 1
relapse_model_vis <- list()
for(i in 1:n_runs) {
  relapse_model_vis[[i]] <- simulate_data(params = relapse_params)
}
relapse_model_vis <- bind_rows(relapse_model_vis, .id = "run")

relapse_plot <- ggplot() +
  geom_line(data = relapse_model_vis, aes(x = nTime, y = smok, group = run), colour = "darkred", linetype = "dashed", linewidth = 0.7) + 
  geom_line(data = tibble(nTime = 1:default_parameters$nTime,
                          smok = relapse_obs), aes(x = nTime, y = smok), colour = "black", linewidth = 0.75) +
  labs(title = "Predicted vs. observed lapses (RELAPSE)",
       x = "Time",
       y = "Lapse") +
  scale_y_continuous(breaks = c(0, 1), limits = c(0, 1)) +
  theme_bw()

# best fitting parameters ('prolapse')

prolapse_diff <- lapply(predicted, function(col) sum(abs(col - prolapse_obs)))

prolapse_valid_indices <- which(sum_of_lapses > 2)

prolapse_min_valid_index <- prolapse_valid_indices[which.min(unlist(prolapse_diff)[prolapse_valid_indices])]

prolapse_parameter_series_to_use <- 12

prolapse_best_model <- lhs_df %>%
  mutate(id = c(1:1000)) %>%
  filter(id == prolapse_parameter_series_to_use)

# replace values in the list with the ones from the named vector
prolapse_params <- default_parameters
for (param_name in names(prolapse_best_model)) {
  if (param_name %in% names(prolapse_params)) {
    prolapse_params[param_name] <- prolapse_best_model[[param_name]]
  }
}
prolapse_model_vis <- list()
for(i in 1:n_runs) {
  prolapse_model_vis[[i]] <- simulate_data(params = prolapse_params)
}
prolapse_model_vis <- bind_rows(prolapse_model_vis, .id = "run")

prolapse_plot <- ggplot() +
  geom_line(data = prolapse_model_vis, aes(x = nTime, y = smok, group = run), colour = "darkred", linetype = "dashed", linewidth = 0.7) +
  geom_line(data = tibble(nTime = 1:default_parameters$nTime,
                          smok = prolapse_obs), aes(x = nTime, y = smok), colour = "black", linewidth = 0.75) +
  labs(title = "Predicted vs. observed lapses (PROLAPSE)",
       x = "Time",
       y = "Lapse") +
  scale_y_continuous(breaks = c(0, 1), limits = c(0, 1)) +
  theme_bw()

# best fitting parameters ('abstinent')
abstinence_diff <- lapply(predicted, function(col) sum(abs(col - abstinence_obs)))

abstinence_min_index <- which.min(unlist(abstinence_diff)) 

abstinence_parameter_series_to_use <- 23

abstinence_best_model <- lhs_df %>%
  mutate(id = c(1:1000)) %>%
  filter(id == abstinence_parameter_series_to_use)

# replace values in the list with the ones from the named vector
abstinence_params <- default_parameters
for (param_name in names(abstinence_best_model)) {
  if (param_name %in% names(abstinence_params)) {
    abstinence_params[param_name] <- abstinence_best_model[[param_name]]
  }
}

abstinence_model_vis <- list()
for(i in 1:n_runs) {
  abstinence_model_vis[[i]] <- simulate_data(params = abstinence_params)
}
abstinence_model_vis <- bind_rows(abstinence_model_vis, .id = "run")

abstinence_plot <- ggplot() +
  geom_line(data = abstinence_model_vis, aes(x = nTime, y = smok, group = run), colour = "darkred", linetype = "dashed", linewidth = 0.7) +
  geom_line(data = tibble(nTime = 1:default_parameters$nTime,
                          smok = abstinent_obs), aes(x = nTime, y = smok), colour = "black", linewidth = 0.75) +
  labs(title = "Predicted vs. observed lapses (ABSTAIN)",
       x = "Time",
       y = "Lapse") +
  scale_y_continuous(breaks = c(0, 1), limits = c(0, 1)) +
  theme_bw()

combined_plot <- plot_grid(relapse_plot, prolapse_plot, abstinence_plot, nrow = 1)
cowplot::save_plot(plot = combined_plot, filename = here("scripts", "parameter_calibration_plot.png"), base_height = 8, base_width = 24)