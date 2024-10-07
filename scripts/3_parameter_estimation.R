library(here)
library(tidyverse)
library(FME)
library(ggplot2)
library(cowplot)
library(dgof)

# prepare for sampling ----------------------------------------------------

# set up the plausible range for the parameter values

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
iota1_max <- 1.205
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

# define parameter ranges as a data frame
parRange <- data.frame(
  min = c(beta1_min, beta2_min, beta3_min, gamma1_min, 
          delta1_min, delta2_min, epsilon1_min, 
          zeta1_min, zeta2_min,
          eta1_min, eta2_min, eta3_min, eta4_min, eta5_min, 
          theta1_min, iota1_min, iota2_min, 
          kappa1_min, kappa2_min,
          lambda1_min, lambda2_min, lambda3_min), 
  max = c(beta1_max, beta2_max, beta3_max, gamma1_max, 
          delta1_max, delta2_max, epsilon1_max, 
          zeta1_max, zeta2_max,
          eta1_max, eta2_max, eta3_max, eta4_max, eta5_max, 
          theta1_max, iota1_max, iota2_max, 
          kappa1_max, kappa2_max,
          lambda1_max, lambda2_max, lambda3_max))

rownames(parRange) <- c("beta1", "beta2", "beta3", "gamma1", 
                        "delta1", "delta2", "epsilon1",
                        "zeta1", "zeta2", 
                        "eta1", "eta2", "eta3", "eta4", "eta5", 
                        "theta1", "iota1", "iota2", 
                        "kappa1", "kappa2",
                        "lambda1", "lambda2", "lambda3")

# perform Latin hypercube sampling ----------------------------------------

lhs_samples <- Latinhyper(parRange, 1000)
lhs_df <- as.data.frame(lhs_samples)
saveRDS(lhs_df, here("scripts", "lhs_df.rds"))
colnames(lhs_df) <- rownames(parRange)

# initial model checks to see if it can generate the phenomena ------------

source(here("scripts", "2_model_functions.R"))

lhs_df <- readRDS(here("scripts", "lhs_df.rds"))

a <- lhs_df %>%
  pivot_longer(cols = everything(), names_to = "parameter", values_to = "values") %>%
  mutate(sample = rep(1:1000, each = 22))

# set the default params to the middle of their plausible range

default_parameters <- list(
  nTime = 8064,
  alpha = 0,
  beta1 = 0.95,
  beta2 = 0.1,
  beta3 = 0.3,
  gamma1 = 0.1,
  delta1 = 0.95,
  delta2 = 2,
  epsilon1 = 0.1,
  zeta1 = 0.45,
  zeta2 = 2,
  eta1 = 0.5,
  eta2 = 0.45,
  eta3 = 0.35,
  eta4 = 0.35,
  eta5 = 0.45,
  theta1 = 0.25,
  iota1 = 1.105,
  iota2 = 0.35,
  kappa1 = 0.95,
  kappa2 = 1,
  lambda1 = 0.3,
  lambda2 = 0.3,
  lambda3 = 0.2,
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

saveRDS(lhs_model_output, here("scripts", "lhs_model_output.rds"))

# check correspondence between the model outputs and the known distributions for the three phenomena of interest ('relapse', 'prolapse', 'abstinence')

### relapse distribution

relapse_distr <- rbinom(8064, 1, 0.005)

### prolapse distribution

n <- 8064
lambda <- 0.2  # rate of exponential decay

# monotonically decreasing probability function

decreasing_prob <- function(i, lambda) {
  
  exp(-lambda * i)
  
}

# generate probabilities for each index (monotonically decreasing)

probabilities <- sapply(1:n, decreasing_prob, lambda = lambda)

# generate a binary sequence based on these probabilities

prolapse_distr <- rbinom(n, size = 1, prob = probabilities)

### abstinence distribution

abstinence_distr <- rbinom(8064, 1, 0)

# distribution tests

lhs_model_output <- readRDS(here("scripts", "lhs_model_output.rds"))

sim_distr_test_relapse <- lapply(lhs_model_output, function(df) dgof::ks.test(df[[6]], relapse_distr))
sim_distr_test_prolapse <- lapply(lhs_model_output, function(df) dgof::ks.test(df[[6]], prolapse_distr))
sim_distr_test_abstinence <- lapply(lhs_model_output, function(df) dgof::ks.test(df[[6]], abstinence_distr))

lapse_count <- sapply(lhs_model_output, function(df) sum(df[[6]]))

# find the index of the first model output which corresponds to the known relapse distribution

p_values_relapse <- round(sapply(sim_distr_test_relapse, function(test) test$p.value), 3)

first_relapse <- which(p_values_relapse > 0.05 & lapse_count > 5)[1]

if (!is.na(first_relapse)) {
  print(paste("First p-value greater than 0.05 with non-zero lapse count is at index:", first_relapse))
  print(paste("P-value:", p_values_relapse[first_relapse]))
} else {
  print("No p-values greater than 0.05 with non-zero lapse count found")
}

# find the index of the first model output which corresponds to the prolapse distribution

p_values_prolapse <- round(sapply(sim_distr_test_prolapse, function(test) test$p.value), 3)

first_prolapse <- which(p_values_prolapse > 0.05 & lapse_count > 3 & lapse_count < 5)[1]

if (!is.na(first_prolapse)) {
  print(paste("First p-value greater than 0.05 with non-zero lapse count is at index:", first_prolapse))
  print(paste("P-value:", p_values_prolapse[first_prolapse]))
} else {
  print("No p-values greater than 0.05 with non-zero lapse count found")
}

# find the index of the first model output which corresponds to the abstinence distribution
                         
p_values_abstinence <- round(sapply(sim_distr_test_abstinence, function(test) test$p.value), 3)

first_abstinence <- which(p_values_abstinence > 0.05 & lapse_count == 0)[1] 

if (!is.na(first_abstinence)) {
  print(paste("First p-value greater than 0.05 with zero lapse count is at index:", first_abstinence))
  print(paste("P-value:", p_values_abstinence[first_abstinence]))
} else {
  print("No p-values greater than 0.05 with zero lapse count found")
}

# plot the three phenomena

phenomenon_1 <- ggplot() +
  geom_line(data = tibble(nTime = 1:default_parameters$nTime,
                          smok = relapse_distr), aes(x = nTime, y = smok), colour = "darkred", linewidth = 0.75) +
  labs(title = "a) Relapse",
       x = "Time (in 5-minute ticks)",
       y = "Lapse") +
  scale_y_continuous(breaks = c(0, 1), limits = c(0, 1)) +
  theme_bw()

phenomenon_2 <- ggplot() +
  geom_line(data = tibble(nTime = 1:default_parameters$nTime,
                          smok = prolapse_distr), aes(x = nTime, y = smok), colour = "darkred", linewidth = 0.75) +
  labs(title = "b) Prolapse",
       x = "Time (in 5-minute ticks)",
       y = "Lapse") +
  scale_y_continuous(breaks = c(0, 1), limits = c(0, 1)) +
  theme_bw()

phenomenon_3 <- ggplot() +
  geom_line(data = tibble(nTime = 1:default_parameters$nTime,
                          smok = abstinence_distr), aes(x = nTime, y = smok), colour = "darkred", linewidth = 0.75) +
  labs(title = "c) Abstinence",
       x = "Time (in 5-minute ticks)",
       y = "Lapse") +
  scale_y_continuous(breaks = c(0, 1), limits = c(0, 1)) +
  theme_bw()

combined_phenomena_plot <- plot_grid(phenomenon_1, phenomenon_2, phenomenon_3, ncol = 1)
cowplot::save_plot(plot = combined_phenomena_plot, filename = here("scripts", "combined_phenomena_plot.png"), base_height = 8, base_width = 12)

# plausible parameter set ('relapse')

relapse_parameter_set_to_use <- 9

relapse_model <- lhs_df %>%
  mutate(id = c(1:1000)) %>%
  filter(id == relapse_parameter_set_to_use)

print(relapse_model)

relapse_params <- default_parameters

# replace values in the list with the ones from the named vector
for (param_name in names(relapse_model)) {
  if (param_name %in% names(relapse_params)) {
    relapse_params[param_name] <- relapse_model[[param_name]]
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
                          smok = relapse_distr), aes(x = nTime, y = smok), colour = "black", linewidth = 0.75) +
  labs(title = "Predicted vs. observed lapses (Relapse)",
       x = "Time (in 5-minute ticks)",
       y = "Lapse") +
  scale_y_continuous(breaks = c(0, 1), limits = c(0, 1)) +
  theme_bw()

# plausible parameter set ('prolapse')

prolapse_parameter_set_to_use <- 68

prolapse_model <- lhs_df %>%
  mutate(id = c(1:1000)) %>%
  filter(id == prolapse_parameter_set_to_use)

print(prolapse_model)

# replace values in the list with the ones from the named vector
prolapse_params <- default_parameters
for (param_name in names(prolapse_model)) {
  if (param_name %in% names(prolapse_params)) {
    prolapse_params[param_name] <- prolapse_model[[param_name]]
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
                          smok = prolapse_distr), aes(x = nTime, y = smok), colour = "black", linewidth = 0.75) +
  labs(title = "Predicted vs. observed lapses (Prolapse)",
       x = "Time (in 5-minute ticks)",
       y = "Lapse") +
  scale_y_continuous(breaks = c(0, 1), limits = c(0, 1)) +
  theme_bw()

# plausible parameter set ('abstinent')

abstinence_parameter_set_to_use <- 2

abstinence_model <- lhs_df %>%
  mutate(id = c(1:1000)) %>%
  filter(id == abstinence_parameter_set_to_use)

print(abstinence_model)

# replace values in the list with the ones from the named vector
abstinence_params <- default_parameters
for (param_name in names(abstinence_model)) {
  if (param_name %in% names(abstinence_params)) {
    abstinence_params[param_name] <- abstinence_model[[param_name]]
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
                          smok = abstinence_distr), aes(x = nTime, y = smok), colour = "black", linewidth = 0.75) +
  labs(title = "Predicted vs. observed lapses (Abstinence)",
       x = "Time (in 5-minute ticks)",
       y = "Lapse") +
  scale_y_continuous(breaks = c(0, 1), limits = c(0, 1)) +
  theme_bw()

combined_plot <- plot_grid(relapse_plot, prolapse_plot, abstinence_plot, ncol = 1)
cowplot::save_plot(plot = combined_plot, filename = here("scripts", "parameter_estimation_plot.png"), base_height = 8, base_width = 8)
