library(here)
library(tidyverse)
library(ggplot2)
library(cowplot)

# read in csv
params_df <- read_csv(here("scripts", "params.csv"))

# convert the matrix to a data frame
params_df <- as.data.frame(params_df)

rownames <- c("gamma1", "delta2", "epsilon1", "zeta2", "eta2", "eta3", "theta1", "kappa2", "lambda1", "lambda2", "lambda3")

# set the column names
colnames(params_df) <- rownames

#load in the functions
source(here("scripts", "2_model_functions.R"))

a <- params_df %>%
  pivot_longer(cols = everything(), names_to = "parameter", values_to = "values") %>%
  mutate(sample = rep(1:20, each = 11))

default_parameters <- list(
  nTime = 2880,
  alpha = 0,
  beta1 = 0.91,
  beta2 = 0.17,
  beta3 = 0.5,
  gamma1 = 0.036,
  delta1 = 0.97,
  delta2 = 1.63,
  epsilon1 = 0.006,
  zeta1 = 0.94,
  zeta2 = 3.71,
  eta1 = 0.51,
  eta2 = 0.6,
  eta3 = 0.43,
  eta4 = 0.39,
  eta5 = 0.34,
  theta1 = 0.11,
  iota1 = 1.0015,
  iota2 = 0.15,
  kappa1 = 0.92,
  kappa2 = 0.96,
  lambda1 = 0.21,
  lambda2 = 0.57,
  lambda3 = 0.18,
  lambda4 = 0.18,
  str_scenario = 0,
  NW_init = 8,
  ES_init = 3,
  CR_init = 3,
  SE_init = 6,
  St_init = matrix(0, nrow = 1, ncol = 3))

sens_model_output <- list()

for(z in 1:20) {
  
  b <- a %>%
    filter(sample == z)
  
  parameters <- setNames(b$values, b$parameter)
  
  # replace the values in the list with the ones from the named vector
  for (param_name in names(parameters)) {
    if (param_name %in% names(default_parameters)) {
      default_parameters[param_name] <- parameters[[param_name]]
    }
  }
  
  try(sens_model_output[[z]] <- simulate_data(params = default_parameters))
  
}

# stressors and experienced stress ----------------------------------------

sens_subset_1 <- sens_model_output[1:5]

sens_subset_1_plot_list <- lapply(seq_along(sens_subset_1), function(i) {
  ggplot(sens_subset_1[[i]], aes(x = nTime)) +
    geom_line(aes(y = ES), linewidth = 0.75, colour = "black", linetype = "dashed") +
    geom_point(aes(y = smok), colour = "darkred", shape = 1, stroke = 1.2) +
    labs(title = paste(i),
         x = "Time",
         y = "Experienced stress") +
    theme_minimal() +
    ylim(0, 10)
})

sens_subset_1_combined_plot <- plot_grid(plotlist = sens_subset_1_plot_list, nrow = 2)
cowplot::save_plot(plot = sens_subset_1_combined_plot, filename = here("scripts", "sens_stress_combined_plot.png"), base_height = 8, base_width = 24)

# cigarette cues and cue reactivity ---------------------------------------

sens_subset_2 <- sens_model_output[6:10]

sens_subset_2_plot_list <- lapply(seq_along(sens_subset_2), function(i) {
  ggplot(sens_subset_2[[i]], aes(x = nTime)) +
    geom_line(aes(y = CR), linewidth = 0.75, colour = "black", linetype = "dashed") +
    geom_point(aes(y = smok), colour = "darkred", shape = 1, stroke = 1.2) +
    labs(title = paste(i),
         x = "Time",
         y = "Cue reactivity") +
    theme_minimal() +
    ylim(0, 10)
})

sens_subset_2_combined_plot <- plot_grid(plotlist = sens_subset_2_plot_list, nrow = 2)
cowplot::save_plot(plot = sens_subset_2_combined_plot, filename = here("scripts", "sens_cues_combined_plot.png"), base_height = 8, base_width = 24)

# perceived permissibility of smoking -------------------------------------

sens_subset_3 <- sens_model_output[11:15]

sens_subset_3_plot_list <- lapply(seq_along(sens_subset_3), function(i) {
  ggplot(sens_subset_3[[i]], aes(x = nTime)) +
    geom_line(aes(y = M), linewidth = 0.75, colour = "blue", linetype = "dashed") +
    geom_point(aes(y = transformed_PE), colour = "black", shape = 1, stroke = 1.2) +
    labs(title = paste(i),
         x = "Time",
         y = "Motivation not to smoke") +
    theme_minimal() +
    ylim(-1, 10)
})

sens_subset_3_combined_plot <- plot_grid(plotlist = sens_subset_3_plot_list, nrow = 2)
cowplot::save_plot(plot = sens_subset_3_combined_plot, filename = here("scripts", "sens_permissibility_combined_plot.png"), base_height = 8, base_width = 24)

# cost of smoking to the most salient self-concept ------------------------

sens_subset_4 <- sens_model_output[16:20]

sens_subset_4_plot_list <- lapply(seq_along(sens_subset_4), function(i) {
  ggplot(sens_subset_4[[i]], aes(x = nTime)) +
    geom_point(aes(y = smok), colour = "black", shape = 1, stroke = 1.2) +
    labs(title = paste(i),
         x = "Time",
         y = "Lapses") +
    theme_minimal() +
    ylim(0, 1)
})

sens_subset_4_combined_plot <- plot_grid(plotlist = sens_subset_4_plot_list, nrow = 2)
cowplot::save_plot(plot = sens_subset_4_combined_plot, filename = here("scripts", "sens_identity_cost_combined_plot.png"), base_height = 8, base_width = 24)
