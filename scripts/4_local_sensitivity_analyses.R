library(here)
library(tidyverse)
library(ggplot2)
library(cowplot)

# read in csv
params_df <- read_csv(here("scripts", "sensitivity_params.csv"))

# convert the matrix to a data frame
params_df <- as.data.frame(params_df)

rownames <- c("zeta2", "iota1")

# set the column names
colnames(params_df) <- rownames

#load in the functions
source(here("scripts", "2_model_functions.R"))

a <- params_df %>%
  pivot_longer(cols = everything(), names_to = "parameter", values_to = "values") %>%
  mutate(sample = rep(1:10, each = 2))

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
  iota2 = 0.5, # set a little higher to influence the self-efficacy
  kappa1 = 0.95,
  kappa2 = 1,
  lambda1 = 0.55, # set a little higher than default to influence the strategy preference
  lambda2 = 0.3,
  lambda3 = 0.2,
  str_scenario = 0,
  NW_init = 8,
  ES_init = 3,
  CR_init = 3,
  SE_init = 6,
  St_init = matrix(0, nrow = 1, ncol = 3))

sens_model_output <- list()

set.seed(123)

for(z in 1:10) {
  
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

# cue reactivity ---------------------------------------

sens_subset_1 <- sens_model_output[1:5]

sens_subset_1_plot_list <- lapply(seq_along(sens_subset_1), function(i) {
  ggplot(sens_subset_1[[i]], aes(x = nTime)) +
    geom_line(aes(y = CR), linewidth = 0.75, colour = "black") +
    geom_point(data = subset(sens_subset_1[[i]], smok == 1), aes(y = smok), 
               colour = "darkred", shape = 1, stroke = 1.2) +
    labs(title = paste(i)) +
    theme_minimal() +
    theme(axis.title.x = element_blank(), 
          axis.title.y = element_blank(),
          plot.margin = unit(c(1, 1, 1, 1), "cm")) +
    ylim(0, 10) +
    xlim(0, 2880)
})

sens_subset_1_combined_plot <- plot_grid(plotlist = sens_subset_1_plot_list, ncol = 2)

final_plot_subset_1 <- ggdraw(sens_subset_1_combined_plot) +
  draw_label("Time (in 5-minute ticks)", x = 0.5, y = 0.03, hjust = 0.5, vjust = 1, size = 12) +
  draw_label("Cue reactivity", x = 0.001, y = 0.5, angle = 90, vjust = 1, hjust = 0.5, size = 12) 

cowplot::save_plot(plot = final_plot_subset_1, filename = here("scripts", "sensitivity_1_CR.png"), base_height = 8, base_width = 10)

# self-efficacy -----------------------------------------------------------

sens_subset_2 <- sens_model_output[6:10]

sens_subset_2_plot_list <- lapply(seq_along(sens_subset_2), function(i) {
  ggplot(sens_subset_2[[i]], aes(x = nTime)) +
    geom_line(aes(y = SE), linewidth = 0.75, colour = "black") +
    geom_point(data = subset(sens_subset_2[[i]], smok == 1), aes(y = smok), 
               colour = "darkred", shape = 1, stroke = 1.2) +
    labs(title = paste(i)) +
    theme_minimal() +
    theme(axis.title.x = element_blank(), 
          axis.title.y = element_blank(),
          plot.margin = unit(c(1, 1, 1, 1), "cm")) +
    ylim(0, 10) +
    xlim(0, 2880)
})

sens_subset_2_combined_plot <- plot_grid(plotlist = sens_subset_2_plot_list, nrow = 2)

final_plot_subset_2 <- ggdraw(sens_subset_2_combined_plot) +
  draw_label("Time (in 5-minute ticks)", x = 0.5, y = 0.03, hjust = 0.5, vjust = 1, size = 12) +
  draw_label("Self-efficacy", x = 0.001, y = 0.5, angle = 90, vjust = 1, hjust = 0.5, size = 12) 

cowplot::save_plot(plot = final_plot_subset_2, filename = here("scripts", "sensitivity_2_SE.png"), base_height = 8, base_width = 10)
