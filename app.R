library(here)
library(tidyverse)
library(rcartocolor)
library(plotly)
library(DT)
library(shiny)
library(shinydashboard)

source(here("www", "2_model_functions.R"))
scenarios <- read_csv(here("www", "scenarios.csv"))
#rmarkdown::render(input = here("www", "welcome.Rmd"), output_format = "html_document", output_file = here("www", "welcome.html"))

# Colour palette for plots
colours_1 <- carto_pal(n = 6, name = "DarkMint")
colours_2 <- carto_pal(n = 3, name = "Magenta")
colours_3 <- carto_pal(n = 3, name = "Peach")

# define UI

ui <- fluidPage(
  navbarPage("COMPLAPSE Simulation Tool", fluid = TRUE,
             tabPanel("Welcome",
                      fluidPage(
                        tags$h2("Welcome to Project COMPLAPSE"),
                        tags$p("This R Shiny application was built to enable interested readers to explore the formal and computational model developed as part of Project COMPLAPSE."),
                        tags$p("This R Shiny application includes the following tabs:"),
                        tags$ul(
                          tags$li(tags$strong("Pre-Specified Simulation Scenarios"), " (contains a few pre-specified simulations, using the parameter sets from the estimation procedure)"),
                          tags$li(tags$strong("Global Simulation Environment"), " (environment in which all model parameters can be tuned via sliders)")
                        )
                      )),
             tabPanel("Pre-Specified Simulation Scenarios",
                      tags$style(HTML("
           .scenario-box {
             background-color: #f9f9f9;
             border: 3px solid #ddd;
             border-radius: 3px;
             padding: 10px;
             margin-bottom: 10px;
           }
           .scenario-box.warning {
             border-color: #f0ad4e;
           }
           .scenario-box.success {
             border-color: #5cb85c;
           }
         ")),
         fluidRow(
           column(width = 6,
                  class = "scenario-box warning",
                  h4("Scenario A:"),
                  selectInput("scenario_select_A", "Select scenario:", choices = unique(scenarios$name)),
                  h5("Description of Scenario A"),
                  uiOutput("desc_scenA"),
                  actionButton("runScenarioA", "Run Scenario A"),
                  plotlyOutput("scenA", height = "120%"),
                  tabsetPanel(
                    tabPanel("Strategy", 
                             plotOutput("scenA_summary_strategy")),
                    tabPanel("Stressors", 
                             plotOutput("scenA_summary_stress")),
                    tabPanel("Cues", 
                             plotOutput("scenA_summary_cues"))
                  )),
           column(width = 6,
                  class = "scenario-box success",
                  h4("Scenario B:"),
                  selectInput("scenario_select_B", "Select scenario:", choices = unique(scenarios$name)),
                  h5("Description of Scenario B"),
                  uiOutput("desc_scenB"),
                  actionButton("runScenarioB", "Run Scenario B"),
                  plotlyOutput("scenB", height = "120%"),
                  tabsetPanel(
                    tabPanel("Strategy", 
                             plotOutput("scenB_summary_strategy")),
                    tabPanel("Stressors", 
                             plotOutput("scenB_summary_stress")),
                    tabPanel("Cues", 
                             plotOutput("scenB_summary_cues"))
                  )))),
         tabPanel(title = "Global Simulation Environment",
                  sidebarLayout(
                    sidebarPanel(
                      tabsetPanel(
                        tabPanel("Simulation time",
                                 sliderInput("nTime", "Time (in 5-minute ticks):", min = 2880, max = 8064, value = 8064)), # first 28 days of the quit attempt
                        tabPanel("Starting conditions",
                                 sliderInput("NW_init", "Nicotine withdrawal:", min = 0, max = 10, value = 8),
                                 sliderInput("ES_init", "Experienced stress:", min = 0, max = 10, value = 3),
                                 sliderInput("CR_init", "Cue reactivity:", min = 0, max = 10, value = 3),
                                 sliderInput("SE_init", "Self-efficacy:", min = 0, max = 10, value = 6)),
                        tabPanel("Short-acting pharmacotherapy",
                                 sliderInput("str_scenario", "Whether the person has access to short-acting NRT:", min = 0, max = 1, step = 1, value = 0)),
                        tabPanel("Long-acting pharmacotherapy",
                                 sliderInput("alpha", "The impact of using long-acting pharmacotherapy (alpha):", min = 0, max = 3, value = 0)),
                        tabPanel("Nicotine withdrawal",
                                 sliderInput("beta1", "The decay of the background craving (beta1):", min = 0, max = 1, value = 0.95),
                                 sliderInput("beta2", "The short-term impact of a smoking episode on the background craving (beta2):", min = 0, max = 1, value = 0.1),
                                 sliderInput("beta3", "The long-term impact of a smoking episode on the background craving (beta3):", min = 0, max = 1, value = 0.3)),
                        tabPanel("Stressors",
                                 sliderInput("gamma1", "The probability of encountering a stressor (gamma1):", min = 0, max = 0.3, value = 0.1)),
                        tabPanel("Experienced stress",
                                 sliderInput("delta1", "Decay of previous stress (gamma1):", min = 0, max = 1, value = 0.95),
                                 sliderInput("delta2", "The impact of a recent stressor (gamma2:", min = 0, max = 5, value = 2)),
                        tabPanel("Cigarette cues",
                                 sliderInput("epsilon1", "The probability of encountering a cigarette cue (epsilon1):", min = 0, max = 0.3, value = 0.1)),
                        tabPanel("Cue reactivity",
                                 sliderInput("zeta1", "Decay of previous cigarette cue (zeta1):", min = 0, max = 1, value = 0.45),
                                 sliderInput("zeta2", "The impact of a recent cigarette cue (zeta2:", min = 0, max = 5, value = 2)),
                        tabPanel("Craving to smoke",
                                 sliderInput("eta1", "The influence of the nicotine withdrawal on the craving (eta1):", min = 0, max = 1, value = 0.5),
                                 sliderInput("eta2", "The influence of the experieneced stress on the craving (eta2):", min = 0, max = 1, value = 0.45),
                                 sliderInput("eta3", "The influence of the cue reactivity on the craving (eta3):", min = 0, max = 1, value = 0.35),
                                 sliderInput("eta4", "The impact of having recently used a regulatory strategy on the craving (eta4):", min = 0, max = 1, value = 0.35)),
                        tabPanel("Perceived permissibility of smoking",
                                 sliderInput("theta1", "The probability of being in a context where smoking is perceived as permissible (theta1):", min = 0, max = 0.5, value = 0.25)),
                        tabPanel("Self-efficacy",
                                 sliderInput("iota1", "The growth of self-efficacy (iota1):", min = 1, max = 1.1, value = 1.0015),
                                 sliderInput("iota2", "The impact of a recent lapse on the self-efficacy (iota2):", min = 0, max = 1, value = 0.35)),
                        tabPanel("Motivation not to smoke",
                                 sliderInput("kappa1", "The impact of self-efficacy on motivation (kappa1):", min = 0, max = 1, value = 0.95),
                                 sliderInput("kappa2", "The impact of the perceived permissibility of smoking on motivation (kappa2):", min = 0, max = 5, value = 1)),
                        tabPanel("Strategy preference",
                                 sliderInput("lambda1", "The contribution of the craving and motivation to the strategy preference estimation (lambda1):", min = 0, max = 1, value = 0.3),
                                 sliderInput("lambda2", "The contribution of the cost of smoking to the most salient self-concept to the strategy preference estimation (lambda2):", min = 0, max = 1, value = 0.3),
                                 sliderInput("lambda3", "The contribution of the possibility of regulating to the strategy preference estimation (lambda3):", min = 0, max = 1, value = 0.2)),
                        footer = div(
                          actionButton("runSimulationBtn", "Run simulation"),
                          actionButton("defaultParamsBtn", "Set to default parameters"),
                          style = "display: flex; justify-content: space-between;"
                        )
                      )),
                    mainPanel(
                      tabsetPanel(
                        tabPanel("Simulation Plot", plotlyOutput("simulationPlot",
                                                                 height = "800px")),
                        tabPanel("Simulation Output", DT::dataTableOutput("simulationData")),
                        tabPanel("Selected Parameters", DT::dataTableOutput("params_output"))
                      )
                    )
                  ))
  ))

# define server logic

server <- function(input, output, session) {
  
  default_params <- list(
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
  
  observeEvent(input$defaultParamsBtn, {
    # reset each slider input to its default value
    updateSliderInput(session, "nTime", value = default_params$nTime)
    updateSliderInput(session, "NW_init", value = default_params$NW_init)
    updateSliderInput(session, "ES_init", value = default_params$ES_init)
    updateSliderInput(session, "CR_init", value = default_params$CR_init)
    updateSliderInput(session, "SE_init", value = default_params$SE_init)
    updateSliderInput(session, "St_init", value = default_params$St_init)
    updateSliderInput(session, "alpha", value = default_params$alpha)
    updateSliderInput(session, "beta1", value = default_params$beta1)
    updateSliderInput(session, "beta2", value = default_params$beta2)
    updateSliderInput(session, "beta3", value = default_params$beta3)
    updateSliderInput(session, "gamma1", value = default_params$gamma1)
    updateSliderInput(session, "delta1", value = default_params$delta1)
    updateSliderInput(session, "delta2", value = default_params$delta2)
    updateSliderInput(session, "epsilon1", value = default_params$epsilon1)
    updateSliderInput(session, "zeta1", value = default_params$zeta1)
    updateSliderInput(session, "zeta2", value = default_params$zeta2)
    updateSliderInput(session, "eta1", value = default_params$eta1)
    updateSliderInput(session, "eta2", value = default_params$eta2)
    updateSliderInput(session, "eta3", value = default_params$eta3)
    updateSliderInput(session, "eta4", value = default_params$eta4)
    updateSliderInput(session, "eta5", value = default_params$eta5)
    updateSliderInput(session, "theta1", value = default_params$theta1)
    updateSliderInput(session, "iota1", value = default_params$iota1)
    updateSliderInput(session, "iota2", value = default_params$iota2)
    updateSliderInput(session, "kappa1", value = default_params$kappa1)
    updateSliderInput(session, "kappa2", value = default_params$kappa2)
    updateSliderInput(session, "lambda1", value = default_params$lambda1)
    updateSliderInput(session, "lambda2", value = default_params$lambda2)
    updateSliderInput(session, "lambda3", value = default_params$lambda3)
    updateSliderInput(session, "str_scenario", value = default_params$str_scenario)
    
  })
  
  input_params <- reactive({
    list(nTime = input$nTime,
         alpha = input$alpha,
         beta1 = input$beta1,
         beta2 = input$beta2,
         beta3 = input$beta3,
         gamma1 = input$gamma1,
         delta1 = input$delta1,
         delta2 = input$delta2,
         epsilon1 = input$epsilon1,
         zeta1 = input$zeta1,
         zeta2 = input$zeta2,
         eta1 = input$eta1,
         eta2 = input$eta2,
         eta3 = input$eta3,
         eta4 = input$eta4,
         eta5 = input$eta5,
         theta1 = input$theta1,
         iota1 = input$iota1,
         iota2 = input$iota2,
         kappa1 = input$kappa1,
         kappa2 = input$kappa2,
         lambda1 = input$lambda1,
         lambda2 = input$lambda2,
         lambda3 = input$lambda3,
         str_scenario = input$str_scenario,
         NW_init = input$NW_init,
         ES_init = input$ES_init,
         CR_init = input$CR_init,
         St_init = matrix(0, nrow = 1, ncol = 3),
         SE_init = input$SE_init)
    
  })
  
  # initialize reactiveValues to store simulation results
  sim_results <- reactiveValues(data = NULL,
                                params = NULL)
  
  observeEvent(input$runSimulationBtn, {
    
    if (identical(unlist(input_params()), unlist(default_params))) {
      # use default parameters if input_params are identical to default_params
      sim_data <- simulate_data(params = default_params)
      params <- default_params
      
    } else {
      
      sim_data <- simulate_data(params = input_params())
      params <- input_params()
      
    }
    
    sim_results$params <- params
    sim_results$data <- sim_data
    
  })
  
  output$simulationPlot <- renderPlotly({
    
    req(sim_results$data)
    
    # create an empty plot
    p <- plot_ly()
    
    # add traces for all numeric variables
    num_vars <- colnames(sim_results$data)[sapply(sim_results$data, is.numeric)]
    
    p <- plot_ly(data = sim_results$data,
                 x = ~nTime) %>%
      add_lines(y = ~NW, type = 'scatter', mode = 'lines', name = "Nicotine withdrawal", visible = "legendonly", line = list(color = colours_1[6])) %>%
      add_lines(y = ~S, type = 'scatter', mode = 'lines', name = "Stressors", visible = "legendonly", line = list(color = colours_1[5])) %>%
      add_lines(y = ~ES, type = 'scatter', mode = 'lines', name = "Experienced stress", visible = "legendonly", line = list(color = colours_1[4])) %>%
      add_lines(y = ~CC, type = 'scatter', mode = 'lines', name = "Cigarette cues", visible = "legendonly", line = list(color = colours_1[1])) %>%
      add_lines(y = ~CR, type = 'scatter', mode = 'lines', name = "Cue reactivity", visible = "legendonly", line = list(color = colours_1[2])) %>%
      add_lines(y = ~C, type = 'scatter', mode = 'lines', name = "Craving to smoke", visible = TRUE, line = list(color = colours_1[3])) %>%
      add_lines(y = ~transformed_PE, type = 'scatter', mode = 'lines', name = "Perceived permissibility of smoking", visible = "legendonly", line = list(color = colours_2[3])) %>%
      add_lines(y = ~SE, type = 'scatter', mode = 'lines', name = "Self-efficacy", visible = "legendonly", line = list(color = colours_2[1])) %>%
      add_lines(y = ~M, type = 'scatter', mode = 'lines', name = "Motivation not to smoke", visible = TRUE, line = list(color = colours_2[2])) %>%
      add_lines(y = ~smok, type = 'scatter', mode = 'lines', name = "Smoke", visible = TRUE, line = list(color = colours_3[3])) %>%
      add_lines(y = ~no_smok, type = 'scatter', mode = 'lines', name = "Do nothing", visible = "legendonly", line = list(color = colours_3[2])) %>%
      add_lines(y = ~reg, type = 'scatter', mode = 'lines', name = "Use regulatory strategy", visible = "legendonly", line = list(color = colours_3[1]))
    
    # add layout options
    p <- p %>% layout(title = "Simulation Data Plot",
                      xaxis = list(title = "Time (in 5-minute ticks)"),
                      yaxis = list(title = "Values"))
    
    # print the plot
    p
    
  })
  
  output$simulationData <- renderDT({
    req(sim_results$data)
    
    datatable(
      sim_results$data %>%
        mutate(across(.cols = where(is.numeric), \(x) round(x, 2)))
    )
    
  })
  
  scenario_A_results <- reactiveValues(data = NULL)
  scenario_B_results <- reactiveValues(data = NULL)
  
  # define a reactive expression for Scenario A
  sim_data_A <- reactive({
    
    scenario_1 <- scenarios %>%
      filter(name == input$scenario_select_A)
    
    scenario_1_params <- list()
    
    for(i in 1:nrow(scenario_1)) {
      
      parameter <- scenario_1[i, "parameter"] %>%
        pull(parameter)
      value <- scenario_1[i, "value"] %>%
        pull(value)
      scenario_1_params[[parameter]] <- value
      
    }
    
    scenario_1_params$St_init = matrix(0, nrow = 1, ncol = 3)
    
    scenario_A_results$data <- simulate_data(params = scenario_1_params)
    
  })
  
  # define a reactive expression for Scenario B
  sim_data_B <- reactive({
    
    scenario_2 <- scenarios %>%
      filter(name == input$scenario_select_B)
    
    scenario_2_params <- list()
    
    for(i in 1:nrow(scenario_2)) {
      
      parameter <- scenario_2[i, "parameter"] %>%
        pull(parameter)
      value <- scenario_2[i, "value"] %>%
        pull(value)
      scenario_2_params[[parameter]] <- value
      
    }
    
    scenario_2_params$St_init = matrix(0, nrow = 1, ncol = 3)
    
    scenario_B_results$data <- simulate_data(params = scenario_2_params)
    
  })
  
  # observe button press for Scenario A
  observeEvent(input$runScenarioA, {
    
    # call the run_simulation function for Scenario A
    sim_data_A()
    
  })
  
  output$desc_scenA <- renderUI({
    req(input$scenario_select_A)  # ensure input$scenario_select_A is available
    
    selected_scenario <- scenarios %>%
      filter(name == input$scenario_select_A) %>%
      filter(!str_detect(description, "default"))
    
    # format the scenario name
    formatted_scenario_name <- paste0("<strong>", unique(selected_scenario$name), "</strong>:<br>")
    
    # format each parameter description
    formatted_descriptions <- character(nrow(selected_scenario))  # Initialize a character vector
    
    for(i in 1:nrow(selected_scenario)) {
      parameter <- selected_scenario[i, "parameter"]
      value <- selected_scenario[i, "value"]
      description <- selected_scenario[i, "description"]
      
      # format parameter and value
      formatted_parameter <- paste("<strong>", parameter, "</strong>", sep = "")
      formatted_value <- sprintf("<em>%.2f</em>", value)  # Assuming value is numeric, round to 2 decimal places
      
      # combine parameter, value, and description
      bullet_point <- paste("- ", formatted_parameter, " ", formatted_value, ". ", description, sep = "")
      
      # assign the bullet point to the corresponding index in the formatted_descriptions vector
      formatted_descriptions[i] <- bullet_point
      
    }
    
    # combine all formatted descriptions into a single string
    formatted_text <- paste0(formatted_scenario_name, 
                             paste(formatted_descriptions, collapse = "<br>"))
    
    HTML(formatted_text)
    
  })
  
  output$scenA <- renderPlotly({
    
    req(scenario_A_results$data)
    
    # create an empty plot
    p <- plot_ly()
    
    # add traces for all numeric variables
    num_vars <- colnames(scenario_A_results$data)[sapply(scenario_A_results$data, is.numeric)]
    
    p <- plot_ly(data = scenario_A_results$data,
                 x = ~nTime) %>%
      add_lines(y = ~NW, type = 'scatter', mode = 'lines', name = "Nicotine withdrawal", visible = "legendonly", line = list(color = colours_1[6])) %>%
      add_lines(y = ~S, type = 'scatter', mode = 'lines', name = "Stressors", visible = "legendonly", line = list(color = colours_1[5])) %>%
      add_lines(y = ~ES, type = 'scatter', mode = 'lines', name = "Experienced stress", visible = "legendonly", line = list(color = colours_1[4])) %>%
      add_lines(y = ~CC, type = 'scatter', mode = 'lines', name = "Cigarette cues", visible = "legendonly", line = list(color = colours_1[1])) %>%
      add_lines(y = ~CR, type = 'scatter', mode = 'lines', name = "Cue reactivity", visible = "legendonly", line = list(color = colours_1[2])) %>%
      add_lines(y = ~C, type = 'scatter', mode = 'lines', name = "Craving to smoke", visible = TRUE, line = list(color = colours_1[3])) %>%
      add_lines(y = ~transformed_PE, type = 'scatter', mode = 'lines', name = "Perceived permissibility of smoking", visible = "legendonly", line = list(color = colours_2[3])) %>%
      add_lines(y = ~SE, type = 'scatter', mode = 'lines', name = "Self-efficacy", visible = "legendonly", line = list(color = colours_2[1])) %>%
      add_lines(y = ~M, type = 'scatter', mode = 'lines', name = "Motivation not to smoke", visible = TRUE, line = list(color = colours_2[2])) %>%
      add_lines(y = ~smok, type = 'scatter', mode = 'lines', name = "Smoke", visible = TRUE, line = list(color = colours_3[3])) %>%
      add_lines(y = ~no_smok, type = 'scatter', mode = 'lines', name = "Do nothing", visible = "legendonly", line = list(color = colours_3[2])) %>%
      add_lines(y = ~reg, type = 'scatter', mode = 'lines', name = "Use regulatory strategy", visible = "legendonly", line = list(color = colours_3[1]))
    
    # add layout options
    p <- p %>% layout(title = "",
                      xaxis = list(title = "Time (in 5-minute ticks)"),
                      yaxis = list(title = "Values"))
    
    # print the plot
    p
    
  })
  
  output$scenA_summary_strategy <- renderPlot({
    
    req(scenario_A_results$data)
    
    summary_scenA <- scenario_A_results$data %>%
      mutate(day = nTime %/% 288,
             strategy = factor(strategy,
                               levels = c("smok", "no_smok", "reg"),
                               labels = c("Smoke", "Do nothing", "Use regulatory strategy"))) %>%
      group_by(day, strategy) %>%
      summarise(n = n())
    
    # need to sort out the fill scale so it is consistent across plots and matches to all possible levels (not just those in the data)
    ggplot(data = summary_scenA) +
      geom_col(aes(x = day, y = n, fill = strategy)) +
      scale_fill_viridis_d(labels = c("Smoke", "Do nothing", "Use regulatory strategy"),
                           option = "mako") +
      theme_bw() +
      xlab("Day in the study") +
      ylab("") +
      labs(fill = "Strategy use")
    
  })
  
  output$scenA_summary_stress <- renderPlot({
    
    req(scenario_A_results$data)
    
    summary_scenA <- scenario_A_results$data %>%
      mutate(day = nTime %/% 288) %>%
      group_by(day) %>%
      summarise(sum_S = sum(S, na.rm = TRUE))
    
    ggplot(data = summary_scenA) +
      geom_col(aes(x = day, y = sum_S), fill = "darkred") +
      theme_bw() + 
      xlab("Day in the study") +
      ylab("Number of stressors")
    
  })
  
  output$scenA_summary_cues <- renderPlot({
    
    req(scenario_A_results$data)
    
    summary_scenA <- scenario_A_results$data %>%
      mutate(day = nTime %/% 288) %>%
      group_by(day) %>%
      summarise(sum_CC = sum(CC, na.rm = TRUE))
    
    ggplot(data = summary_scenA) +
      geom_col(aes(x = day, y = sum_CC), fill = "darkblue") +
      theme_bw() + 
      xlab("Day in the study") +
      ylab("Number of cigarette cues")
    
  })
  
  # observe button press for Scenario B
  observeEvent(input$runScenarioB, {
    
    # call the run_simulation function for Scenario B
    sim_data_B()
    
  })
  
  output$desc_scenB <- renderUI({
    req(input$scenario_select_B)  # ensure input$scenario_select_A is available
    
    selected_scenario <- scenarios %>%
      filter(name == input$scenario_select_B) %>%
      filter(!str_detect(description, "default"))
    
    # format the scenario name
    formatted_scenario_name <- paste0("<strong>", unique(selected_scenario$name), "</strong>:<br>")
    
    # format each parameter description
    formatted_descriptions <- character(nrow(selected_scenario))  # initialize a character vector
    
    for(i in 1:nrow(selected_scenario)) {
      parameter <- selected_scenario[i, "parameter"]
      value <- selected_scenario[i, "value"]
      description <- selected_scenario[i, "description"]
      
      # format parameter and value
      formatted_parameter <- paste("<strong>", parameter, "</strong>", sep = "")
      formatted_value <- sprintf("<em>%.2f</em>", value)  # Assuming value is numeric, round to 2 decimal places
      
      # combine parameter, value, and description
      bullet_point <- paste("- ", formatted_parameter, " ", formatted_value, ". ", description, sep = "")
      
      # assign the bullet point to the corresponding index in the formatted_descriptions vector
      formatted_descriptions[i] <- bullet_point
    }
    
    # combine all formatted descriptions into a single string
    formatted_text <- paste0(formatted_scenario_name, 
                             paste(formatted_descriptions, collapse = "<br>"))
    
    HTML(formatted_text)
    
  })
  
  output$scenB <- renderPlotly({
    
    req(scenario_B_results$data)
    
    # create an empty plot
    p <- plot_ly()
    
    # add traces for all numeric variables
    num_vars <- colnames(scenario_B_results$data)[sapply(scenario_B_results$data, is.numeric)]
    
    p <- plot_ly(data = scenario_B_results$data,
                 x = ~nTime) %>%
      add_lines(y = ~NW, type = 'scatter', mode = 'lines', name = "Nicotine withdrawal", visible = "legendonly", line = list(color = colours_1[6])) %>%
      add_lines(y = ~S, type = 'scatter', mode = 'lines', name = "Stressors", visible = "legendonly", line = list(color = colours_1[5])) %>%
      add_lines(y = ~ES, type = 'scatter', mode = 'lines', name = "Experienced stress", visible = "legendonly", line = list(color = colours_1[4])) %>%
      add_lines(y = ~CC, type = 'scatter', mode = 'lines', name = "Cigarette cues", visible = "legendonly", line = list(color = colours_1[1])) %>%
      add_lines(y = ~CR, type = 'scatter', mode = 'lines', name = "Cue reactivity", visible = "legendonly", line = list(color = colours_1[2])) %>%
      add_lines(y = ~C, type = 'scatter', mode = 'lines', name = "Craving to smoke", visible = TRUE, line = list(color = colours_1[3])) %>%
      add_lines(y = ~transformed_PE, type = 'scatter', mode = 'lines', name = "Perceived permissibility of smoking", visible = "legendonly", line = list(color = colours_2[3])) %>%
      add_lines(y = ~SE, type = 'scatter', mode = 'lines', name = "Self-efficacy", visible = "legendonly", line = list(color = colours_2[1])) %>%
      add_lines(y = ~M, type = 'scatter', mode = 'lines', name = "Motivation not to smoke", visible = TRUE, line = list(color = colours_2[2])) %>%
      add_lines(y = ~smok, type = 'scatter', mode = 'lines', name = "Smoke", visible = TRUE, line = list(color = colours_3[3])) %>%
      add_lines(y = ~no_smok, type = 'scatter', mode = 'lines', name = "Do nothing", visible = "legendonly", line = list(color = colours_3[2])) %>%
      add_lines(y = ~reg, type = 'scatter', mode = 'lines', name = "Use regulatory strategy", visible = "legendonly", line = list(color = colours_3[1]))
    
    # add layout options
    p <- p %>% layout(title = "",
                      xaxis = list(title = "Time (in 5-minute ticks)"),
                      yaxis = list(title = "Values"))
    
    # print the plot
    p
    
  })
  
  output$scenB_summary_strategy <- renderPlot({
    
    req(scenario_B_results$data)
    
    summary_scenB <- scenario_B_results$data %>%
      mutate(day = nTime %/% 288) %>%
      group_by(day, strategy) %>%
      summarise(n = n())
    
    # need to sort out the fill scale so it is consistent across plots and matches to all possible levels (not just those in the data)
    ggplot(data = summary_scenB) +
      geom_col(aes(x = day, y = n, fill = strategy)) +
      scale_fill_viridis_d(labels = c("Smoke", "Do nothing", "Use regulatory strategy"),
                           option = "mako") +
      theme_bw() +
      xlab("Day in the study") +
      ylab("") +
      labs(fill = "Strategy use")
    
  })
  
  output$scenB_summary_stress <- renderPlot({
    
    req(scenario_B_results$data)
    
    summary_scenB <- scenario_B_results$data %>%
      mutate(day = nTime %/% 288) %>%
      group_by(day) %>%
      summarise(sum_S = sum(S, na.rm = TRUE)) 
    
    ggplot(data = summary_scenB) +
      geom_col(aes(x = day, y = sum_S), fill = "darkred") +
      theme_bw() + 
      xlab("Day in the study") +
      ylab("Number of stressors")
    
  })
  
  output$scenB_summary_cues <- renderPlot({
    
    req(scenario_B_results$data)
    
    summary_scenB <- scenario_B_results$data %>%
      mutate(day = nTime %/% 288) %>%
      group_by(day) %>%
      summarise(sum_CC = sum(CC, na.rm = TRUE))
    
    ggplot(data = summary_scenB) +
      geom_col(aes(x = day, y = sum_CC), fill = "darkblue") +
      theme_bw() +
      xlab("Day in the study") +
      ylab("Number of cigarette cues")
    
  })
  
  output$params_output <- renderDT({
    
    req(sim_results$params)
    
    cbind("Values" = sim_results$params)
    
  })
  
  output$welcome <- renderUI({
    includeHTML("./welcome.html")
    
  })
  
}

# run the application

shinyApp(ui = ui, server = server)
