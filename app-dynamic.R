library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(dplyr)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Soil Oxygen Dynamics Model"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Model Parameters", tabName = "parameters", icon = icon("sliders-h")),
      menuItem("Results", tabName = "results", icon = icon("chart-line")),
      menuItem("Layer Comparison", tabName = "comparison", icon = icon("layer-group")),
      menuItem("Time Series", tabName = "timeseries", icon = icon("clock"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
      "))
    ),
    
    tabItems(
      # Parameters tab
      tabItem(tabName = "parameters",
        fluidRow(
          box(title = "Environmental Conditions", status = "primary", solidHeader = TRUE, width = 6,
            sliderInput("temperature", "Temperature (°C)", 
                       min = 5, max = 35, value = 20, step = 1),
            sliderInput("moisture", "Volumetric Water Content (m³/m³)", 
                       min = 0.1, max = 0.6, value = 0.3, step = 0.01),
            selectInput("soil_type", "Soil Type",
                       choices = list("Sandy" = "sandy", "Loamy" = "loamy", "Clay" = "clay"),
                       selected = "loamy")
          ),
          
          box(title = "Model Settings", status = "info", solidHeader = TRUE, width = 6,
            numericInput("depth_max", "Maximum Depth (cm)", value = 120, min = 60, max = 200),
            numericInput("grid_size", "Grid Size (cm)", value = 5, min = 2, max = 10),
            checkboxInput("steady_state", "Steady State Solution", value = TRUE),
            conditionalPanel(
              condition = "input.steady_state == false",
              numericInput("time_hours", "Simulation Time (hours)", value = 24, min = 1, max = 168)
            )
          )
        ),
        
        fluidRow(
          box(title = "Layer 1: Surface (0-30cm)", status = "success", solidHeader = TRUE, width = 6,
            sliderInput("l1_porosity", "Total Porosity", 
                       min = 0.45, max = 0.65, value = 0.55, step = 0.01),
            sliderInput("l1_organic", "Organic Matter (%)", 
                       min = 1, max = 10, value = 4, step = 0.1),
            sliderInput("l1_root_resp", "Root Respiration (kg O₂/m³/day)", 
                       min = 0.1, max = 3.0, value = 1.0, step = 0.1)
          ),
          
          box(title = "Layer 2: Transition (30-60cm)", status = "warning", solidHeader = TRUE, width = 6,
            sliderInput("l2_porosity", "Total Porosity", 
                       min = 0.40, max = 0.60, value = 0.50, step = 0.01),
            sliderInput("l2_organic", "Organic Matter (%)", 
                       min = 0.5, max = 4, value = 2, step = 0.1),
            sliderInput("l2_root_resp", "Root Respiration (kg O₂/m³/day)", 
                       min = 0.05, max = 1.0, value = 0.3, step = 0.05)
          )
        ),
        
        fluidRow(
          box(title = "Layer 3: Subsoil (60-90cm)", status = "warning", solidHeader = TRUE, width = 6,
            sliderInput("l3_porosity", "Total Porosity", 
                       min = 0.30, max = 0.55, value = 0.42, step = 0.01),
            sliderInput("l3_organic", "Organic Matter (%)", 
                       min = 0.2, max = 2, value = 0.8, step = 0.1),
            sliderInput("l3_root_resp", "Root Respiration (kg O₂/m³/day)", 
                       min = 0.01, max = 0.5, value = 0.1, step = 0.01)
          ),
          
          box(title = "Layer 4: Deep (90-120cm)", status = "danger", solidHeader = TRUE, width = 6,
            sliderInput("l4_porosity", "Total Porosity", 
                       min = 0.25, max = 0.50, value = 0.35, step = 0.01),
            sliderInput("l4_organic", "Organic Matter (%)", 
                       min = 0.1, max = 1, value = 0.3, step = 0.1),
            sliderInput("l4_root_resp", "Root Respiration (kg O₂/m³/day)", 
                       min = 0.005, max = 0.2, value = 0.05, step = 0.005)
          )
        ),
        
        fluidRow(
          box(title = "Calculate", status = "primary", solidHeader = TRUE, width = 12,
            actionButton("calculate", "Run Model", class = "btn-primary btn-lg"),
            br(), br(),
            verbatimTextOutput("model_status")
          )
        )
      ),
      
      # Results tab
      tabItem(tabName = "results",
        fluidRow(
          box(title = "Oxygen Concentration Profile", status = "primary", solidHeader = TRUE, width = 8,
            plotlyOutput("oxygen_profile", height = "500px")
          ),
          box(title = "Key Metrics", status = "info", solidHeader = TRUE, width = 4,
            valueBoxOutput("surface_o2", width = NULL),
            valueBoxOutput("critical_depth", width = NULL),
            valueBoxOutput("anaerobic_zone", width = NULL)
          )
        ),
        
        fluidRow(
          box(title = "Diffusion Coefficients", status = "success", solidHeader = TRUE, width = 6,
            plotlyOutput("diffusion_profile", height = "400px")
          ),
          box(title = "Consumption Rates", status = "warning", solidHeader = TRUE, width = 6,
            plotlyOutput("consumption_profile", height = "400px")
          )
        )
      ),
      
      # Comparison tab
      tabItem(tabName = "comparison",
        fluidRow(
          box(title = "Layer Properties Comparison", status = "primary", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("layer_table")
          )
        ),
        
        fluidRow(
          box(title = "Physical Properties by Layer", status = "info", solidHeader = TRUE, width = 6,
            plotlyOutput("physical_comparison", height = "400px")
          ),
          box(title = "Biological Activity by Layer", status = "success", solidHeader = TRUE, width = 6,
            plotlyOutput("biological_comparison", height = "400px")
          )
        )
      ),
      
      # Time series tab
      tabItem(tabName = "timeseries",
        fluidRow(
          box(title = "Dynamic Environmental Conditions", status = "primary", solidHeader = TRUE, width = 4,
            checkboxInput("enable_dynamic", "Enable Dynamic Conditions", value = FALSE),
            
            conditionalPanel(
              condition = "input.enable_dynamic == true",
              
              h4("Temperature Dynamics"),
              fluidRow(
                column(6, numericInput("temp_mean", "Mean Temp (°C)", value = 20, min = 5, max = 35)),
                column(6, numericInput("temp_amplitude", "Daily Range (°C)", value = 8, min = 1, max = 20))
              ),
              selectInput("temp_pattern", "Temperature Pattern",
                         choices = list("Sinusoidal Daily Cycle" = "daily",
                                      "Step Change" = "step", 
                                      "Gradual Warming" = "warming",
                                      "Weather Event" = "weather"),
                         selected = "daily"),
              
              conditionalPanel(
                condition = "input.temp_pattern == 'step'",
                numericInput("step_time", "Change Time (hours)", value = 12, min = 1, max = 100)
              ),
              
              conditionalPanel(
                condition = "input.temp_pattern == 'weather'",
                numericInput("event_start", "Event Start (hours)", value = 24, min = 1, max = 100),
                numericInput("event_duration", "Event Duration (hours)", value = 6, min = 1, max = 48)
              ),
              
              h4("Moisture Dynamics"),
              fluidRow(
                column(6, numericInput("moisture_mean", "Mean Moisture", value = 0.3, min = 0.1, max = 0.6, step = 0.01)),
                column(6, numericInput("moisture_amplitude", "Variation", value = 0.05, min = 0.01, max = 0.2, step = 0.01))
              ),
              selectInput("moisture_pattern", "Moisture Pattern",
                         choices = list("Steady" = "steady",
                                      "Drying Cycle" = "drying",
                                      "Irrigation Event" = "irrigation",
                                      "Rain Event" = "rain"),
                         selected = "steady"),
              
              conditionalPanel(
                condition = "input.moisture_pattern == 'irrigation' || input.moisture_pattern == 'rain'",
                numericInput("moisture_event_start", "Event Start (hours)", value = 12, min = 1, max = 100),
                numericInput("moisture_event_duration", "Event Duration (hours)", value = 2, min = 0.5, max = 24, step = 0.5)
              )
            ),
            
            br(),
            actionButton("run_dynamic", "Run Dynamic Model", class = "btn-warning btn-block"),
            
            conditionalPanel(
              condition = "input.enable_dynamic == true",
              br(),
              plotOutput("environmental_preview", height = "200px")
            )
          ),
          
          box(title = "Oxygen Dynamics Over Time", status = "info", solidHeader = TRUE, width = 8,
            plotlyOutput("time_series_plot", height = "500px")
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive values to store model results
  model_results <- reactiveValues(
    oxygen = NULL,
    diffusion = NULL,
    consumption = NULL,
    depths = NULL,
    layer_summary = NULL,
    time_series = NULL,
    dt_hours = NULL,
    env_conditions = NULL,
    diffusion_series = NULL,
    consumption_series = NULL
  )
  
  # Function to calculate soil properties based on type
  get_soil_properties <- function(soil_type) {
    properties <- switch(soil_type,
      "sandy" = list(bulk_density = 1.5, b_campbell = 3, tortuosity = 0.45),
      "loamy" = list(bulk_density = 1.4, b_campbell = 6, tortuosity = 0.35),
      "clay" = list(bulk_density = 1.3, b_campbell = 9, tortuosity = 0.25)
    )
    return(properties)
  }
  
  # Function to calculate effective diffusion coefficient
  calc_diffusion <- function(porosity, moisture, soil_props) {
    D0 <- 1.98e-5  # m²/s oxygen diffusion in free air
    air_porosity <- max(0.01, porosity - moisture)
    
    # Buckingham-Burdine-Campbell model
    relative_diffusion <- porosity^2 * (air_porosity / porosity)^(2 + 3/soil_props$b_campbell)
    D_eff <- D0 * relative_diffusion * soil_props$tortuosity
    
    return(D_eff * 1e6)  # Convert to cm²/s for easier handling
  }
  
  # Function to calculate consumption rates
  calc_consumption <- function(depth, root_resp_surface, organic_matter, temperature) {
    # Temperature correction (Q10 = 2.5)
    temp_factor <- 2.5^((temperature - 20) / 10)
    
    # Root respiration with exponential decay
    z_root <- 35  # cm
    root_consumption <- root_resp_surface * exp(-depth / z_root) * temp_factor
    
    # Microbial respiration
    microbial_surface <- organic_matter * 0.1  # kg O₂/m³/day per % OM
    z_microbial <- 25  # cm
    microbial_consumption <- microbial_surface * exp(-depth / z_microbial) * temp_factor
    
    return(root_consumption + microbial_consumption)
  }
  
  # Function to generate environmental time series
  generate_environmental_conditions <- function(time_hours, dt_hours) {
    time_vector <- seq(0, time_hours, by = dt_hours)
    n_steps <- length(time_vector)
    
    # Initialize arrays
    temperature <- numeric(n_steps)
    moisture <- numeric(n_steps)
    
    # Temperature patterns
    if (input$temp_pattern == "daily") {
      # Sinusoidal daily cycle: T = mean + amplitude * sin(2π * t/24)
      temperature <- input$temp_mean + input$temp_amplitude * sin(2 * pi * time_vector / 24)
      
    } else if (input$temp_pattern == "step") {
      # Step change at specified time
      temperature <- ifelse(time_vector < input$step_time, 
                           input$temp_mean - input$temp_amplitude/2,
                           input$temp_mean + input$temp_amplitude/2)
      
    } else if (input$temp_pattern == "warming") {
      # Gradual warming trend
      temperature <- input$temp_mean + (input$temp_amplitude * time_vector / max(time_vector))
      
    } else if (input$temp_pattern == "weather") {
      # Weather event (e.g., heat wave or cold snap)
      temperature <- rep(input$temp_mean, n_steps)
      event_end <- input$event_start + input$event_duration
      event_mask <- time_vector >= input$event_start & time_vector <= event_end
      temperature[event_mask] <- input$temp_mean + input$temp_amplitude
    }
    
    # Moisture patterns
    if (input$moisture_pattern == "steady") {
      moisture <- rep(input$moisture_mean, n_steps)
      
    } else if (input$moisture_pattern == "drying") {
      # Exponential drying
      moisture <- input$moisture_mean * exp(-time_vector / (max(time_vector) / 3))
      moisture <- moisture + input$moisture_amplitude  # Add baseline
      
    } else if (input$moisture_pattern == "irrigation") {
      # Irrigation event
      moisture <- rep(input$moisture_mean, n_steps)
      event_end <- input$moisture_event_start + input$moisture_event_duration
      event_mask <- time_vector >= input$moisture_event_start & time_vector <= event_end
      moisture[event_mask] <- moisture[event_mask] + input$moisture_amplitude
      
    } else if (input$moisture_pattern == "rain") {
      # Rain event with gradual drying
      moisture <- rep(input$moisture_mean, n_steps)
      event_end <- input$moisture_event_start + input$moisture_event_duration
      
      # During rain event
      event_mask <- time_vector >= input$moisture_event_start & time_vector <= event_end
      moisture[event_mask] <- input$moisture_mean + input$moisture_amplitude
      
      # Post-rain drying
      post_rain_mask <- time_vector > event_end
      if (any(post_rain_mask)) {
        post_rain_time <- time_vector[post_rain_mask] - event_end
        moisture[post_rain_mask] <- (input$moisture_mean + input$moisture_amplitude) * 
                                   exp(-post_rain_time / 24) + input$moisture_mean * 
                                   (1 - exp(-post_rain_time / 24))
      }
    }
    
    # Ensure moisture stays within physical bounds
    moisture <- pmax(0.05, pmin(0.8, moisture))
    
    return(list(time = time_vector, temperature = temperature, moisture = moisture))
  }

  # Function to solve time-dependent diffusion equation with dynamic conditions
  solve_dynamic_oxygen <- function(depths, env_conditions, soil_props) {
    n_depths <- length(depths)
    n_steps <- length(env_conditions$time)
    dz <- depths[2] - depths[1]  # Grid spacing in cm
    dt <- (env_conditions$time[2] - env_conditions$time[1]) * 3600  # Convert hours to seconds
    
    # Convert units
    dz_m <- dz / 100  # Convert cm to meters
    
    # Initialize oxygen concentration
    oxygen <- rep(0.21, n_depths)  # Start with atmospheric concentration
    
    # Storage for results
    oxygen_series <- array(0, dim = c(n_steps, n_depths))
    diffusion_series <- array(0, dim = c(n_steps, n_depths))
    consumption_series <- array(0, dim = c(n_steps, n_depths))
    
    oxygen_series[1, ] <- oxygen
    
    # Time stepping loop
    for (step in 2:n_steps) {
      current_temp <- env_conditions$temperature[step]
      current_moisture <- env_conditions$moisture[step]
      
      # Recalculate properties for current conditions
      diffusion <- numeric(n_depths)
      consumption <- numeric(n_depths)
      
      for (i in 1:n_depths) {
        depth <- depths[i]
        
        # Get layer properties
        if (depth <= 30) {
          porosity <- input$l1_porosity
          organic <- input$l1_organic
          root_resp <- input$l1_root_resp
        } else if (depth <= 60) {
          porosity <- input$l2_porosity
          organic <- input$l2_organic
          root_resp <- input$l2_root_resp
        } else if (depth <= 90) {
          porosity <- input$l3_porosity
          organic <- input$l3_organic
          root_resp <- input$l3_root_resp
        } else {
          porosity <- input$l4_porosity
          organic <- input$l4_organic
          root_resp <- input$l4_root_resp
        }
        
        # Calculate dynamic properties
        diffusion[i] <- calc_diffusion(porosity, current_moisture, soil_props)
        consumption[i] <- calc_consumption(depth, root_resp, organic, current_temp)
      }
      
      # Store for plotting
      diffusion_series[step, ] <- diffusion
      consumption_series[step, ] <- consumption
      
      # Convert diffusion to m²/s
      diffusion_m2s <- diffusion * 1e-6
      
      # Update oxygen concentrations
      oxygen_new <- oxygen
      
      # Interior points - explicit finite difference
      for (i in 2:(n_depths-1)) {
        # Calculate diffusion term
        d2c_dz2 <- (diffusion_m2s[i+1] * (oxygen[i+1] - oxygen[i]) - 
                    diffusion_m2s[i-1] * (oxygen[i] - oxygen[i-1])) / dz_m^2
        
        # Update concentration
        oxygen_new[i] <- oxygen[i] + dt * (d2c_dz2 - consumption[i] / (24 * 3600))
        
        # Prevent negative concentrations
        oxygen_new[i] <- max(0, oxygen_new[i])
      }
      
      # Boundary conditions
      oxygen_new[1] <- 0.21  # Surface: atmospheric concentration
      oxygen_new[n_depths] <- oxygen_new[n_depths-1]  # Bottom: zero flux
      
      oxygen <- oxygen_new
      oxygen_series[step, ] <- oxygen
    }
    
    return(list(
      oxygen_series = oxygen_series,
      diffusion_series = diffusion_series,
      consumption_series = consumption_series,
      final_oxygen = oxygen,
      env_conditions = env_conditions
    ))
  }

  # Function to solve time-dependent diffusion equation (static conditions)
  solve_time_dependent <- function(depths, diffusion, consumption, dt_hours, total_hours) {
    n_depths <- length(depths)
    dz <- depths[2] - depths[1]  # Grid spacing in cm
    dt <- dt_hours * 3600  # Convert hours to seconds
    n_steps <- round(total_hours / dt_hours)
    
    # Convert units for calculation
    dz_m <- dz / 100  # Convert cm to meters
    diffusion_m2s <- diffusion * 1e-6  # Convert from cm²/s to m²/s
    
    # Initialize oxygen concentration
    oxygen <- rep(0.21, n_depths)  # Start with atmospheric concentration
    
    # Storage for time series
    time_series <- array(0, dim = c(n_steps + 1, n_depths))
    time_series[1, ] <- oxygen
    
    # Stability check
    max_D <- max(diffusion_m2s)
    max_dt <- 0.25 * dz_m^2 / max_D
    if (dt > max_dt) {
      warning(paste("Time step may be unstable. Recommended max dt:", round(max_dt/3600, 4), "hours"))
    }
    
    # Time stepping loop
    for (step in 1:n_steps) {
      oxygen_new <- oxygen
      
      # Interior points - explicit finite difference
      for (i in 2:(n_depths-1)) {
        # Calculate diffusion term using central differences
        d2c_dz2 <- (diffusion_m2s[i+1] * (oxygen[i+1] - oxygen[i]) - 
                    diffusion_m2s[i-1] * (oxygen[i] - oxygen[i-1])) / dz_m^2
        
        # Update concentration
        oxygen_new[i] <- oxygen[i] + dt * (d2c_dz2 - consumption[i] / (24 * 3600))
        
        # Prevent negative concentrations
        oxygen_new[i] <- max(0, oxygen_new[i])
      }
      
      # Boundary conditions
      oxygen_new[1] <- 0.21  # Surface: atmospheric concentration
      oxygen_new[n_depths] <- oxygen_new[n_depths-1]  # Bottom: zero flux
      
      oxygen <- oxygen_new
      time_series[step + 1, ] <- oxygen
    }
    
    return(list(final_profile = oxygen, time_series = time_series))
  }

  # Main model calculation
  observeEvent(input$calculate, {
    withProgress(message = 'Running soil oxygen model...', value = 0, {
      
      # Setup depth grid
      depths <- seq(0, input$depth_max, by = input$grid_size)
      n_depths <- length(depths)
      
      # Initialize arrays
      oxygen <- numeric(n_depths)
      diffusion <- numeric(n_depths)
      consumption <- numeric(n_depths)
      
      # Soil properties
      soil_props <- get_soil_properties(input$soil_type)
      
      incProgress(0.2, detail = "Calculating layer properties...")
      
      # Calculate properties for each depth
      for (i in 1:n_depths) {
        depth <- depths[i]
        
        # Determine layer and get properties
        if (depth <= 30) {
          porosity <- input$l1_porosity
          organic <- input$l1_organic
          root_resp <- input$l1_root_resp
        } else if (depth <= 60) {
          porosity <- input$l2_porosity
          organic <- input$l2_organic
          root_resp <- input$l2_root_resp
        } else if (depth <= 90) {
          porosity <- input$l3_porosity
          organic <- input$l3_organic
          root_resp <- input$l3_root_resp
        } else {
          porosity <- input$l4_porosity
          organic <- input$l4_organic
          root_resp <- input$l4_root_resp
        }
        
        # Calculate diffusion coefficient
        diffusion[i] <- calc_diffusion(porosity, input$moisture, soil_props)
        
        # Calculate consumption rate
        consumption[i] <- calc_consumption(depth, root_resp, organic, input$temperature)
      }
      
      incProgress(0.5, detail = "Solving oxygen transport equation...")
      
      # Choose solution method
      if (input$steady_state) {
        # Steady-state analytical approximation
        oxygen[1] <- 0.21  # Surface boundary condition
        
        for (i in 2:n_depths) {
          if (diffusion[i] > 0) {
            # Penetration depth calculation
            penetration_depth <- sqrt(diffusion[i] / (consumption[i] + 1e-10))
            oxygen[i] <- oxygen[i-1] * exp(-(depths[i] - depths[i-1]) / penetration_depth)
          } else {
            oxygen[i] <- 0
          }
        }
      } else {
        # Time-dependent solution
        incProgress(0.3, detail = "Running time-dependent simulation...")
        
        # Check if dynamic conditions are enabled
        if (input$enable_dynamic) {
          # Calculate appropriate time step
          dz_m <- (depths[2] - depths[1]) / 100
          # Use a conservative estimate for dynamic conditions
          dt_hours <- min(0.1, input$time_hours / 200)  # Ensure at least 200 time steps
          
          # Generate environmental conditions
          env_conditions <- generate_environmental_conditions(input$time_hours, dt_hours)
          
          # Run dynamic simulation
          dynamic_result <- solve_dynamic_oxygen(depths, env_conditions, soil_props)
          oxygen <- dynamic_result$final_oxygen
          
          # Store dynamic results
          model_results$time_series <- dynamic_result$oxygen_series
          model_results$env_conditions <- dynamic_result$env_conditions
          model_results$diffusion_series <- dynamic_result$diffusion_series
          model_results$consumption_series <- dynamic_result$consumption_series
          model_results$dt_hours <- dt_hours
          
        } else {
          # Static time-dependent (original method)
          dz_m <- (depths[2] - depths[1]) / 100
          max_D <- max(diffusion) * 1e-6
          stable_dt_hours <- 0.2 * dz_m^2 / max_D / 3600
          dt_hours <- min(stable_dt_hours, 0.1)
          
          time_result <- solve_time_dependent(depths, diffusion, consumption, 
                                            dt_hours, input$time_hours)
          oxygen <- time_result$final_profile
          model_results$time_series <- time_result$time_series
          model_results$dt_hours <- dt_hours
        }
      }
      
      incProgress(0.8, detail = "Preparing results...")
      
      # Store results
      model_results$oxygen <- oxygen
      model_results$diffusion <- diffusion
      model_results$consumption <- consumption
      model_results$depths <- depths
      
      # Create layer summary
      layer_boundaries <- c(0, 30, 60, 90, input$depth_max)
      layer_names <- c("Surface (0-30cm)", "Transition (30-60cm)", "Subsoil (60-90cm)", "Deep (90+cm)")
      
      layer_data <- data.frame(
        Layer = layer_names,
        Porosity = c(input$l1_porosity, input$l2_porosity, input$l3_porosity, input$l4_porosity),
        Organic_Matter = c(input$l1_organic, input$l2_organic, input$l3_organic, input$l4_organic),
        Root_Respiration = c(input$l1_root_resp, input$l2_root_resp, input$l3_root_resp, input$l4_root_resp),
        Avg_Diffusion = sapply(1:4, function(j) {
          idx <- which(depths >= layer_boundaries[j] & depths < layer_boundaries[j+1])
          if(length(idx) > 0) mean(diffusion[idx]) else 0
        }),
        Avg_Consumption = sapply(1:4, function(j) {
          idx <- which(depths >= layer_boundaries[j] & depths < layer_boundaries[j+1])
          if(length(idx) > 0) mean(consumption[idx]) else 0
        }),
        Avg_Oxygen = sapply(1:4, function(j) {
          idx <- which(depths >= layer_boundaries[j] & depths < layer_boundaries[j+1])
          if(length(idx) > 0) mean(oxygen[idx]) else 0
        })
      )
      
      model_results$layer_summary <- layer_data
      
      incProgress(1, detail = "Complete!")
    })
    
    output$model_status <- renderText({
      "Model calculation completed successfully!"
    })
  })
  
  # Oxygen profile plot
  output$oxygen_profile <- renderPlotly({
    req(model_results$oxygen)
    
    # Convert to percentage
    oxygen_pct <- model_results$oxygen * 100 / 0.21
    
    p <- plot_ly(x = oxygen_pct, y = -model_results$depths, type = 'scatter', mode = 'lines+markers',
                line = list(color = 'blue', width = 3),
                marker = list(size = 4, color = 'darkblue'),
                name = 'O₂ Concentration') %>%
      add_trace(x = c(0, 21), y = c(-30, -30), mode = 'lines', 
                line = list(color = 'red', dash = 'dash'), name = 'Layer 1-2 Boundary') %>%
      add_trace(x = c(0, 21), y = c(-60, -60), mode = 'lines', 
                line = list(color = 'orange', dash = 'dash'), name = 'Layer 2-3 Boundary') %>%
      add_trace(x = c(0, 21), y = c(-90, -90), mode = 'lines', 
                line = list(color = 'brown', dash = 'dash'), name = 'Layer 3-4 Boundary') %>%
      layout(title = 'Soil Oxygen Concentration Profile',
             xaxis = list(title = 'Oxygen Concentration (%)'),
             yaxis = list(title = 'Depth (cm)', autorange = 'reversed'),
             hovermode = 'closest')
    
    p
  })
  
  # Key metrics
  output$surface_o2 <- renderValueBox({
    req(model_results$oxygen)
    surface_val <- round(model_results$oxygen[1] * 100 / 0.21, 1)
    valueBox(
      value = paste0(surface_val, "%"),
      subtitle = "Surface O₂",
      icon = icon("leaf"),
      color = "green"
    )
  })
  
  output$critical_depth <- renderValueBox({
    req(model_results$oxygen)
    # Find depth where O2 drops below 10%
    critical_idx <- which(model_results$oxygen < 0.021)[1]
    critical_depth <- if(!is.na(critical_idx)) model_results$depths[critical_idx] else model_results$depths[length(model_results$depths)]
    
    valueBox(
      value = paste0(round(critical_depth, 0), " cm"),
      subtitle = "Critical Depth (<10% O₂)",
      icon = icon("exclamation-triangle"),
      color = "yellow"
    )
  })
  
  output$anaerobic_zone <- renderValueBox({
    req(model_results$oxygen)
    # Find depth where O2 drops below 2%
    anaerobic_idx <- which(model_results$oxygen < 0.0042)[1]
    anaerobic_depth <- if(!is.na(anaerobic_idx)) model_results$depths[anaerobic_idx] else "None"
    
    valueBox(
      value = if(anaerobic_depth != "None") paste0(round(anaerobic_depth, 0), " cm") else "None",
      subtitle = "Anaerobic Zone (<2% O₂)",
      icon = icon("ban"),
      color = "red"
    )
  })
  
  # Diffusion profile plot
  output$diffusion_profile <- renderPlotly({
    req(model_results$diffusion)
    
    plot_ly(x = model_results$diffusion, y = -model_results$depths, 
            type = 'scatter', mode = 'lines+markers',
            line = list(color = 'green', width = 2),
            marker = list(size = 3, color = 'darkgreen')) %>%
      layout(title = 'Diffusion Coefficient Profile',
             xaxis = list(title = 'Diffusion Coefficient (cm²/s × 10⁶)'),
             yaxis = list(title = 'Depth (cm)', autorange = 'reversed'))
  })
  
  # Consumption profile plot
  output$consumption_profile <- renderPlotly({
    req(model_results$consumption)
    
    plot_ly(x = model_results$consumption, y = -model_results$depths, 
            type = 'scatter', mode = 'lines+markers',
            line = list(color = 'red', width = 2),
            marker = list(size = 3, color = 'darkred')) %>%
      layout(title = 'Oxygen Consumption Profile',
             xaxis = list(title = 'Consumption Rate (kg O₂/m³/day)'),
             yaxis = list(title = 'Depth (cm)', autorange = 'reversed'))
  })
  
  # Layer comparison table
  output$layer_table <- DT::renderDataTable({
    req(model_results$layer_summary)
    
    DT::datatable(model_results$layer_summary, 
                  options = list(pageLength = 10, scrollX = TRUE),
                  caption = "Layer-by-Layer Soil Properties Summary") %>%
      DT::formatRound(columns = c('Porosity', 'Organic_Matter', 'Root_Respiration', 
                                  'Avg_Diffusion', 'Avg_Consumption', 'Avg_Oxygen'), 
                      digits = 3)
  })
  
  # Physical properties comparison
  output$physical_comparison <- renderPlotly({
    req(model_results$layer_summary)
    
    plot_ly(data = model_results$layer_summary, x = ~Layer, y = ~Porosity, 
            type = 'bar', name = 'Porosity', marker = list(color = 'lightblue')) %>%
      add_trace(y = ~Avg_Diffusion * 1000, name = 'Diffusion (×1000)', 
                marker = list(color = 'darkblue')) %>%
      layout(title = 'Physical Properties by Layer',
             yaxis = list(title = 'Value'),
             barmode = 'group')
  })
  
  # Biological activity comparison
  output$biological_comparison <- renderPlotly({
    req(model_results$layer_summary)
    
    plot_ly(data = model_results$layer_summary, x = ~Layer, y = ~Root_Respiration, 
            type = 'bar', name = 'Root Respiration', marker = list(color = 'lightgreen')) %>%
      add_trace(y = ~Avg_Consumption, name = 'Total Consumption', 
                marker = list(color = 'darkgreen')) %>%
      layout(title = 'Biological Activity by Layer',
             yaxis = list(title = 'Rate (kg O₂/m³/day)'),
             barmode = 'group')
  })
  
  # Environmental conditions preview
  output$environmental_preview <- renderPlot({
    if (input$enable_dynamic) {
      # Generate preview with current settings
      preview_time <- 72  # 3 days for preview
      dt_preview <- 0.5   # 30-minute intervals for preview
      preview_conditions <- generate_environmental_conditions(preview_time, dt_preview)
      
      par(mfrow = c(2, 1), mar = c(3, 4, 2, 1))
      
      # Temperature plot
      plot(preview_conditions$time, preview_conditions$temperature, 
           type = "l", col = "red", lwd = 2,
           xlab = "", ylab = "Temperature (°C)", 
           main = "Temperature Pattern")
      grid()
      
      # Moisture plot
      plot(preview_conditions$time, preview_conditions$moisture, 
           type = "l", col = "blue", lwd = 2,
           xlab = "Time (hours)", ylab = "Moisture (m³/m³)", 
           main = "Moisture Pattern")
      grid()
    }
  })

  # Enhanced time series plot with environmental data
  output$time_series_plot <- renderPlotly({
    if (!is.null(model_results$time_series)) {
      
      if (!is.null(model_results$env_conditions)) {
        # Dynamic conditions plot
        time_hours <- model_results$env_conditions$time
        
        # Create subplot with environmental conditions and oxygen
        p1 <- plot_ly(x = time_hours, y = model_results$env_conditions$temperature, 
                     type = 'scatter', mode = 'lines', name = 'Temperature',
                     line = list(color = 'red', width = 2)) %>%
          layout(yaxis = list(title = 'Temperature (°C)', side = 'left'))
        
        p2 <- plot_ly(x = time_hours, y = model_results$env_conditions$moisture, 
                     type = 'scatter', mode = 'lines', name = 'Moisture',
                     line = list(color = 'blue', width = 2)) %>%
          layout(yaxis = list(title = 'Moisture (m³/m³)', side = 'left'))
        
        # Select depths for oxygen plot
        depth_indices <- seq(1, length(model_results$depths), by = 6)
        selected_depths <- model_results$depths[depth_indices]
        
        p3 <- plot_ly()
        colors <- rainbow(length(depth_indices))
        
        for (i in seq_along(depth_indices)) {
          idx <- depth_indices[i]
          oxygen_pct <- model_results$time_series[, idx] * 100 / 0.21
          
          p3 <- p3 %>% add_trace(x = time_hours, y = oxygen_pct, 
                                type = 'scatter', mode = 'lines',
                                name = paste0(selected_depths[i], " cm"),
                                line = list(color = colors[i], width = 2))
        }
        
        p3 <- p3 %>% layout(yaxis = list(title = 'Oxygen (%)'))
        
        # Combine plots
        subplot(p1, p2, p3, nrows = 3, shareX = TRUE, titleY = TRUE) %>%
          layout(title = 'Dynamic Environmental Conditions and Oxygen Response',
                 xaxis = list(title = 'Time (hours)'))
        
      } else {
        # Static conditions time series
        time_hours <- seq(0, (nrow(model_results$time_series)-1) * model_results$dt_hours, 
                         by = model_results$dt_hours)
        
        depth_indices <- seq(1, length(model_results$depths), by = 4)
        selected_depths <- model_results$depths[depth_indices]
        
        p <- plot_ly()
        
        for (i in seq_along(depth_indices)) {
          idx <- depth_indices[i]
          oxygen_pct <- model_results$time_series[, idx] * 100 / 0.21
          
          p <- p %>% add_trace(x = time_hours, y = oxygen_pct, 
                              type = 'scatter', mode = 'lines',
                              name = paste0(selected_depths[i], " cm"),
                              line = list(width = 2))
        }
        
        p %>% layout(
          title = 'Oxygen Concentration Over Time (Static Conditions)',
          xaxis = list(title = 'Time (hours)'),
          yaxis = list(title = 'Oxygen Concentration (%)'),
          hovermode = 'closest'
        )
      }
      
    } else {
      plot_ly() %>%
        add_annotations(
          text = "Run the model with 'Steady State Solution' unchecked\nto see time-dependent oxygen dynamics!",
          x = 0.5, y = 0.5, 
          xref = "paper", yref = "paper",
          showarrow = FALSE,
          font = list(size = 16)
        ) %>%
        layout(xaxis = list(showgrid = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, showticklabels = FALSE))
    }
  })
  
  # Dynamic model runner for time series tab
  observeEvent(input$run_dynamic, {
    if (input$enable_dynamic) {
      # Trigger the main calculation with dynamic conditions
      updateCheckboxInput(session, "steady_state", value = FALSE)
      
      # Show notification
      showNotification("Dynamic simulation enabled! Use the 'Calculate' button in Parameters tab to run.", 
                       type = "message", duration = 5)
    } else {
      showNotification("Enable 'Dynamic Conditions' first, then set your environmental patterns!", 
                       type = "warning", duration = 5)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)