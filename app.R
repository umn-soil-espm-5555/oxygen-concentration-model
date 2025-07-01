library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(dplyr)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Multi-Layer Soil Oxygen Diffusion Model"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Oxygen Profile", tabName = "profile", icon = icon("chart-line")),
      menuItem("Soil Layers", tabName = "soil_layers", icon = icon("layer-group")),
      menuItem("Respiration Components", tabName = "respiration", icon = icon("leaf")),
      menuItem("Model Theory", tabName = "theory", icon = icon("book"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Main oxygen profile tab
      tabItem(tabName = "profile",
        fluidRow(
          box(width = 4, status = "primary", solidHeader = TRUE,
              title = "Environmental Conditions",
              
              h4("Atmospheric Conditions"),
              numericInput("atmospheric_pressure", "Atmospheric Pressure (kPa)", 
                          value = 101.3, min = 80, max = 110, step = 0.1),
              numericInput("temperature", "Temperature (°C)", 
                          value = 20, min = 5, max = 35, step = 1),
              numericInput("atmospheric_O2", "Atmospheric O₂ (%)", 
                          value = 20.9, min = 15, max = 25, step = 0.1),
              
              h4("Model Domain"),
              numericInput("max_depth", "Maximum Depth (cm)", 
                          value = 150, min = 50, max = 300, step = 10),
              numericInput("depth_resolution", "Depth Resolution (cm)", 
                          value = 1, min = 0.1, max = 5, step = 0.1),
              
              h4("Display Units"),
              selectInput("conc_units", "Concentration Units:",
                         choices = list("kg/m³" = "kg_m3", "mg/m³" = "mg_m3", "%" = "percent"),
                         selected = "mg_m3"),
              
              hr(),
              actionButton("update_model", "Update Model", 
                          class = "btn-primary", width = "100%")
          ),
          
          box(width = 8, status = "primary", solidHeader = TRUE,
              title = "Oxygen Concentration Profile",
              plotlyOutput("oxygen_profile", height = "500px")
          )
        ),
        
        fluidRow(
          box(width = 6, status = "info", solidHeader = TRUE,
              title = "Calculated Parameters",
              verbatimTextOutput("calculated_params")
          ),
          
          box(width = 6, status = "info", solidHeader = TRUE,
              title = "Model Information",
              verbatimTextOutput("model_info")
          )
        )
      ),
      
      # Soil layers tab
      tabItem(tabName = "soil_layers",
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE,
              title = "Soil Layer Configuration",
              p("Define up to 4 soil layers with different properties. Layers must be sequential and non-overlapping."),
              
              fluidRow(
                column(3,
                  h4("Layer 1 (Surface)", style = "color: #3c8dbc;"),
                  numericInput("layer1_top", "Top Depth (cm)", value = 0, min = 0, max = 300, step = 1),
                  numericInput("layer1_bottom", "Bottom Depth (cm)", value = 30, min = 1, max = 300, step = 1),
                  checkboxInput("layer1_active", "Active", value = TRUE),
                  
                  conditionalPanel(
                    condition = "input.layer1_active",
                    h5("Physical Properties"),
                    numericInput("layer1_porosity", "Total Porosity", value = 0.50, min = 0.2, max = 0.8, step = 0.01),
                    numericInput("layer1_wfps", "Water-Filled Pore Space (%)", value = 50, min = 0, max = 100, step = 1),
                    
                    h5("Respiration Properties"),
                    numericInput("layer1_root_density", "Root Density (kg/m³)", value = 1.0, min = 0, max = 10, step = 0.1),
                    numericInput("layer1_som", "SOM Content (%)", value = 4.0, min = 0, max = 15, step = 0.1)
                  )
                ),
                
                column(3,
                  h4("Layer 2", style = "color: #00a65a;"),
                  numericInput("layer2_top", "Top Depth (cm)", value = 30, min = 0, max = 300, step = 1),
                  numericInput("layer2_bottom", "Bottom Depth (cm)", value = 60, min = 1, max = 300, step = 1),
                  checkboxInput("layer2_active", "Active", value = TRUE),
                  
                  conditionalPanel(
                    condition = "input.layer2_active",
                    h5("Physical Properties"),
                    numericInput("layer2_porosity", "Total Porosity", value = 0.45, min = 0.2, max = 0.8, step = 0.01),
                    numericInput("layer2_wfps", "Water-Filled Pore Space (%)", value = 65, min = 0, max = 100, step = 1),
                    
                    h5("Respiration Properties"),
                    numericInput("layer2_root_density", "Root Density (kg/m³)", value = 0.5, min = 0, max = 10, step = 0.1),
                    numericInput("layer2_som", "SOM Content (%)", value = 2.5, min = 0, max = 15, step = 0.1)
                  )
                ),
                
                column(3,
                  h4("Layer 3", style = "color: #f39c12;"),
                  numericInput("layer3_top", "Top Depth (cm)", value = 60, min = 0, max = 300, step = 1),
                  numericInput("layer3_bottom", "Bottom Depth (cm)", value = 100, min = 1, max = 300, step = 1),
                  checkboxInput("layer3_active", "Active", value = TRUE),
                  
                  conditionalPanel(
                    condition = "input.layer3_active",
                    h5("Physical Properties"),
                    numericInput("layer3_porosity", "Total Porosity", value = 0.40, min = 0.2, max = 0.8, step = 0.01),
                    numericInput("layer3_wfps", "Water-Filled Pore Space (%)", value = 80, min = 0, max = 100, step = 1),
                    
                    h5("Respiration Properties"),
                    numericInput("layer3_root_density", "Root Density (kg/m³)", value = 0.2, min = 0, max = 10, step = 0.1),
                    numericInput("layer3_som", "SOM Content (%)", value = 1.5, min = 0, max = 15, step = 0.1)
                  )
                ),
                
                column(3,
                  h4("Layer 4 (Deep)", style = "color: #dd4b39;"),
                  numericInput("layer4_top", "Top Depth (cm)", value = 100, min = 0, max = 300, step = 1),
                  numericInput("layer4_bottom", "Bottom Depth (cm)", value = 150, min = 1, max = 300, step = 1),
                  checkboxInput("layer4_active", "Active", value = TRUE),
                  
                  conditionalPanel(
                    condition = "input.layer4_active",
                    h5("Physical Properties"),
                    numericInput("layer4_porosity", "Total Porosity", value = 0.35, min = 0.2, max = 0.8, step = 0.01),
                    numericInput("layer4_wfps", "Water-Filled Pore Space (%)", value = 90, min = 0, max = 100, step = 1),
                    
                    h5("Respiration Properties"),
                    numericInput("layer4_root_density", "Root Density (kg/m³)", value = 0.05, min = 0, max = 10, step = 0.01),
                    numericInput("layer4_som", "SOM Content (%)", value = 1.0, min = 0, max = 15, step = 0.1)
                  )
                )
              )
          )
        ),
        
        fluidRow(
          box(width = 6, status = "primary", solidHeader = TRUE,
              title = "Soil Properties vs Depth",
              plotlyOutput("soil_properties_plot", height = "500px")
          ),
          
          box(width = 6, status = "primary", solidHeader = TRUE,
              title = "Diffusion Coefficient vs Depth",
              plotlyOutput("diffusion_profile", height = "500px")
          )
        ),
        
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE,
              title = "Global Respiration Parameters",
              
              fluidRow(
                column(4,
                  h4("Root Parameters"),
                  numericInput("root_resp_rate", "Root Respiration Rate (kg O₂/kg root/s)", 
                              value = 1e-6, min = 1e-8, max = 1e-4, step = 1e-7),
                  numericInput("Q10_roots", "Root Q₁₀", 
                              value = 2.0, min = 1.5, max = 3.5, step = 0.1)
                ),
                
                column(4,
                  h4("Microbial Parameters"),
                  numericInput("microbial_resp_rate", "Microbial Resp. Rate (kg O₂/kg SOM/s)", 
                              value = 2e-7, min = 1e-9, max = 1e-5, step = 1e-8),
                  numericInput("Q10_microbes", "Microbial Q₁₀", 
                              value = 2.5, min = 1.5, max = 4.0, step = 0.1)
                ),
                
                column(4,
                  h4("Environmental Controls"),
                  selectInput("tortuosity_model", "Tortuosity Model:",
                             choices = list(
                               "Millington-Quirk" = "millington_quirk",
                               "Moldrup et al." = "moldrup",
                               "Custom" = "custom"
                             ), selected = "millington_quirk"),
                  
                  conditionalPanel(
                    condition = "input.tortuosity_model == 'custom'",
                    numericInput("custom_tortuosity", "Custom Tortuosity Factor", 
                               value = 0.66, min = 0.1, max = 1.0, step = 0.01)
                  ),
                  
                  numericInput("moisture_optimum", "Optimal Water Content", 
                              value = 0.3, min = 0.1, max = 0.5, step = 0.01)
                )
              )
          )
        )
      ),
      
      # Respiration tab
      tabItem(tabName = "respiration",
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE,
              title = "Respiration Components vs Depth",
              plotlyOutput("respiration_components", height = "500px")
          )
        ),
        
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE,
              title = "Respiration Data Table",
              DT::dataTableOutput("respiration_table")
          )
        )
      ),
      
      # Theory tab
      tabItem(tabName = "theory",
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE,
              title = "Multi-Layer Model Theory",
              
              h3("1. Multi-Layer Diffusion"),
              p("For a layered soil system, the diffusion equation is solved piecewise:"),
              withMathJax("$$\\frac{d}{dz}\\left(D_i\\frac{dC}{dz}\\right) = q_i(z) \\text{ for layer } i$$"),
              
              p("With boundary conditions at layer interfaces ensuring continuity of concentration and flux."),
              
              h3("2. Layer-Specific Properties"),
              h4("Diffusion Coefficient in Each Layer:"),
              withMathJax("$$D_{eff,i} = D_0(T) \\cdot \\frac{\\theta_{a,i}}{\\phi_i} \\cdot \\tau_i$$"),
              
              h4("Respiration in Each Layer:"),
              withMathJax("$$q_i(z) = \\rho_{root,i} \\cdot R_{root} \\cdot f_T + SOM_i \\cdot R_{microbe} \\cdot f_T \\cdot f_{\\theta}$$"),
              
              h3("3. Unit Conversions"),
              h4("Oxygen Concentration Units:"),
              tags$ul(
                tags$li("kg/m³ → mg/m³: multiply by 1000"),
                tags$li("kg/m³ → %: (C/C_atm) × 100"),
                tags$li("At 20°C, 1 atm: C_atm ≈ 0.276 kg/m³")
              ),
              
              h4("Standard Conditions:"),
              p("Atmospheric O₂ concentration at 20°C, 1 atm ≈ 276 mg/m³ (20.9%)"),
              
              h3("4. Typical Layer Properties"),
              tags$div(
                style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-top: 15px;",
                h4("Surface Layer (0-30 cm):"),
                p("• High porosity (0.45-0.55)"),
                p("• High root density (0.5-2.0 kg/m³)"),
                p("• High SOM content (3-6%)"),
                
                h4("Subsoil Layers (30-100 cm):"),
                p("• Moderate porosity (0.35-0.45)"),
                p("• Decreasing root density (0.1-0.5 kg/m³)"),
                p("• Lower SOM content (1-3%)"),
                
                h4("Deep Layer (>100 cm):"),
                p("• Lower porosity (0.30-0.40)"),
                p("• Minimal roots (<0.1 kg/m³)"),
                p("• Low SOM content (<1.5%)")
              )
            )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive values to store model results
  model_results <- reactiveVal(NULL)
  
  # Get active layers
  get_active_layers <- reactive({
    layers <- list()
    
    if (input$layer1_active) {
      water_content <- input$layer1_porosity * (input$layer1_wfps / 100)
      layers[[1]] <- list(
        top = input$layer1_top/100, bottom = input$layer1_bottom/100,
        porosity = input$layer1_porosity, 
        wfps = input$layer1_wfps,
        water = water_content,
        root_density = input$layer1_root_density, som = input$layer1_som/100,
        color = "#3c8dbc", name = "Layer 1"
      )
    }
    
    if (input$layer2_active) {
      water_content <- input$layer2_porosity * (input$layer2_wfps / 100)
      layers[[length(layers) + 1]] <- list(
        top = input$layer2_top/100, bottom = input$layer2_bottom/100,
        porosity = input$layer2_porosity,
        wfps = input$layer2_wfps,
        water = water_content,
        root_density = input$layer2_root_density, som = input$layer2_som/100,
        color = "#00a65a", name = "Layer 2"
      )
    }
    
    if (input$layer3_active) {
      water_content <- input$layer3_porosity * (input$layer3_wfps / 100)
      layers[[length(layers) + 1]] <- list(
        top = input$layer3_top/100, bottom = input$layer3_bottom/100,
        porosity = input$layer3_porosity,
        wfps = input$layer3_wfps,
        water = water_content,
        root_density = input$layer3_root_density, som = input$layer3_som/100,
        color = "#f39c12", name = "Layer 3"
      )
    }
    
    if (input$layer4_active) {
      water_content <- input$layer4_porosity * (input$layer4_wfps / 100)
      layers[[length(layers) + 1]] <- list(
        top = input$layer4_top/100, bottom = input$layer4_bottom/100,
        porosity = input$layer4_porosity,
        wfps = input$layer4_wfps,
        water = water_content,
        root_density = input$layer4_root_density, som = input$layer4_som/100,
        color = "#dd4b39", name = "Layer 4"
      )
    }
    
    return(layers)
  })
  
  # Calculate layer properties at each depth
  calculate_layer_properties <- reactive({
    depths <- seq(0, input$max_depth/100, by = input$depth_resolution/100)
    layers <- get_active_layers()
    
    # Initialize vectors
    porosity <- numeric(length(depths))
    water_content <- numeric(length(depths))
    root_density <- numeric(length(depths))
    som_content <- numeric(length(depths))
    layer_id <- numeric(length(depths))
    
    # Define transition zone width (in meters)
    transition_width <- input$depth_resolution/100 * 2  # 2 grid points worth
    
    # Assign properties based on layers with smooth transitions
    for (i in seq_along(depths)) {
      depth <- depths[i]
      
      # Find which layer this depth belongs to
      layer_found <- FALSE
      for (j in seq_along(layers)) {
        if (depth >= layers[[j]]$top && depth <= layers[[j]]$bottom) {
          layer_found <- TRUE
          
          # Check if we're near a layer boundary and need transition
          at_top_boundary <- abs(depth - layers[[j]]$top) < transition_width && j > 1
          at_bottom_boundary <- abs(depth - layers[[j]]$bottom) < transition_width && j < length(layers)
          
          if (at_top_boundary) {
            # Transition from previous layer
            prev_layer <- layers[[j-1]]
            transition_factor <- (depth - layers[[j]]$top + transition_width) / (2 * transition_width)
            transition_factor <- max(0, min(1, transition_factor))
            
            porosity[i] <- prev_layer$porosity * (1 - transition_factor) + layers[[j]]$porosity * transition_factor
            water_content[i] <- prev_layer$water * (1 - transition_factor) + layers[[j]]$water * transition_factor
            root_density[i] <- prev_layer$root_density * (1 - transition_factor) + layers[[j]]$root_density * transition_factor
            som_content[i] <- prev_layer$som * (1 - transition_factor) + layers[[j]]$som * transition_factor
            layer_id[i] <- j
            
          } else if (at_bottom_boundary) {
            # Transition to next layer
            next_layer <- layers[[j+1]]
            transition_factor <- (layers[[j]]$bottom - depth + transition_width) / (2 * transition_width)
            transition_factor <- max(0, min(1, transition_factor))
            
            porosity[i] <- layers[[j]]$porosity * transition_factor + next_layer$porosity * (1 - transition_factor)
            water_content[i] <- layers[[j]]$water * transition_factor + next_layer$water * (1 - transition_factor)
            root_density[i] <- layers[[j]]$root_density * transition_factor + next_layer$root_density * (1 - transition_factor)
            som_content[i] <- layers[[j]]$som * transition_factor + next_layer$som * (1 - transition_factor)
            layer_id[i] <- j
            
          } else {
            # Interior of layer - use layer properties directly
            porosity[i] <- layers[[j]]$porosity
            water_content[i] <- layers[[j]]$water
            root_density[i] <- layers[[j]]$root_density
            som_content[i] <- layers[[j]]$som
            layer_id[i] <- j
          }
          break
        }
      }
      
      # If no layer found, use last layer's properties
      if (!layer_found && length(layers) > 0) {
        last_layer <- layers[[length(layers)]]
        porosity[i] <- last_layer$porosity
        water_content[i] <- last_layer$water
        root_density[i] <- last_layer$root_density
        som_content[i] <- last_layer$som
        layer_id[i] <- length(layers)
      }
    }
    
    list(
      depths = depths,
      porosity = porosity,
      water_content = water_content,
      root_density = root_density,
      som_content = som_content,
      layer_id = layer_id,
      layers = layers
    )
  })
  
  # Calculate diffusion coefficient
  calculate_diffusion <- reactive({
    layer_props <- calculate_layer_properties()
    
    # Temperature in Kelvin
    T_K <- input$temperature + 273.15
    
    # Free air diffusion coefficient (temperature corrected)
    D0_base <- 2.01e-5  # m²/s
    D0 <- D0_base * (T_K / 293.15)^1.75
    
    # Air-filled porosity - this is critical for oxygen diffusion
    air_porosity <- pmax(0.001, layer_props$porosity - layer_props$water_content)
    
    # Calculate WFPS for each depth
    wfps <- (layer_props$water_content / layer_props$porosity) * 100
    
    # More realistic tortuosity calculation
    if (input$tortuosity_model == "millington_quirk") {
      # Classic Millington-Quirk model
      tortuosity <- (air_porosity^(10/3)) / (layer_props$porosity^2)
    } else if (input$tortuosity_model == "moldrup") {
      # Moldrup model
      tortuosity <- (air_porosity^2.5) / (layer_props$porosity^1.5)
    } else {
      tortuosity <- rep(input$custom_tortuosity, length(layer_props$depths))
    }
    
    # Ensure minimum tortuosity
    tortuosity[tortuosity < 1e-6] <- 1e-6
    
    # Effective diffusion coefficient with realistic WFPS sensitivity
    air_fraction <- air_porosity / layer_props$porosity
    
    # Create WFPS-based reduction factor that's more realistic
    # Oxygen diffusion starts to become limited around 60-70% WFPS
    # and becomes negligible around 85-90% WFPS
    wfps_factor <- rep(1, length(wfps))
    
    # Gradual decline from 60% to 85% WFPS
    moderate_wet <- wfps >= 60 & wfps < 85
    wfps_factor[moderate_wet] <- (85 - wfps[moderate_wet]) / 25  # Linear decline
    
    # Very low diffusion above 85% WFPS
    very_wet <- wfps >= 85
    wfps_factor[very_wet] <- 0.01 * exp(-(wfps[very_wet] - 85) / 5)  # Exponential decline
    
    # Standard diffusion calculation
    D_eff <- D0 * air_fraction * tortuosity * wfps_factor
    
    # Absolute minimum to prevent numerical issues
    D_eff[D_eff < 1e-15] <- 1e-15
    
    list(
      depths = layer_props$depths,
      D0 = D0,
      porosity = layer_props$porosity,
      air_porosity = air_porosity,
      air_fraction = air_fraction,
      wfps = wfps,
      wfps_factor = wfps_factor,
      tortuosity = tortuosity,
      D_eff = D_eff,
      layer_id = layer_props$layer_id
    )
  })
  
  # Calculate respiration components
  calculate_respiration <- reactive({
    layer_props <- calculate_layer_properties()
    
    # Temperature factors
    temp_factor_roots <- input$Q10_roots^((input$temperature - 20)/10)
    temp_factor_microbes <- input$Q10_microbes^((input$temperature - 20)/10)
    
    # Root respiration
    root_respiration <- layer_props$root_density * input$root_resp_rate * temp_factor_roots
    
    # Microbial respiration
    # Convert SOM content to density (assuming bulk density ~1300 kg/m³)
    som_density <- layer_props$som_content * 1300
    
    # Moisture stress factor (simplified)
    moisture_stress <- exp(-((layer_props$water_content - input$moisture_optimum) / (input$moisture_optimum * 0.5))^2)
    
    microbial_respiration <- som_density * input$microbial_resp_rate * temp_factor_microbes * moisture_stress
    
    # Total respiration
    total_respiration <- root_respiration + microbial_respiration
    
    list(
      depths = layer_props$depths,
      root_respiration = root_respiration,
      microbial_respiration = microbial_respiration,
      total_respiration = total_respiration,
      root_density = layer_props$root_density,
      som_density = som_density,
      layer_id = layer_props$layer_id
    )
  })
  
  # Calculate surface oxygen concentration
  calculate_surface_O2 <- reactive({
    # Convert to SI units
    P <- input$atmospheric_pressure * 1000  # Pa
    f_O2 <- input$atmospheric_O2 / 100      # fraction
    M_O2 <- 0.032                           # kg/mol
    R <- 8.314                              # J/(mol·K)
    T <- input$temperature + 273.15         # K
    
    # Ideal gas law: C = (P * f * M) / (R * T)
    C_a <- (P * f_O2 * M_O2) / (R * T)
    return(C_a)
  })
  
  # Convert concentration units
  convert_concentration <- function(conc_kg_m3, units, C_atm) {
    switch(units,
           "kg_m3" = conc_kg_m3,
           "mg_m3" = conc_kg_m3 * 1000,
           "percent" = (conc_kg_m3 / C_atm) * 100)
  }
  
  # Get concentration axis label
  get_conc_label <- function(units) {
    switch(units,
           "kg_m3" = "Oxygen Concentration (kg/m³)",
           "mg_m3" = "Oxygen Concentration (mg/m³)",
           "percent" = "Oxygen Concentration (%)")
  }
  
  # Scientific notation helper
  scientific <- function(x, digits = 2) {
    formatC(x, format = "e", digits = digits)
  }
  
  # Main model calculation using layer-by-layer analytical approach
  observeEvent(input$update_model, {
    
    # Get component calculations
    diffusion_data <- calculate_diffusion()
    respiration_data <- calculate_respiration()
    C_a <- calculate_surface_O2()
    
    depths <- diffusion_data$depths
    D_eff <- diffusion_data$D_eff
    total_respiration <- respiration_data$total_respiration
    layers <- get_active_layers()
    
    # Debug: Check values
    cat("Surface O2 concentration:", C_a, "kg/m³\n")
    cat("D_eff range:", range(D_eff), "\n")
    cat("Number of layers:", length(layers), "\n")
    
    # Layer-by-layer analytical solution with continuity
    n <- length(depths)
    concentration <- numeric(n)
    
    # Start with surface concentration
    current_surface_conc <- C_a
    
    # Solve each layer sequentially, ensuring continuity
    for (layer_idx in seq_along(layers)) {
      layer <- layers[[layer_idx]]
      
      # Find depths in this layer
      layer_mask <- diffusion_data$layer_id == layer_idx
      layer_depths <- depths[layer_mask]
      layer_indices <- which(layer_mask)
      
      if (length(layer_depths) > 0) {
        # Get average properties for this layer
        layer_D_values <- D_eff[layer_mask]
        layer_R_values <- total_respiration[layer_mask]
        
        avg_D <- mean(layer_D_values[layer_D_values > 0])
        avg_R <- mean(layer_R_values[is.finite(layer_R_values)])
        
        # Ensure finite values
        if (!is.finite(avg_D) || avg_D <= 0) avg_D <- 1e-12
        if (!is.finite(avg_R) || avg_R < 0) avg_R <- 0
        
        # Calculate penetration depth for this layer
        if (avg_R > 0) {
          lambda <- sqrt(avg_D / avg_R)  # penetration depth
        } else {
          lambda <- 1e6  # Very large if no respiration
        }
        
        # Solve within this layer: C(z) = C_top * exp(-(z-z_top)/lambda)
        # where z is measured from the top of the layer
        layer_top <- layer$top
        
        for (i in seq_along(layer_indices)) {
          idx <- layer_indices[i]
          relative_depth <- depths[idx] - layer_top
          
          # Exponential decay from the top of this layer
          concentration[idx] <- current_surface_conc * exp(-relative_depth / lambda)
        }
        
        # Set the surface concentration for the next layer
        # This is the concentration at the bottom of the current layer
        layer_bottom_depth <- layer$bottom - layer_top
        current_surface_conc <- current_surface_conc * exp(-layer_bottom_depth / lambda)
        
        # Ensure concentration doesn't go below zero
        current_surface_conc <- max(0, current_surface_conc)
      }
    }
    
    # Handle depths below the last defined layer
    last_layer_bottom <- if (length(layers) > 0) layers[[length(layers)]]$bottom else 0
    below_layers_mask <- depths > last_layer_bottom
    
    if (any(below_layers_mask)) {
      # Use very low diffusion and low respiration for depths below defined layers
      deep_D <- 1e-12
      deep_R <- 1e-9
      deep_lambda <- sqrt(deep_D / deep_R)
      
      below_indices <- which(below_layers_mask)
      for (idx in below_indices) {
        relative_depth <- depths[idx] - last_layer_bottom
        concentration[idx] <- current_surface_conc * exp(-relative_depth / deep_lambda)
      }
    }
    
    # Ensure non-negative concentrations
    concentration <- pmax(0, concentration)
    
    # Calculate oxygen flux at each depth
    flux <- numeric(n)
    dz <- input$depth_resolution/100
    for (i in 1:(n-1)) {
      flux[i] <- -D_eff[i] * (concentration[i+1] - concentration[i]) / dz
    }
    flux[n] <- flux[n-1]  # Boundary condition
    
    # Store results
    results <- data.frame(
      depth_m = depths,
      depth_cm = depths * 100,
      concentration = concentration,
      total_respiration = total_respiration,
      root_respiration = respiration_data$root_respiration,
      microbial_respiration = respiration_data$microbial_respiration,
      D_eff = D_eff,
      flux = flux,
      layer_id = diffusion_data$layer_id
    )
    
    # Add metadata
    attr(results, "C_a") <- C_a
    attr(results, "layers") <- get_active_layers()
    
    model_results(results)
  })
  
  # Initialize model on startup
  observe({
    if (is.null(model_results())) {
      # Auto-trigger with simple calculation
      diffusion_data <- calculate_diffusion()
      respiration_data <- calculate_respiration()
      C_a <- calculate_surface_O2()
      
      depths <- diffusion_data$depths
      n <- length(depths)
      
      # Simple exponential decay approximation for initialization
      avg_D <- mean(diffusion_data$D_eff[diffusion_data$D_eff > 0])
      avg_Q <- mean(respiration_data$total_respiration)
      
      concentration <- pmax(0, C_a * exp(-depths * sqrt(avg_Q / avg_D)))
      flux <- -avg_D * c(0, diff(concentration) / (input$depth_resolution/100))
      
      results <- data.frame(
        depth_m = depths,
        depth_cm = depths * 100,
        concentration = concentration,
        total_respiration = respiration_data$total_respiration,
        root_respiration = respiration_data$root_respiration,
        microbial_respiration = respiration_data$microbial_respiration,
        D_eff = diffusion_data$D_eff,
        flux = flux,
        layer_id = diffusion_data$layer_id
      )
      
      attr(results, "C_a") <- C_a
      attr(results, "layers") <- get_active_layers()
      
      model_results(results)
    }
  })
  
  # Main oxygen profile plot
  output$oxygen_profile <- renderPlotly({
    req(model_results())
    
    results <- model_results()
    C_a <- attr(results, "C_a")
    layers <- attr(results, "layers")
    
    # Convert concentration units
    conc_display <- convert_concentration(results$concentration, input$conc_units, C_a)
    conc_label <- get_conc_label(input$conc_units)
    
    p <- plot_ly(results, x = ~conc_display, y = ~-depth_cm, type = 'scatter', mode = 'lines',
                name = 'Oxygen Concentration', line = list(width = 3, color = 'blue')) %>%
      layout(
        title = "Multi-Layer Soil Oxygen Concentration Profile",
        xaxis = list(title = conc_label),
        yaxis = list(title = "Depth (cm)"),
        hovermode = 'closest'
      )
    
    # Add layer boundaries
    for (i in seq_along(layers)) {
      layer <- layers[[i]]
      p <- p %>% add_segments(
        x = 0, xend = max(conc_display), 
        y = -layer$top*100, yend = -layer$top*100,
        line = list(dash = "dot", color = layer$color, width = 1),
        name = paste(layer$name, "top"), showlegend = FALSE)
      
      p <- p %>% add_segments(
        x = 0, xend = max(conc_display), 
        y = -layer$bottom*100, yend = -layer$bottom*100,
        line = list(dash = "dot", color = layer$color, width = 1),
        name = paste(layer$name, "bottom"), showlegend = FALSE)
    }
    
    p
  })
  
  # Soil properties plot
  output$soil_properties_plot <- renderPlotly({
    layer_props <- calculate_layer_properties()
    
    df <- data.frame(
      depth_cm = layer_props$depths * 100,
      porosity = layer_props$porosity,
      water_content = layer_props$water_content,
      air_porosity = pmax(0, layer_props$porosity - layer_props$water_content),
      root_density = layer_props$root_density,
      som_content = layer_props$som_content * 100  # Convert to %
    )
    
    plot_ly(df, y = ~-depth_cm) %>%
      add_trace(x = ~porosity, name = 'Total Porosity', type = 'scatter', mode = 'lines',
               line = list(color = 'blue')) %>%
      add_trace(x = ~water_content, name = 'Water Content', type = 'scatter', mode = 'lines',
               line = list(color = 'cyan')) %>%
      add_trace(x = ~air_porosity, name = 'Air Porosity', type = 'scatter', mode = 'lines',
               line = list(color = 'red')) %>%
      layout(
        title = "Soil Physical Properties vs Depth",
        xaxis = list(title = "Volumetric Fraction"),
        yaxis = list(title = "Depth (cm)")
      )
  })
  
  # Diffusion profile plot
  output$diffusion_profile <- renderPlotly({
    diffusion_data <- calculate_diffusion()
    
    df <- data.frame(
      depth_cm = diffusion_data$depths * 100,
      D_eff = diffusion_data$D_eff * 1e6,  # Convert to mm²/s
      tortuosity = diffusion_data$tortuosity,
      layer_id = diffusion_data$layer_id
    )
    
    plot_ly(df, x = ~D_eff, y = ~-depth_cm, type = 'scatter', mode = 'lines',
           name = 'Effective Diffusivity', line = list(color = 'green', width = 3)) %>%
      layout(
        title = "Diffusion Coefficient vs Depth",
        xaxis = list(title = "Effective Diffusivity (mm²/s)"),
        yaxis = list(title = "Depth (cm)")
      )
  })
  
  # Respiration components plot
  output$respiration_components <- renderPlotly({
    respiration_data <- calculate_respiration()
    layers <- get_active_layers()
    
    df <- data.frame(
      depth_cm = respiration_data$depths * 100,
      root = respiration_data$root_respiration * 1e9,  # Convert to ng/m³/s
      microbial = respiration_data$microbial_respiration * 1e9,
      total = respiration_data$total_respiration * 1e9,
      layer_id = respiration_data$layer_id
    )
    
    p <- plot_ly(df, y = ~-depth_cm) %>%
      add_trace(x = ~root, name = 'Root Respiration', type = 'scatter', mode = 'lines',
               line = list(color = 'green')) %>%
      add_trace(x = ~microbial, name = 'Microbial Respiration', type = 'scatter', mode = 'lines',
               line = list(color = 'brown')) %>%
      add_trace(x = ~total, name = 'Total Respiration', type = 'scatter', mode = 'lines',
               line = list(color = 'black', width = 3)) %>%
      layout(
        title = "Respiration Components vs Depth",
        xaxis = list(title = "Respiration Rate (ng O₂/m³/s)"),
        yaxis = list(title = "Depth (cm)")
      )
    
    # Add layer boundaries
    for (i in seq_along(layers)) {
      layer <- layers[[i]]
      p <- p %>% add_segments(
        x = 0, xend = max(df$total), 
        y = -layer$top*100, yend = -layer$top*100,
        line = list(dash = "dot", color = layer$color, width = 2),
        name = paste(layer$name, "boundary"), showlegend = FALSE)
    }
    
    p
  })
  
  # Calculated parameters output
  output$calculated_params <- renderText({
    diffusion_data <- calculate_diffusion()
    respiration_data <- calculate_respiration()
    C_a <- calculate_surface_O2()
    layers <- get_active_layers()
    
    # Convert surface concentration to different units
    C_a_mg <- C_a * 1000
    C_a_percent <- (C_a / C_a) * 100  # This is always 100% at surface
    
    # Calculate moisture metrics
    surface_wfps <- diffusion_data$wfps[1]
    max_wfps <- max(diffusion_data$wfps)
    
    paste(
      paste("Number of Active Layers:", length(layers)),
      paste("Surface O₂ Concentration:"),
      paste("  ", round(C_a, 4), "kg/m³"),
      paste("  ", round(C_a_mg, 1), "mg/m³"),
      paste("  ", round(C_a_percent, 1), "%"),
      "",
      paste("Moisture Analysis:"),
      paste("  Surface WFPS:", round(surface_wfps, 1), "%"),
      paste("  Maximum WFPS:", round(max_wfps, 1), "%"),
      paste("  Layers >85% WFPS:", sum(diffusion_data$wfps > 85)),
      paste("  Surface air fraction:", round(diffusion_data$air_fraction[1], 3)),
      "",
      paste("Free Air Diffusivity:", round(diffusion_data$D0 * 1e6, 2), "mm²/s"),
      paste("Effective Diffusivity Range:"),
      paste("  ", scientific(min(diffusion_data$D_eff), 2), "-", 
            scientific(max(diffusion_data$D_eff), 2), "m²/s"),
      "",
      paste("Surface Respiration Components:"),
      paste("  Root:", round(respiration_data$root_respiration[1] * 1e9, 2), "ng/m³/s"),
      paste("  Microbial:", round(respiration_data$microbial_respiration[1] * 1e9, 2), "ng/m³/s"),
      paste("  Total:", round(respiration_data$total_respiration[1] * 1e9, 2), "ng/m³/s"),
      sep = "\n"
    )
  })
  
  # Model information
  output$model_info <- renderText({
    req(model_results())
    
    results <- model_results()
    C_a <- attr(results, "C_a")
    layers <- attr(results, "layers")
    
    # Calculate some summary statistics
    min_conc <- min(results$concentration)
    min_depth_cm <- results$depth_cm[which.min(results$concentration)]
    surface_flux <- results$flux[1]
    
    # Convert to different units
    min_conc_mg <- min_conc * 1000
    min_conc_percent <- (min_conc / C_a) * 100
    
    info <- paste(
      paste("Model Type: Multi-layer numerical solution"),
      paste("Active Layers:", length(layers)),
      "",
      paste("Minimum O₂ Concentration:"),
      paste("  ", round(min_conc, 6), "kg/m³"),
      paste("  ", round(min_conc_mg, 2), "mg/m³"),
      paste("  ", round(min_conc_percent, 1), "%"),
      paste("  at depth:", round(min_depth_cm, 1), "cm"),
      "",
      paste("Surface O₂ Flux:", round(surface_flux * 1e9, 2), "ng/m²/s"),
      paste("Depth Resolution:", input$depth_resolution, "cm"),
      sep = "\n"
    )
    
    return(info)
  })
  
  # Respiration data table
  output$respiration_table <- DT::renderDataTable({
    respiration_data <- calculate_respiration()
    layers <- get_active_layers()
    
    # Create layer names
    layer_names <- character(length(respiration_data$layer_id))
    for (i in seq_along(respiration_data$layer_id)) {
      if (respiration_data$layer_id[i] > 0 && respiration_data$layer_id[i] <= length(layers)) {
        layer_names[i] <- layers[[respiration_data$layer_id[i]]]$name
      } else {
        layer_names[i] <- "Outside layers"
      }
    }
    
    display_data <- data.frame(
      Depth_cm = round(respiration_data$depths * 100, 1),
      Layer = layer_names,
      Root_Density_kg_m3 = round(respiration_data$root_density, 3),
      SOM_Density_kg_m3 = round(respiration_data$som_density, 1),
      Root_Respiration_ng_m3_s = round(respiration_data$root_respiration * 1e9, 2),
      Microbial_Respiration_ng_m3_s = round(respiration_data$microbial_respiration * 1e9, 2),
      Total_Respiration_ng_m3_s = round(respiration_data$total_respiration * 1e9, 2)
    )
    
    datatable(display_data, 
              options = list(pageLength = 15, scrollY = "400px"),
              colnames = c("Depth (cm)", "Layer", "Root Density (kg/m³)", 
                          "SOM Density (kg/m³)", "Root Resp. (ng/m³/s)",
                          "Microbial Resp. (ng/m³/s)", "Total Resp. (ng/m³/s)"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)