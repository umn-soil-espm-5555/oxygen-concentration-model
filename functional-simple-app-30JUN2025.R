library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(dplyr)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Mechanistic Soil Oxygen Diffusion Model"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Oxygen Profile", tabName = "profile", icon = icon("chart-line")),
      menuItem("Soil Properties", tabName = "soil_props", icon = icon("layer-group")),
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
      
      # Soil properties tab
      tabItem(tabName = "soil_props",
        fluidRow(
          box(width = 6, status = "primary", solidHeader = TRUE,
              title = "Physical Soil Properties",
              
              h4("Porosity and Structure"),
              numericInput("total_porosity", "Total Porosity", 
                          value = 0.45, min = 0.2, max = 0.8, step = 0.01),
              numericInput("water_content", "Volumetric Water Content", 
                          value = 0.25, min = 0.05, max = 0.6, step = 0.01),
              
              h4("Tortuosity Model"),
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
              
              h4("Depth Variation"),
              checkboxInput("depth_varying_porosity", "Depth-varying porosity", FALSE),
              conditionalPanel(
                condition = "input.depth_varying_porosity",
                numericInput("porosity_decay", "Porosity decay rate (/m)", 
                           value = 0.5, min = 0, max = 2, step = 0.1)
              )
          ),
          
          box(width = 6, status = "primary", solidHeader = TRUE,
              title = "Calculated Diffusion Properties",
              plotlyOutput("diffusion_profile", height = "400px"),
              
              h4("Component Values"),
              verbatimTextOutput("diffusion_components")
          )
        )
      ),
      
      # Respiration tab
      tabItem(tabName = "respiration",
        fluidRow(
          box(width = 6, status = "primary", solidHeader = TRUE,
              title = "Root Respiration",
              
              h4("Root Distribution"),
              numericInput("root_density_surface", "Surface Root Density (kg/m³)", 
                          value = 0.5, min = 0, max = 5, step = 0.1),
              numericInput("root_decay_constant", "Root Decay Constant (m)", 
                          value = 0.3, min = 0.1, max = 1.0, step = 0.05),
              
              h4("Root Metabolic Activity"),
              numericInput("root_resp_rate", "Root Respiration Rate (kg O₂/kg root/s)", 
                          value = 1e-6, min = 1e-8, max = 1e-4, step = 1e-7),
              
              h4("Temperature Response"),
              numericInput("Q10_roots", "Root Q₁₀", 
                          value = 2.0, min = 1.5, max = 3.5, step = 0.1),
              numericInput("ref_temp_roots", "Reference Temperature (°C)", 
                          value = 20, min = 10, max = 30, step = 1)
          ),
          
          box(width = 6, status = "primary", solidHeader = TRUE,
              title = "Microbial Respiration",
              
              h4("Soil Organic Matter"),
              numericInput("som_content", "SOM Content (%)", 
                          value = 3.0, min = 0.5, max = 10, step = 0.1),
              numericInput("som_decay_constant", "SOM Decay Constant (m)", 
                          value = 0.2, min = 0.05, max = 0.5, step = 0.01),
              
              h4("Microbial Activity"),
              numericInput("microbial_resp_rate", "Microbial Resp. Rate (kg O₂/kg SOM/s)", 
                          value = 2e-7, min = 1e-9, max = 1e-5, step = 1e-8),
              
              h4("Environmental Controls"),
              numericInput("Q10_microbes", "Microbial Q₁₀", 
                          value = 2.5, min = 1.5, max = 4.0, step = 0.1),
              numericInput("moisture_optimum", "Optimal Water Content", 
                          value = 0.3, min = 0.1, max = 0.5, step = 0.01),
              numericInput("moisture_stress_factor", "Moisture Stress Sensitivity", 
                          value = 2.0, min = 1.0, max = 5.0, step = 0.1)
          )
        ),
        
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE,
              title = "Respiration Components vs Depth",
              plotlyOutput("respiration_components", height = "400px")
          )
        )
      ),
      
      # Theory tab
      tabItem(tabName = "theory",
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE,
              title = "Mechanistic Model Components",
              
              h3("1. Diffusion Coefficient Calculation"),
              p("The effective diffusion coefficient in soil is calculated as:"),
              withMathJax("$$D_{eff} = D_0 \\cdot \\frac{\\theta_a}{\\phi} \\cdot \\tau$$"),
              
              h4("Where:"),
              tags$ul(
                tags$li(withMathJax("$D_0$ = diffusion coefficient in free air")),
                tags$li(withMathJax("$\\theta_a$ = air-filled porosity")),
                tags$li(withMathJax("$\\phi$ = total porosity")),
                tags$li(withMathJax("$\\tau$ = tortuosity factor"))
              ),
              
              h4("Tortuosity Models:"),
              tags$ul(
                tags$li(withMathJax("Millington-Quirk: $\\tau = \\theta_a^{10/3} / \\phi^2$")),
                tags$li(withMathJax("Moldrup et al.: $\\tau = \\theta_a^{2.5} / \\phi^{1.5}$"))
              ),
              
              h3("2. Temperature Effects"),
              p("Diffusion coefficient temperature dependence:"),
              withMathJax("$$D_0(T) = D_{0,ref} \\cdot \\left(\\frac{T}{T_{ref}}\\right)^{1.75}$$"),
              
              h3("3. Respiration Components"),
              
              h4("Root Respiration:"),
              withMathJax("$$q_{root}(z) = \\rho_{root}(z) \\cdot R_{root} \\cdot Q_{10}^{(T-T_{ref})/10}$$"),
              withMathJax("$$\\rho_{root}(z) = \\rho_{root,0} \\cdot e^{-z/Z_{root}}$$"),
              
              h4("Microbial Respiration:"),
              withMathJax("$$q_{microbe}(z) = SOM(z) \\cdot R_{microbe} \\cdot Q_{10}^{(T-T_{ref})/10} \\cdot f(\\theta)$$"),
              withMathJax("$$SOM(z) = SOM_0 \\cdot e^{-z/Z_{SOM}}$$"),
              withMathJax("$$f(\\theta) = e^{-\\left(\\frac{\\theta - \\theta_{opt}}{\\sigma}\\right)^2}$$"),
              
              h3("4. Surface Oxygen Concentration"),
              p("Calculated from atmospheric conditions using ideal gas law:"),
              withMathJax("$$C_a = \\frac{P \\cdot f_{O2} \\cdot M_{O2}}{R \\cdot T}$$"),
              
              h4("Typical Parameter Values:"),
              tags$div(
                style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-top: 15px;",
                h4("Soil Physical Properties:"),
                p("• Total porosity: 0.3-0.6 (sandy to clay soils)"),
                p("• Air-filled porosity: 0.1-0.3"),
                p("• Tortuosity: 0.1-0.8"),
                
                h4("Root Properties:"),
                p("• Surface root density: 0.1-2.0 kg/m³"),
                p("• Root decay constant: 0.2-0.5 m"),
                p("• Root respiration rate: 1×10⁻⁷ to 1×10⁻⁵ kg O₂/kg root/s"),
                
                h4("Microbial Properties:"),
                p("• SOM content: 1-8%"),
                p("• Microbial respiration: 1×10⁻⁸ to 1×10⁻⁶ kg O₂/kg SOM/s"),
                p("• Q₁₀ values: 2.0-3.0")
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
  
  # Calculate diffusion coefficient
  calculate_diffusion <- reactive({
    # Temperature in Kelvin
    T_K <- input$temperature + 273.15
    
    # Free air diffusion coefficient (temperature corrected)
    # Base value at 20°C: 2.01e-5 m²/s
    D0_base <- 2.01e-5
    D0 <- D0_base * (T_K / 293.15)^1.75
    
    # Create depth vector in meters
    depths <- seq(0, input$max_depth/100, by = input$depth_resolution/100)
    
    # Calculate porosity at depth
    if (input$depth_varying_porosity) {
      porosity <- input$total_porosity * exp(-depths * input$porosity_decay)
    } else {
      porosity <- rep(input$total_porosity, length(depths))
    }
    
    # Air-filled porosity
    air_porosity <- pmax(0, porosity - input$water_content)
    
    # Tortuosity calculation
    if (input$tortuosity_model == "millington_quirk") {
      tortuosity <- (air_porosity^(10/3)) / (porosity^2)
    } else if (input$tortuosity_model == "moldrup") {
      tortuosity <- (air_porosity^2.5) / (porosity^1.5)
    } else {
      tortuosity <- rep(input$custom_tortuosity, length(depths))
    }
    
    # Effective diffusion coefficient
    D_eff <- D0 * (air_porosity / porosity) * tortuosity
    
    list(
      depths = depths,
      D0 = D0,
      porosity = porosity,
      air_porosity = air_porosity,
      tortuosity = tortuosity,
      D_eff = D_eff
    )
  })
  
  # Calculate respiration components
  calculate_respiration <- reactive({
    depths <- seq(0, input$max_depth/100, by = input$depth_resolution/100)
    
    # Temperature factor
    temp_factor_roots <- input$Q10_roots^((input$temperature - input$ref_temp_roots)/10)
    temp_factor_microbes <- input$Q10_microbes^((input$temperature - input$ref_temp_roots)/10)
    
    # Root respiration
    root_density <- input$root_density_surface * exp(-depths / input$root_decay_constant)
    root_respiration <- root_density * input$root_resp_rate * temp_factor_roots
    
    # Microbial respiration
    som_density <- (input$som_content/100) * 1300 * exp(-depths / input$som_decay_constant)  # Assuming bulk density ~1300 kg/m³
    
    # Moisture stress factor (Gaussian)
    moisture_stress <- exp(-((input$water_content - input$moisture_optimum) / (input$moisture_optimum * input$moisture_stress_factor))^2)
    
    microbial_respiration <- som_density * input$microbial_resp_rate * temp_factor_microbes * moisture_stress
    
    # Total respiration
    total_respiration <- root_respiration + microbial_respiration
    
    list(
      depths = depths,
      root_respiration = root_respiration,
      microbial_respiration = microbial_respiration,
      total_respiration = total_respiration,
      root_density = root_density,
      som_density = som_density
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
  
  # Main model calculation
  observeEvent(input$update_model, {
    
    # Get component calculations
    diffusion_data <- calculate_diffusion()
    respiration_data <- calculate_respiration()
    C_a <- calculate_surface_O2()
    
    depths <- diffusion_data$depths
    D_eff <- diffusion_data$D_eff
    total_respiration <- respiration_data$total_respiration
    
    # Fit exponential model to total respiration
    # q(z) = Q * exp(-z/Z_a)
    nls_result <- tryCatch({
      nls(total_respiration ~ Q * exp(-depths / Z_a), 
          start = list(Q = max(total_respiration), Z_a = 0.3),
          control = nls.control(maxiter = 100))
    }, error = function(e) NULL)
    
    if (!is.null(nls_result)) {
      Q_fitted <- coef(nls_result)["Q"]
      Z_a_fitted <- coef(nls_result)["Z_a"]
    } else {
      # Fallback: use simple estimates
      Q_fitted <- total_respiration[1]
      # Estimate Z_a from half-maximum depth
      half_max_idx <- which.min(abs(total_respiration - Q_fitted/2))
      Z_a_fitted <- depths[half_max_idx] / log(2)
    }
    
    # Use average diffusion coefficient for analytical solution
    D_avg <- mean(D_eff[is.finite(D_eff) & D_eff > 0])
    
    # Determine solution type
    ratio <- (Z_a_fitted^2 * Q_fitted) / (D_avg * C_a)
    
    if (ratio < 1) {
      # Type 1: Concentration approaches constant value
      C_star <- C_a - (Z_a_fitted^2 * Q_fitted) / D_avg
      concentration <- C_a - (Z_a_fitted^2 * Q_fitted / D_avg) * (1 - exp(-depths / Z_a_fitted))
      solution_type <- "Type 1: Asymptotic to constant value"
      critical_depth <- NA
    } else {
      # Type 2: Concentration goes to zero at some depth L
      L <- find_critical_depth(C_a, D_avg, Q_fitted, Z_a_fitted)
      
      if (is.finite(L) && L > 0) {
        concentration <- ifelse(depths <= L,
                              C_a - (Z_a_fitted^2 * Q_fitted / D_avg) * (1 - (depths/Z_a_fitted) * exp(-L/Z_a_fitted) - exp(-depths/Z_a_fitted)),
                              0)
        solution_type <- "Type 2: Goes to zero at depth"
        critical_depth <- L
        C_star <- 0
      } else {
        # Fallback to type 1
        C_star <- C_a - (Z_a_fitted^2 * Q_fitted) / D_avg
        concentration <- C_a - (Z_a_fitted^2 * Q_fitted / D_avg) * (1 - exp(-depths / Z_a_fitted))
        solution_type <- "Type 1: Asymptotic to constant value (fallback)"
        critical_depth <- NA
      }
    }
    
    # Calculate flux
    flux <- -D_avg * c(0, diff(concentration) / (input$depth_resolution/100))
    
    # Store results
    results <- data.frame(
      depth_m = depths,
      depth_cm = depths * 100,
      concentration = pmax(concentration, 0),
      total_respiration = total_respiration,
      root_respiration = respiration_data$root_respiration,
      microbial_respiration = respiration_data$microbial_respiration,
      D_eff = D_eff,
      flux = flux
    )
    
    # Add metadata
    attr(results, "solution_type") <- solution_type
    attr(results, "C_star") <- C_star
    attr(results, "critical_depth") <- critical_depth
    attr(results, "ratio") <- ratio
    attr(results, "C_a") <- C_a
    attr(results, "Q_fitted") <- Q_fitted
    attr(results, "Z_a_fitted") <- Z_a_fitted
    attr(results, "D_avg") <- D_avg
    
    model_results(results)
  })
  
  # Initialize model on startup
  observe({
    if (is.null(model_results())) {
      # Auto-trigger model calculation
      diffusion_data <- calculate_diffusion()
      respiration_data <- calculate_respiration()
      C_a <- calculate_surface_O2()
      
      depths <- diffusion_data$depths
      D_eff <- diffusion_data$D_eff
      total_respiration <- respiration_data$total_respiration
      
      # Simple exponential fit
      Q_fitted <- total_respiration[1]
      Z_a_fitted <- 0.3
      D_avg <- mean(D_eff[is.finite(D_eff) & D_eff > 0])
      
      # Simple concentration calculation
      ratio <- (Z_a_fitted^2 * Q_fitted) / (D_avg * C_a)
      
      if (ratio < 1) {
        C_star <- C_a - (Z_a_fitted^2 * Q_fitted) / D_avg
        concentration <- C_a - (Z_a_fitted^2 * Q_fitted / D_avg) * (1 - exp(-depths / Z_a_fitted))
        solution_type <- "Type 1: Asymptotic to constant value"
        critical_depth <- NA
      } else {
        concentration <- pmax(0, C_a - (Z_a_fitted^2 * Q_fitted / D_avg) * (1 - exp(-depths / Z_a_fitted)))
        solution_type <- "Type 2: Goes to zero at depth"
        critical_depth <- NA
        C_star <- 0
      }
      
      flux <- -D_avg * c(0, diff(concentration) / (input$depth_resolution/100))
      
      results <- data.frame(
        depth_m = depths,
        depth_cm = depths * 100,
        concentration = concentration,
        total_respiration = total_respiration,
        root_respiration = respiration_data$root_respiration,
        microbial_respiration = respiration_data$microbial_respiration,
        D_eff = D_eff,
        flux = flux
      )
      
      attr(results, "solution_type") <- solution_type
      attr(results, "C_star") <- C_star
      attr(results, "critical_depth") <- critical_depth
      attr(results, "ratio") <- ratio
      attr(results, "C_a") <- C_a
      attr(results, "Q_fitted") <- Q_fitted
      attr(results, "Z_a_fitted") <- Z_a_fitted
      attr(results, "D_avg") <- D_avg
      
      model_results(results)
    }
  })
  
  # Newton-Raphson method to find critical depth L
  find_critical_depth <- function(C_a, D, Q, Z_a, max_iter = 50, tol = 1e-6) {
    L <- Z_a
    
    for (i in 1:max_iter) {
      f_L <- L + Z_a - exp(-L/Z_a) * Z_a - (C_a * D) / (Z_a * Q)
      f_prime_L <- 1 - exp(-L/Z_a)
      
      if (abs(f_prime_L) < tol) break
      
      L_new <- L - f_L / f_prime_L
      
      if (abs(L_new - L) < tol) break
      
      L <- L_new
      if (L < 0) L <- Z_a
    }
    
    return(L)
  }
  
  # Main oxygen profile plot
  output$oxygen_profile <- renderPlotly({
    req(model_results())
    
    results <- model_results()
    
    p <- plot_ly(results, x = ~concentration, y = ~-depth_cm, type = 'scatter', mode = 'lines',
                name = 'Oxygen Concentration', line = list(width = 3, color = 'blue')) %>%
      layout(
        title = "Soil Oxygen Concentration Profile",
        xaxis = list(title = "Oxygen Concentration (kg/m³)"),
        yaxis = list(title = "Depth (cm)"),
        hovermode = 'closest'
      )
    
    # Add critical depth line if applicable
    critical_depth <- attr(results, "critical_depth")
    if (!is.na(critical_depth)) {
      p <- p %>% add_segments(x = 0, xend = max(results$concentration), 
                             y = -critical_depth*100, yend = -critical_depth*100,
                             line = list(dash = "dash", color = "red"),
                             name = "Critical Depth", showlegend = TRUE)
    }
    
    p
  })
  
  # Diffusion profile plot
  output$diffusion_profile <- renderPlotly({
    diffusion_data <- calculate_diffusion()
    
    df <- data.frame(
      depth_cm = diffusion_data$depths * 100,
      air_porosity = diffusion_data$air_porosity,
      tortuosity = diffusion_data$tortuosity,
      D_eff = diffusion_data$D_eff * 1e6  # Convert to mm²/s for readability
    )
    
    p <- plot_ly(df, y = ~-depth_cm) %>%
      add_trace(x = ~D_eff, name = 'Effective Diffusivity (mm²/s)', 
               type = 'scatter', mode = 'lines', line = list(color = 'red')) %>%
      layout(
        title = "Diffusion Coefficient vs Depth",
        xaxis = list(title = "Effective Diffusivity (mm²/s)"),
        yaxis = list(title = "Depth (cm)")
      )
    
    p
  })
  
  # Respiration components plot
  output$respiration_components <- renderPlotly({
    respiration_data <- calculate_respiration()
    
    df <- data.frame(
      depth_cm = respiration_data$depths * 100,
      root = respiration_data$root_respiration * 1e9,  # Convert to ng/m³/s
      microbial = respiration_data$microbial_respiration * 1e9,
      total = respiration_data$total_respiration * 1e9
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
    
    p
  })
  
  # Calculated parameters output
  output$calculated_params <- renderText({
    diffusion_data <- calculate_diffusion()
    respiration_data <- calculate_respiration()
    C_a <- calculate_surface_O2()
    
    paste(
      paste("Surface O₂ Concentration:", round(C_a, 4), "kg/m³"),
      paste("Free Air Diffusivity:", round(diffusion_data$D0 * 1e6, 2), "mm²/s"),
      paste("Average Air Porosity:", round(mean(diffusion_data$air_porosity), 3)),
      paste("Average Tortuosity:", round(mean(diffusion_data$tortuosity), 3)),
      paste("Average Effective Diffusivity:", round(mean(diffusion_data$D_eff) * 1e6, 2), "mm²/s"),
      paste("Surface Root Respiration:", round(respiration_data$root_respiration[1] * 1e9, 2), "ng/m³/s"),
      paste("Surface Microbial Respiration:", round(respiration_data$microbial_respiration[1] * 1e9, 2), "ng/m³/s"),
      paste("Surface Total Respiration:", round(respiration_data$total_respiration[1] * 1e9, 2), "ng/m³/s"),
      sep = "\n"
    )
  })
  
  # Diffusion components text
  output$diffusion_components <- renderText({
    diffusion_data <- calculate_diffusion()
    
    paste(
      paste("D₀ (free air):", round(diffusion_data$D0 * 1e6, 2), "mm²/s"),
      paste("Air porosity range:", round(min(diffusion_data$air_porosity), 3), "-", round(max(diffusion_data$air_porosity), 3)),
      paste("Tortuosity range:", round(min(diffusion_data$tortuosity), 3), "-", round(max(diffusion_data$tortuosity), 3)),
      paste("D_eff range:", round(min(diffusion_data$D_eff) * 1e6, 3), "-", round(max(diffusion_data$D_eff) * 1e6, 3), "mm²/s"),
      sep = "\n"
    )
  })
  
  # Model information
  output$model_info <- renderText({
    req(model_results())
    
    results <- model_results()
    solution_type <- attr(results, "solution_type")
    C_star <- attr(results, "C_star")
    critical_depth <- attr(results, "critical_depth")
    ratio <- attr(results, "ratio")
    Q_fitted <- attr(results, "Q_fitted")
    Z_a_fitted <- attr(results, "Z_a_fitted")
    
    info <- paste(
      paste("Solution Type:", solution_type),
      paste("Z_a²Q/DC_a Ratio:", round(ratio, 4)),
      paste("Fitted Q:", round(Q_fitted * 1e9, 2), "ng/m³/s"),
      paste("Fitted Z_a:", round(Z_a_fitted, 3), "m"),
      if (!is.na(C_star)) paste("Asymptotic Concentration (C*):", round(C_star, 6), "kg/m³") else "",
      if (!is.na(critical_depth)) paste("Critical Depth (L):", round(critical_depth*100, 1), "cm") else "",
      paste("Surface Flux:", round(results$flux[1] * 1e9, 2), "ng/m²/s"),
      sep = "\n"
    )
    
    return(info)
  })
}

# Run the application
shinyApp(ui = ui, server = server)