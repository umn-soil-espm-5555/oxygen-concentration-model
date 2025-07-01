library(shiny)
library(ggplot2)
library(dplyr)

# Define UI
ui <- fluidPage(
  titlePanel("Steady-State Oxygen Concentration in Soil Profile"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Soil Profile Parameters"),
      
      numericInput("n_layers", "Number of Layers:", 
                   value = 4, min = 1, max = 10, step = 1),
      
      h3("Boundary Conditions"),
      numericInput("C_surface", "Surface O₂ concentration (mg m⁻³):", 
                   value = 300, min = 0, step = 10),
      
      # Create layer inputs directly in UI (not dynamic)
      conditionalPanel(
        condition = "input.n_layers >= 1",
        h4("Layer 1 (Surface)"),
        numericInput("a1", "Layer thickness (cm):", value = 25, min = 1),
        numericInput("q1", "Respiration rate (mg m⁻³ s⁻¹):", value = 0.5, min = 0, step = 0.01),
        numericInput("D1", "Diffusion coefficient (m² s⁻¹):", value = 2.10e-7, min = 0, step = 1e-8)
      ),
      
      conditionalPanel(
        condition = "input.n_layers >= 2",
        h4("Layer 2"),
        numericInput("a2", "Layer thickness (cm):", value = 25, min = 1),
        numericInput("q2", "Respiration rate (mg m⁻³ s⁻¹):", value = 0.1, min = 0, step = 0.01),
        numericInput("D2", "Diffusion coefficient (m² s⁻¹):", value = 2.10e-7, min = 0, step = 1e-8)
      ),
      
      conditionalPanel(
        condition = "input.n_layers >= 3",
        h4("Layer 3"),
        numericInput("a3", "Layer thickness (cm):", value = 25, min = 1),
        numericInput("q3", "Respiration rate (mg m⁻³ s⁻¹):", value = 0.3, min = 0, step = 0.01),
        numericInput("D3", "Diffusion coefficient (m² s⁻¹):", value = 2.10e-7, min = 0, step = 1e-8)
      ),
      
      conditionalPanel(
        condition = "input.n_layers >= 4",
        h4("Layer 4"),
        numericInput("a4", "Layer thickness (cm):", value = 25, min = 1),
        numericInput("q4", "Respiration rate (mg m⁻³ s⁻¹):", value = 10, min = 0, step = 0.1),
        numericInput("D4", "Diffusion coefficient (m² s⁻¹):", value = 2.10e-7, min = 0, step = 1e-8)
      ),
      
      conditionalPanel(
        condition = "input.n_layers >= 5",
        h4("Layer 5"),
        numericInput("a5", "Layer thickness (cm):", value = 25, min = 1),
        numericInput("q5", "Respiration rate (mg m⁻³ s⁻¹):", value = 0.001, min = 0, step = 0.001),
        numericInput("D5", "Diffusion coefficient (m² s⁻¹):", value = 2.10e-7, min = 0, step = 1e-8)
      ),
      
      h3("Plot Settings"),
      numericInput("plot_resolution", "Plot resolution (points per cm):", 
                   value = 2, min = 0.1, max = 10, step = 0.1),
      
      # Add option to show multiple curves like in textbook
      checkboxInput("show_multiple", "Show multiple parameter sets", FALSE),
      
      conditionalPanel(
        condition = "input.show_multiple",
        actionButton("add_curve", "Add Current Settings as Curve"),
        actionButton("clear_curves", "Clear All Curves")
      )
    ),
    
    mainPanel(
      plotOutput("oxygen_profile", height = "600px"),
      
      h3("Model Information"),
      p("This model calculates steady-state oxygen concentrations in a layered soil profile using the diffusion-respiration equation:"),
      p("Within each layer: d²C/dx² = q/D"),
      p("Solution: C(x) = C₀ - [q × (x - H_{k-1})²] / (2D)"),
      p("Where:"),
      tags$ul(
        tags$li("C(x) = oxygen concentration at depth x"),
        tags$li("q = respiration rate in layer"),
        tags$li("D = diffusion coefficient in layer"),
        tags$li("x = depth from surface"),
        tags$li("H_{k-1} = upper boundary of layer k")
      ),
      p("Based on equations (55), (56), and (57) from the textbook."),
      
      tableOutput("layer_summary")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive values to store multiple curves
  curves_data <- reactiveValues(
    curves = list(),
    curve_count = 0
  )
  
  # Calculate oxygen profile with proper equations
  calculate_profile <- reactive({
    # Get layer parameters
    n_layers <- input$n_layers
    
    # Initialize vectors
    layer_thickness <- numeric(n_layers)  # layer thickness in meters
    q <- numeric(n_layers)  # respiration rate
    D <- numeric(n_layers)  # diffusion coefficient
    
    # Fill vectors based on number of layers
    for(i in 1:n_layers) {
      # Use default values if inputs don't exist yet
      layer_thickness[i] <- ifelse(!is.null(input[[paste0("a", i)]]), 
                                   input[[paste0("a", i)]] / 100, 
                                   0.25)  # convert cm to m
      q[i] <- ifelse(!is.null(input[[paste0("q", i)]]), 
                     input[[paste0("q", i)]], 
                     0.001)
      D[i] <- ifelse(!is.null(input[[paste0("D", i)]]), 
                     input[[paste0("D", i)]], 
                     2.10e-7)
    }
    
    # Calculate cumulative depths (H values in textbook notation)
    H <- c(0, cumsum(layer_thickness))
    total_depth <- sum(layer_thickness)
    
    # Create depth vector for plotting
    depth_points <- seq(0, total_depth, by = 0.01/input$plot_resolution)
    
    # Initialize concentration vector
    C <- numeric(length(depth_points))
    C_surface <- input$C_surface
    
    # Calculate concentration at each depth point
    for(j in 1:length(depth_points)) {
      x <- depth_points[j]  # depth from surface in meters
      
      if(x == 0) {
        C[j] <- C_surface  # At surface
      } else {
        # Find which layer this depth belongs to
        layer_idx <- which(x > H[-length(H)] & x <= H[-1])[1]
        if(length(layer_idx) == 0 || is.na(layer_idx)) layer_idx <- n_layers
        
        # Calculate concentration using equation (55) from textbook
        # First, calculate concentration at upper boundary of current layer
        C_upper <- C_surface
        
        # Apply drops through all previous layers (equation 56)
        if(layer_idx > 1) {
          for(k in 1:(layer_idx-1)) {
            drop <- (q[k] * layer_thickness[k]^2) / (2 * D[k])
            C_upper <- C_upper - drop
          }
        }
        
        # Now apply the parabolic profile within current layer
        x_in_layer <- x - H[layer_idx]  # distance from upper boundary of current layer
        drop_in_layer <- (q[layer_idx] * x_in_layer^2) / (2 * D[layer_idx])
        
        C[j] <- C_upper - drop_in_layer
        
        # Ensure no negative concentrations
        if(C[j] < 0) C[j] <- 0
      }
    }
    
    # Create data frame
    data.frame(
      depth_cm = depth_points * 100,  # convert back to cm for display
      concentration = C
    )
  })
  
  # Add current curve to stored curves
  observeEvent(input$add_curve, {
    curves_data$curve_count <- curves_data$curve_count + 1
    curve_name <- paste("Curve", curves_data$curve_count)
    
    # Store current profile
    current_profile <- calculate_profile()
    current_profile$curve <- curve_name
    
    # Store parameters for legend
    n_layers <- input$n_layers
    params <- paste0("q₁=", input$q1, ", D₁=", formatC(input$D1, format = "e", digits = 2))
    current_profile$params <- params
    
    curves_data$curves[[curve_name]] <- current_profile
  })
  
  # Clear all curves
  observeEvent(input$clear_curves, {
    curves_data$curves <- list()
    curves_data$curve_count <- 0
  })
  
  # Create summary table
  output$layer_summary <- renderTable({
    n_layers <- input$n_layers
    
    # Initialize data frame
    layer_data <- data.frame(
      Layer = integer(),
      `Thickness (cm)` = numeric(),
      `Respiration Rate (mg m⁻³ s⁻¹)` = numeric(),
      `Diffusion Coefficient (m² s⁻¹)` = character(),
      `Depth Range (cm)` = character(),
      stringsAsFactors = FALSE
    )
    
    # Only proceed if we have inputs
    if(!is.null(input$a1)) {
      # Get layer parameters
      thickness_vals <- numeric(n_layers)
      q_vals <- numeric(n_layers)
      D_vals <- character(n_layers)
      
      for(i in 1:n_layers) {
        thickness_vals[i] <- ifelse(!is.null(input[[paste0("a", i)]]), input[[paste0("a", i)]], 25)
        q_vals[i] <- ifelse(!is.null(input[[paste0("q", i)]]), input[[paste0("q", i)]], 0.001)
        D_vals[i] <- ifelse(!is.null(input[[paste0("D", i)]]), 
                            formatC(input[[paste0("D", i)]], format = "e", digits = 2),
                            "2.10e-07")
      }
      
      # Calculate depth ranges
      depth_ranges <- character(n_layers)
      for(i in 1:n_layers) {
        start <- ifelse(i == 1, 0, sum(thickness_vals[1:(i-1)]))
        end <- sum(thickness_vals[1:i])
        depth_ranges[i] <- paste0(start, " - ", end)
      }
      
      layer_data <- data.frame(
        Layer = 1:n_layers,
        `Thickness (cm)` = thickness_vals,
        `Respiration Rate (mg m⁻³ s⁻¹)` = q_vals,
        `Diffusion Coefficient (m² s⁻¹)` = D_vals,
        `Depth Range (cm)` = depth_ranges,
        stringsAsFactors = FALSE
      )
    }
    
    layer_data
  }, digits = 4)
  
  # Create plot
  output$oxygen_profile <- renderPlot({
    profile_data <- NULL
    
    tryCatch({
      profile_data <- calculate_profile()
    }, error = function(e) {
      # Return empty plot if error
      return(NULL)
    })
    
    if(is.null(profile_data)) return(NULL)
    
    if(input$show_multiple && length(curves_data$curves) > 0) {
      # Plot multiple curves
      all_data <- do.call(rbind, curves_data$curves)
      
      p <- ggplot(all_data, aes(x = concentration, y = depth_cm, color = curve)) +
        geom_line(size = 1.2) +
        scale_y_reverse()
    } else {
      # Plot single curve
      p <- ggplot(profile_data, aes(x = concentration, y = depth_cm)) +
        geom_line(size = 1.2, color = "blue") +
        scale_y_reverse()
    }
    
    # Common plot settings
    p <- p +
      labs(
        x = expression(O[2] ~ "concentration in soil air (mg m"^{-3}*")"),
        y = "x, cm",
        title = "Steady-State Distribution of Oxygen Concentration in Soil Air"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray95"),
        legend.position = if(input$show_multiple && length(curves_data$curves) > 0) "right" else "none"
      ) +
      scale_x_continuous(
        breaks = seq(0, 300, by = 100),
        sec.axis = sec_axis(~./1000, name = expression("m"^{3}*"m"^{-3}), breaks = seq(0, 0.3, by = 0.05))
      )
    
    # Add layer boundaries only if we have valid inputs
    if(!is.null(input$a1)) {
      n_layers <- input$n_layers
      cumulative_depth <- 0
      
      for(i in 1:n_layers) {
        if(i < n_layers) {
          thickness <- ifelse(!is.null(input[[paste0("a", i)]]), input[[paste0("a", i)]], 25)
          cumulative_depth <- cumulative_depth + thickness
          p <- p + geom_hline(yintercept = cumulative_depth, 
                             linetype = "dashed", color = "gray60", alpha = 0.5)
          
          # Add layer labels on the right side
          max_conc <- max(profile_data$concentration, na.rm = TRUE)
          p <- p + annotate("text", 
                           x = ifelse(input$show_multiple && length(curves_data$curves) > 0, 
                                     290, max_conc * 0.95), 
                           y = cumulative_depth - 2, 
                           label = paste("H", i, sep = ""), 
                           size = 3, color = "gray40")
        }
      }
      
      # Add parameter annotations for layers (like in textbook)
      if(!input$show_multiple) {
        cumulative_depth <- 0
        for(i in 1:n_layers) {
          thickness <- ifelse(!is.null(input[[paste0("a", i)]]), input[[paste0("a", i)]], 25)
          layer_mid <- cumulative_depth + thickness / 2
          
          # Add layer size and parameters
          p <- p + annotate("text", 
                           x = 50,  # Position on left side
                           y = layer_mid, 
                           label = paste0(thickness, "cm; q", i, ", D", i), 
                           size = 3, color = "black", hjust = 0)
          
          cumulative_depth <- cumulative_depth + thickness
        }
      }
    }
    
    # Add surface line
    p <- p + geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1)
    
    print(p)
  })
}

# Run the application
shinyApp(ui = ui, server = server)