library(shiny)
library(ggplot2)
library(dplyr)

# Define UI
ui <- fluidPage(
  titlePanel("Steady-State Oxygen Concentration in Multi-layered Soil Profile"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Surface Conditions"),
      numericInput("C0", "Surface O₂ concentration (g/m³):", value = 0.3, min = 0, max = 1, step = 0.01),
      
      hr(),
      h4("Layer Properties"),
      p("Define up to 4 layers. Set thickness to 0 to ignore a layer."),
      
      # Layer 1
      h5("Layer 1"),
      fluidRow(
        column(4, numericInput("B1", "Thickness (cm):", value = 25, min = 0, step = 5)),
        column(4, numericInput("q1", "q₁ (mg/m³s):", value = 0.5, min = 0, step = 0.1)),
        column(4, numericInput("D1", "D₁ (m²/s × 10⁻⁷):", value = 2.1, min = 0, step = 0.1))
      ),
      
      # Layer 2
      h5("Layer 2"),
      fluidRow(
        column(4, numericInput("B2", "Thickness (cm):", value = 25, min = 0, step = 5)),
        column(4, numericInput("q2", "q₂ (mg/m³s):", value = 1.2, min = 0, step = 0.1)),
        column(4, numericInput("D2", "D₂ (m²/s × 10⁻⁷):", value = 2.1, min = 0, step = 0.1))
      ),
      
      # Layer 3
      h5("Layer 3"),
      fluidRow(
        column(4, numericInput("B3", "Thickness (cm):", value = 25, min = 0, step = 5)),
        column(4, numericInput("q3", "q₃ (mg/m³s):", value = 0.3, min = 0, step = 0.1)),
        column(4, numericInput("D3", "D₃ (m²/s × 10⁻⁷):", value = 2.1, min = 0, step = 0.1))
      ),
      
      # Layer 4
      h5("Layer 4"),
      fluidRow(
        column(4, numericInput("B4", "Thickness (cm):", value = 25, min = 0, step = 5)),
        column(4, numericInput("q4", "q₄ (mg/m³s):", value = 0.1, min = 0, step = 0.1)),
        column(4, numericInput("D4", "D₄ (m²/s × 10⁻⁷):", value = 10, min = 0, step = 0.1))
      ),
      
      hr(),
      actionButton("preset", "Load Figure 11 Preset"),
      br(),
      br(),
      helpText("Note: The diffusion coefficients are entered as values × 10⁻⁷ m²/s")
    ),
    
    mainPanel(
      plotOutput("oxygenPlot", height = "600px"),
      hr(),
      h4("Profile Summary"),
      tableOutput("summaryTable")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Load preset values from Figure 11
  observeEvent(input$preset, {
    # Layer 1: B<25cm, q1 = 0.5 mg/m³s, D1 = 2.1×10⁻⁷ m²/s
    updateNumericInput(session, "q3", value = 0.3)
    updateNumericInput(session, "D3", value = 2.1)
    
    # Layer 4: 75<x<100cm, q4 = 0.1 mg/m³s, D4 = 10×10⁻⁷ m²/s
    updateNumericInput(session, "B4", value = 25)
    updateNumericInput(session, "q4", value = 0.1)
    updateNumericInput(session, "D4", value = 10)
    
    # Surface concentration
    updateNumericInput(session, "C0", value = 0.3)
  })
  
  # Calculate oxygen concentration profile
  calculateProfile <- reactive({
    # Get layer parameters
    layers <- data.frame(
      layer = 1:4,
      B = c(input$B1, input$B2, input$B3, input$B4),
      q = c(input$q1, input$q2, input$q3, input$q4),
      D = c(input$D1, input$D2, input$D3, input$D4) * 1e-7  # Convert to m²/s
    ) %>%
      filter(B > 0)  # Only include layers with positive thickness
    
    if(nrow(layers) == 0) return(NULL)
    
    # Convert units
    layers$B <- layers$B / 100  # cm to m
    layers$q <- layers$q / 1000  # mg to g
    
    # Calculate layer boundaries
    layers$H_lower <- cumsum(layers$B)
    layers$H_upper <- c(0, layers$H_lower[-nrow(layers)])
    
    # Create fine grid for plotting
    n_points <- 100
    results <- data.frame()
    
    for(i in 1:nrow(layers)) {
      # Grid points within this layer
      x_layer <- seq(layers$H_upper[i], layers$H_lower[i], length.out = n_points)
      
      # Calculate concentration at layer top
      if(i == 1) {
        C_top <- input$C0
      } else {
        # Use equation (56) to calculate C at top of current layer
        prev_layers <- layers[1:(i-1), ]
        
        # Sum of respiratory activities
        sum_qa <- sum(prev_layers$q * prev_layers$B)
        
        # Calculate based on previous layer
        x_prev <- layers$H_upper[i]
        C_top <- input$C0 - (2 * sum_qa * x_prev - prev_layers$q[i-1] * (x_prev - prev_layers$H_upper[i-1])^2) / (2 * prev_layers$D[i-1])
      }
      
      # Calculate concentration within layer using equation (55)
      sum_qa_prev <- if(i == 1) 0 else sum(layers$q[1:(i-1)] * layers$B[1:(i-1)])
      
      C_layer <- C_top - (2 * sum_qa_prev * (x_layer - layers$H_upper[i]) - 
                          layers$q[i] * (x_layer - layers$H_upper[i])^2) / (2 * layers$D[i])
      
      # Store results
      layer_results <- data.frame(
        x = x_layer * 100,  # Convert back to cm for plotting
        C = C_layer,
        layer = paste("Layer", i)
      )
      
      results <- rbind(results, layer_results)
    }
    
    # Add surface point
    surface <- data.frame(x = 0, C = input$C0, layer = "Surface")
    results <- rbind(surface, results)
    
    return(results)
  })
  
  # Create plot
  output$oxygenPlot <- renderPlot({
    profile <- calculateProfile()
    if(is.null(profile)) return(NULL)
    
    # Create the plot
    p <- ggplot(profile, aes(x = C, y = x)) +
      geom_line(size = 1.5, color = "darkblue") +
      geom_point(data = profile[profile$layer == "Surface", ], 
                 color = "red", size = 3) +
      scale_y_reverse(limits = c(max(profile$x), 0)) +
      scale_x_continuous(
        limits = c(0, max(0.35, max(profile$C) * 1.1)),
        sec.axis = sec_axis(~ . * 1000, name = "O₂ concentration in soil air (mg/m³)")
      ) +
      labs(
        x = "O₂ concentration in soil air (g/m³)",
        y = "Depth (cm)",
        title = "Steady-State Oxygen Concentration Profile"
      ) +
      theme_minimal() +
      theme(
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_line(color = "gray90"),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold")
      )
    
    # Add layer boundaries
    layers <- data.frame(
      layer = 1:4,
      B = c(input$B1, input$B2, input$B3, input$B4)
    ) %>%
      filter(B > 0)
    
    if(nrow(layers) > 1) {
      boundaries <- cumsum(layers$B[-nrow(layers)])
      p <- p + geom_hline(yintercept = boundaries, 
                          linetype = "dashed", 
                          color = "gray40", 
                          alpha = 0.5)
    }
    
    # Add layer labels
    layer_mids <- c()
    layer_labels <- c()
    cum_depth <- 0
    
    for(i in 1:nrow(layers)) {
      layer_mids <- c(layer_mids, cum_depth + layers$B[i]/2)
      layer_labels <- c(layer_labels, paste0("Layer ", i))
      cum_depth <- cum_depth + layers$B[i]
    }
    
    # Add text annotations for layers
    for(i in 1:length(layer_mids)) {
      p <- p + annotate("text", 
                        x = 0.02, 
                        y = layer_mids[i], 
                        label = layer_labels[i],
                        hjust = 0,
                        size = 3.5,
                        color = "gray30")
    }
    
    print(p)
  })
  
  # Create summary table
  output$summaryTable <- renderTable({
    layers <- data.frame(
      Layer = paste("Layer", 1:4),
      "Thickness (cm)" = c(input$B1, input$B2, input$B3, input$B4),
      "q (mg/m³s)" = c(input$q1, input$q2, input$q3, input$q4),
      "D (×10⁻⁷ m²/s)" = c(input$D1, input$D2, input$D3, input$D4)
    ) %>%
      filter(`Thickness (cm)` > 0)
    
    layers
  }, digits = 2)
}

# Run the application (comment out if sourcing)
# shinyApp(ui = ui, server = server)(session, "B1", value = 25)
    updateNumericInput(session, "q1", value = 0.5)
    updateNumericInput(session, "D1", value = 2.1)
    
    # Layer 2: 25<x<50cm, q2 = 1.2 mg/m³s, D2 = 2.1×10⁻⁷ m²/s
    updateNumericInput(session, "B2", value = 25)
    updateNumericInput(session, "q2", value = 1.2)
    updateNumericInput(session, "D2", value = 2.1)
    
    # Layer 3: 50<x<75cm, q3 = 0.3 mg/m³s, D3 = 2.1×10⁻⁷ m²/s
    updateNumericInput(session, "B3", value = 25)
    updateNumericInput