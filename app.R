library(shiny)

# ---- helper functions ----

# Simulate S(t) once
simulate_S_once <- function(t, lambda, beta) {
  N <- rpois(1, lambda * t)
  if (N > 0) {
    sum(rexp(N, rate = beta))
  } else {
    0
  }
}

# Simulate one sample path S(t) on [0, T_max]
simulate_path <- function(T_max, lambda, beta) {
  times  <- c(0)
  values <- c(0)
  t_curr <- 0
  S_curr <- 0
  
  repeat {
    t_curr <- t_curr + rexp(1, rate = lambda)  # next arrival
    if (t_curr > T_max) break
    times  <- c(times, t_curr)
    S_curr <- S_curr + rexp(1, rate = beta)    # jump size
    values <- c(values, S_curr)
  }
  
  times  <- c(times, T_max)
  values <- c(values, S_curr)
  
  data.frame(time = times, S = values)
}

ui <- fluidPage(
  titlePanel("Compound Poisson with Exponential Jumps: S(t)"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("lambda", "Arrival rate λ:",
                  min = 0.01, max = 5, value = 0.5, step = 0.01),
      sliderInput("beta", "Jump rate β (X ~ Exp(β)):",
                  min = 0.1, max = 5, value = 1, step = 0.1),
      numericInput("Tmax", "Time horizon for sample path (T_max):",
                   value = 100, min = 1),
      numericInput("t0", "Time t₀ for distribution of S(t₀):",
                   value = 10, min = 0),
      numericInput("nsim", "Number of simulations for histogram:",
                   value = 2000, min = 100),
      actionButton("run", "Run simulation")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Sample path",
                 plotOutput("pathPlot")),
        tabPanel("Distribution at t₀",
                 plotOutput("histPlot"),
                 verbatimTextOutput("statsOutput"))
      )
    )
  )
)

server <- function(input, output, session) {
  sim_results <- eventReactive(input$run, {
    lambda <- input$lambda
    beta   <- input$beta
    Tmax   <- input$Tmax
    t0     <- input$t0
    nsim   <- input$nsim
    
    path_df <- simulate_path(Tmax, lambda, beta)
    S_t0 <- replicate(nsim, simulate_S_once(t0, lambda, beta))
    
    list(path_df = path_df, S_t0 = S_t0)
  })
  
  output$pathPlot <- renderPlot({
    res <- sim_results()
    path_df <- res$path_df
    plot(path_df$time, path_df$S, type = "s",
         xlab = "Time t", ylab = "S(t)",
         main = "Sample path of S(t)")
  })
  
  output$histPlot <- renderPlot({
    res <- sim_results()
    S_t0 <- res$S_t0
    
    hist(S_t0, breaks = 50, freq = FALSE,
         main = paste0("Histogram of S(t₀) at t₀ = ", input$t0),
         xlab = "S(t₀)")
    
    m_theory <- input$lambda * input$t0 / input$beta
    abline(v = m_theory, lwd = 2)
  })
  
  output$statsOutput <- renderPrint({
    res <- sim_results()
    S_t0 <- res$S_t0
    lambda <- input$lambda
    beta   <- input$beta
    t0     <- input$t0
    
    m_emp <- mean(S_t0)
    v_emp <- var(S_t0)
    m_the <- lambda * t0 / beta
    v_the <- 2 * lambda * t0 / (beta^2)
    
    cat("Empirical mean of S(t₀):", m_emp, "\n")
    cat("Theoretical mean of S(t₀):", m_the, "\n\n")
    cat("Empirical variance of S(t₀):", v_emp, "\n")
    cat("Theoretical variance of S(t₀):", v_the, "\n")
  })
}

shinyApp(ui = ui, server = server)

