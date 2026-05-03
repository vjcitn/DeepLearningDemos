# Shiny app: Interactive 3x3 (Linear -> ReLU -> Scaled ReLU) plot
#
# Sources plot_3x3_relu_scaled.R from the repo root (two levels up).
# Run from the repo root with:
#   shiny::runApp("shiny/plot_relu_scaled")

source(file.path("..", "..", "plot_3x3_relu_scaled.R"))

library(shiny)

# Default parameter values (user-specified; a1[2] and b[2..3] are exact values from the problem)
default_a0 <- c(-0.1,   -0.2,    1.1)
default_a1 <- c( 0.3,    0.3354, -0.7)
default_b  <- c(-1.0,    1.4912,  1.4437)

ui <- fluidPage(
  titlePanel("Linear \u2192 ReLU \u2192 Scaled ReLU: Interactive 3\u00d73 Plot"),
  sidebarLayout(
    sidebarPanel(
      width = 3,

      h4("Intercepts (a0)"),
      sliderInput("a0_1", "a0[1]", min = -2, max = 2, value = default_a0[1], step = 0.01),
      sliderInput("a0_2", "a0[2]", min = -2, max = 2, value = default_a0[2], step = 0.01),
      sliderInput("a0_3", "a0[3]", min = -2, max = 2, value = default_a0[3], step = 0.01),

      h4("Slopes (a1)"),
      sliderInput("a1_1", "a1[1]", min = -2, max = 2, value = default_a1[1], step = 0.01),
      sliderInput("a1_2", "a1[2]", min = -2, max = 2, value = default_a1[2], step = 0.01),
      sliderInput("a1_3", "a1[3]", min = -2, max = 2, value = default_a1[3], step = 0.01),

      h4("Scale factors (b)"),
      sliderInput("b_1", "b[1]", min = -3, max = 3, value = default_b[1], step = 0.01),
      sliderInput("b_2", "b[2]", min = -3, max = 3, value = default_b[2], step = 0.01),
      sliderInput("b_3", "b[3]", min = -3, max = 3, value = default_b[3], step = 0.01),

      h4("Summary intercept (b0)"),
      sliderInput("b0", "b0", min = -3, max = 3, value = 0.5, step = 0.01),

      hr(),
      h4("Plot settings"),
      sliderInput("xlim_range", "x range",
                  min = -1, max = 5, value = c(0, 2), step = 0.1),
      numericInput("n_pts", "Number of points (n)",
                   value = 401, min = 50, max = 2000, step = 10),
      checkboxInput("same_ylim", "Same y-limits per row", value = TRUE)
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("main",
          plotOutput("relu_plot", height = "700px"),
          plotOutput("summary_plot", height = "250px")
        ),
        tabPanel("About",
          br(),
          p("This visualization tool was modeled after ",
            strong("Figure 3.3"),
            " of:"),
          p(em("Simon J. D. Prince,"),
            strong("\u201cDeep Learning\u201d"),
            ", MIT Press, 2023."),
          p("The three columns correspond to three linear functions",
            "y\u1d62 = a\u2080\u1d62 + a\u2081\u1d62 x. Each row applies a successive",
            "transformation:"),
          tags$ul(
            tags$li(strong("Row 1 \u2014 Linear:"),
                    " the raw linear output y\u1d62(x)."),
            tags$li(strong("Row 2 \u2014 ReLU:"),
                    " ReLU(y\u1d62) = max(0, y\u1d62)."),
            tags$li(strong("Row 3 \u2014 Scaled ReLU:"),
                    " b\u1d62 \u00d7 ReLU(y\u1d62), where b\u1d62 is the scale factor.")
          ),
          p("Use the sliders on the left to adjust the intercepts (a0),",
            "slopes (a1), and scale factors (b) for each of the three",
            "functions. Scale factors may be negative."),
          p("The", strong("b0"), "slider sets the overall intercept for the",
            "summary plot at the bottom, which shows",
            "y = b0 + \u03a3 b\u1d62 \u00d7 ReLU(a0\u1d62 + a1\u1d62 x).")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  output$relu_plot <- renderPlot({
    a0 <- c(input$a0_1, input$a0_2, input$a0_3)
    a1 <- c(input$a1_1, input$a1_2, input$a1_3)
    b  <- c(input$b_1,  input$b_2,  input$b_3)

    n_val <- as.integer(input$n_pts)
    if (is.na(n_val) || n_val < 2L) {
      validate(need(FALSE, "Please enter a number of points \u2265 2."))
    }

    plot_3x3_relu_scaled(
      a0 = a0, a1 = a1, b = b,
      xlim = input$xlim_range,
      n    = n_val,
      same_ylim_by_row = input$same_ylim
    )
  })

  output$summary_plot <- renderPlot({
    a0 <- c(input$a0_1, input$a0_2, input$a0_3)
    a1 <- c(input$a1_1, input$a1_2, input$a1_3)
    b  <- c(input$b_1,  input$b_2,  input$b_3)

    n_val <- as.integer(input$n_pts)
    if (is.na(n_val) || n_val < 2L) {
      validate(need(FALSE, "Please enter a number of points \u2265 2."))
    }

    xlim <- input$xlim_range
    x <- seq(xlim[1], xlim[2], length.out = n_val)
    relu <- function(y) pmax(0, y)

    y_total <- input$b0 +
      b[1] * relu(a0[1] + a1[1] * x) +
      b[2] * relu(a0[2] + a1[2] * x) +
      b[3] * relu(a0[3] + a1[3] * x)

    plot(x, y_total, type = "l", col = "darkorange", lwd = 2,
         xlim = xlim,
         xlab = "x", ylab = "y_total",
         main = sprintf("Summary: b0=%.2f + \u03a3 b\u1d62\u00d7ReLU(a0\u1d62 + a1\u1d62 x)", input$b0))
    abline(h = 0, col = "gray80", lty = 2)
  })
}

shinyApp(ui, server)
