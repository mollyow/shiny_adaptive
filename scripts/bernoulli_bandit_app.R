# File: app.R

bandit_app <- function(height = 1600) {

  # Define UI for application that draws a histogram
  ui <- fillPage(

    # Application title
    titlePanel("Bernoulli Bandit Experiment"),

    # Sidebar with a slider input for number of bi
    sidebarLayout(
      sidebarPanel(
        sliderInput("EY1",
          "Prob[Y(1) = 1] equals...",
          min = 0,
          max = 1,
          value = 0.2
        ),
        sliderInput("EY2",
          "Prob[Y(2) = 1] equals...",
          min = 0,
          max = 1,
          value = 0.7
        ),
        sliderInput("EY3",
          "Prob[Y(3) = 1] equals...",
          min = 0,
          max = 1,
          value = 0.2
        ),
        sliderInput("EY4",
          "Prob[Y(4) = 1] equals...",
          min = 0,
          max = 1,
          value = 0.2
        ),
        sliderInput("EY5",
          "Prob[Y(5) = 1] equals...",
          min = 0,
          max = 1,
          value = 0.2
        ),
        sliderInput("EY6",
          "Prob[Y(6) = 1] equals...",
          min = 0,
          max = 1,
          value = 0.2
        ),
        sliderInput("EY7",
          "Prob[Y(7) = 1] equals...",
          min = 0,
          max = 1,
          value = 0.2
        ),
        sliderInput("EY8",
          "Prob[Y(8) = 1] equals...",
          min = 0,
          max = 1,
          value = 0.2
        ),
        sliderInput("T",
          "Experiment length...",
          min = 5,
          max = 5000,
          value = 100
        ),
        sliderInput("initial_batch_prop",
          "Initial phase length (% of total length)...",
          min = 0,
          max = 1,
          value = 0
        ),
        sliderInput("floor",
          "Assignment probability floor...",
          min = 0.01,
          max = 0.125, # TODO: 1/K
          value = 0.025
        ),
        sliderInput("num_batches",
          "Number of batches...",
          min = 1,
          max = 100,
          value = 10
        ),
        radioButtons("algorithm",
          "Algorithm...",
          choiceNames = list(
            "Thompson sampling",
            "Epsilon-greedy",
            "Random"
          ),
          choiceValues = list(
            "thompson_sampling",
            "epsilon_greedy",
            "random"
          ),
        ),
        radioButtons("bw",
          "Balancing weights...",
          choiceNames = list(
            "No balancing weights",
            "Balancing weights"
          ),
          choiceValues = list(
            "not_balanced",
            "balanced"
          ),
        ),
        radioButtons("control",
          "Control condition",
          choiceNames = list(
            "Fix proportion at 1/K",
            "Algorithmic default"
          ),
          choiceValues = list(
            "fixed",
            "standard"
          ),
        ),
        sliderInput("M",
          "Number of simulations...",
          min = 3,
          max = 1000,
          value = 20
        ),
        actionButton(
          "run",
          "Run Experiment"
        ),
        # downloadButton('download', 'Download results'),
      ),

      # Show a plot of the generated distribution
      mainPanel(plotOutput("distPlot"), height = "10px")
    )
  )

  # Define server logic required to draw a histogram
  server <- function(input, output, session) {

    # Runs experiment when user presses 'Run' button
    simulate <- eventReactive(input$run, {
      input$goPlot

      withProgress(message = "Running", value = 0, {
        if (input$bw == "balanced" & (input$algorithm %in% c("thompson_sampling", "epsilon_greedy"))) {
          algorithm <- paste0(input$algorithm, "_balanced")
        } else {
          algorithm <- input$algorithm
        }

        result <- lapply(seq(input$M), function(i) {

          # incProgress(1/input$M, detail = paste("Simulation ", i))

          if (input$control == "fixed") {
            run_bernoulli_control_experiment(
              means = c(input$EY1, input$EY2, input$EY3, input$EY4, input$EY5, input$EY6, input$EY7, input$EY8),
              algorithm = algorithm,
              T = input$T,
              floor = input$floor,
              initial_batch = as.integer(input$T * input$initial_batch_prop),
              num_batches = input$num_batches
            )
          } else {
            run_bernoulli_experiment(
              means = c(input$EY1, input$EY2, input$EY3, input$EY4, input$EY5, input$EY6, input$EY7, input$EY8),
              algorithm = input$algorithm,
              T = input$T,
              floor = input$floor,
              initial_batch = as.integer(input$T * input$initial_batch_prop),
              num_batches = input$num_batches
            )
          }
        })
      })

      result
    })

    # TODO: Allow for downloading results
    output$downloadData <- downloadHandler(
      filename = function() {},
      content = function(con) {}
    )

    # Plots results
    output$distPlot <- renderPlot(
      {

        # Runs simulations on click
        print("running")
        results <- simulate()
        p_assign <- plot_assignment_probabilities(results)

        # Makes plots
        p_infer <- plot_inference(results)
        d_infer <- plot_control_inference(
          results,
          # estimators = c('sm', 'ipw', 'haj')
        )
        p_regret <- plot_regret(results)
        grid.arrange(p_assign, p_regret, p_infer, d_infer,
          ncol = 1
        )
      },
      height = 1200
    )
  }


  # Run the application
  shinyApp(ui = ui, server = server, options = list(height = height))
}
