
regret_experiment <- function(num_sims = 10, floor = 0, initial_batch_prop = 0,
                              height = 300, T = 100, color = FALSE) {

  # Define UI for application that draws a histogram
  ui <- fillPage(

    # Sidebar with a slider input for number of bi
    sidebarLayout(
      sidebarPanel(
        sliderInput("EY1",
          "Prob[Y(1) = 1] equals...",
          min = 0,
          max = 1,
          value = 0.7
        ),
        sliderInput("EY2",
          "Prob[Y(2) = 1] equals...",
          min = 0,
          max = 1,
          value = 0.4
        ),
        # sliderInput("EY3",
        #             "Prob[Y(3) = 1] equals...",
        #             min = 0,
        #             max = 1,
        #             value = 0.2
        # ),
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
        actionButton(
          "run",
          "Run Experiment"
        ),
        # downloadButton('download', 'Download results'),
      ),
      # Show a plot of the generated distribution
      mainPanel(plotOutput("distPlot"))
    )
  )

  # Define server logic required to draw a histogram
  server <- function(input, output, session) {

    # Runs experiment when user presses 'Run' button
    simulate <- eventReactive(input$run, {
      input$goPlot

      withProgress(message = "Running", value = 0, {
        result <- lapply(seq(num_sims), function(i) {
          incProgress(1 / num_sims, detail = paste("Simulation ", i))

          run_bernoulli_experiment(
            means = c(input$EY1, input$EY2, input$EY3),
            algorithm = input$algorithm,
            T = T,
            floor = 0,
            initial_batch = initial_batch_prop * T,
            num_batches = T
          )
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
        p_assign <- plot_assignment_probabilities(results, color = color)
        p_regret <- plot_regret(results)
        grid.arrange(p_assign, p_regret,
          layout_matrix = rbind(c(1), c(2))
        )
      },
      height = function() {
        height
      }
    ) # TODO: Make this reactive?
  }


  # Run the application
  shinyApp(ui = ui, server = server, options = list(height = height))
}



regret_experiment_balancing <- function(num_sims = 10, initial_batch_prop = 0,
                                        height = 300, T = 100, color = FALSE) {

  # Define UI for application that draws a histogram
  ui <- fillPage(

    # Sidebar with a slider input for number of bi
    sidebarLayout(
      sidebarPanel(
        sliderInput("EY1",
          "Prob[Y(1) = 1] equals...",
          min = 0,
          max = 1,
          value = 0.5
        ),
        sliderInput("EY2",
          "Prob[Y(2) = 1] equals...",
          min = 0,
          max = 1,
          value = 0.5
        ),
        # sliderInput("EY3",
        #             "Prob[Y(3) = 1] equals...",
        #             min = 0,
        #             max = 1,
        #             value = 0.5
        # ),
        sliderInput("floor",
          "Probability floor",
          min = 0.01,
          max = 0.33,
          value = 0.05
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
        actionButton(
          "run",
          "Run Experiment"
        ),
        # downloadButton('download', 'Download results'),
      ),
      # Show a plot of the generated distribution
      mainPanel(plotOutput("distPlot"))
    )
  )

  # Define server logic required to draw a histogram
  server <- function(input, output, session) {

    # Runs experiment when user presses 'Run' button
    simulate <- eventReactive(input$run, {
      input$goPlot

      if (input$bw == "balanced" & (input$algorithm %in% c("thompson_sampling", "epsilon_greedy"))) {
        algorithm <- paste0(input$algorithm, "_balanced")
      } else {
        algorithm <- input$algorithm
      }

      withProgress(message = "Running", value = 0, {
        result <- lapply(seq(num_sims), function(i) {
          incProgress(1 / num_sims, detail = paste("Simulation ", i))

          run_bernoulli_experiment(
            means = c(input$EY1, input$EY2, input$EY3),
            algorithm = algorithm,
            T = T,
            floor = input$floor,
            initial_batch = initial_batch_prop * T,
            num_batches = T
          )
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
        p_assign <- plot_assignment_probabilities(results, color = color)
        p_regret <- plot_regret(results)
        grid.arrange(p_assign, p_regret,
          layout_matrix = rbind(c(1), c(2))
        )
      },
      height = function() {
        height
      }
    ) # TODO: Make this reactive?
  }


  # Run the application
  shinyApp(ui = ui, server = server, options = list(height = height))
}
