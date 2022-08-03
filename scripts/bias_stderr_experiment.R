bias_stderr_experiment <- function(T = 100,
                                   floor = 0,
                                   initial_batch_prop = 0,
                                   height = 500,
                                   statistics = c(
                                     "estimate", "bias",
                                     "stderr", "tstat",
                                     "coverage", "power"
                                   )) {

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
          value = 0.6
        ),
        sliderInput("EY3",
          "Prob[Y(3) = 1] equals...",
          min = 0,
          max = 1,
          value = 0.45
        ),
        sliderInput("M",
          "Number of simulations...",
          min = 3,
          max = 1000,
          value = 100
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
        result <- lapply(seq(input$M), function(i) {
          incProgress(1 / input$M, detail = paste("Simulation ", i))

          run_bernoulli_experiment(
            means = c(input$EY1, input$EY2, input$EY3),
            algorithm = input$algorithm,
            T = T,
            floor = floor,
            initial_batch = as.integer(T * initial_batch_prop)
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
        results <- simulate()

        p_assign <- plot_assignment_probabilities(results)
        p_infer <- plot_inference(results, statistics = statistics)

        grid.arrange(p_assign, p_infer,
          layout_matrix = rbind(
            c(1, 1),
            c(2, 2),
            c(2, 2)
          )
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



bias_stderr_experiment_control <- function(T = 100,
                                           floor = 0.01,
                                           initial_batch_prop = 0,
                                           height = 700,
                                           statistics = c(
                                             "estimate", "bias",
                                             "stderr", "tstat",
                                             "coverage", "power"
                                           )) {

  # Define UI for application that draws a histogram
  ui <- fillPage(

    # Sidebar with a slider input for number of bi
    sidebarLayout(
      sidebarPanel(
        sliderInput("EY1",
          "Prob[Y(1) = 1] equals...",
          min = 0,
          max = 1,
          value = 0.25
        ),
        sliderInput("EY2",
          "Prob[Y(2) = 1] equals...",
          min = 0,
          max = 1,
          value = 0.6
        ),
        sliderInput("EY3",
          "Prob[Y(3) = 1] equals...",
          min = 0,
          max = 1,
          value = 0.45
        ),
        # sliderInput("control",
        #             "Fixed assignment to control",
        #             min = 0,
        #             max = 1,
        #             value = 0.33
        # ),
        sliderInput("M",
          "Number of simulations...",
          min = 3,
          max = 1000,
          value = 100
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
        result <- lapply(seq(input$M), function(i) {
          incProgress(1 / input$M, detail = paste("Simulation ", i))

          if (input$control == "fixed") {
            run_bernoulli_control_experiment(
              means = c(input$EY1, input$EY2, input$EY3),
              algorithm = input$algorithm,
              T = T,
              floor = floor,
              initial_batch = as.integer(T * initial_batch_prop)
            )
          } else {
            run_bernoulli_experiment(
              means = c(input$EY1, input$EY2, input$EY3),
              algorithm = input$algorithm,
              T = T,
              floor = floor,
              initial_batch = as.integer(T * initial_batch_prop)
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
        results <- simulate()

        p_assign <- plot_assignment_probabilities(results)
        p_infer <- plot_inference(results, statistics = statistics)
        p_cinfer <- plot_control_inference(results, statistics = statistics)

        grid.arrange(p_assign, p_cinfer,
          layout_matrix = rbind(
            c(1, 1),
            c(2, 2),
            c(2, 2)
          )
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
