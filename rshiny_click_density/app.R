library(shiny)
library(tidyverse)

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "morph"),
  titlePanel("User Click Density Plotted in 1-D and 2-D"),
  p(
    "This page demonstrates reactive shiny plots where the density plot is
    updated by where the user clicks. This is demonstrated in 1 dimension
    and 2 dimensions. In the plots, 2 points placed initially."
  ),
  fluidRow(
    column(
      width = 6,
      plotOutput("plot1", click = "plot1_click"),
    ),
    column(
      width = 6,
      plotOutput("plot2", click = "plot2_click")
    )
  )
)

server <- function(input, output, session) {
  thematic::thematic_shiny()
  # 1-D Plot logic ------------------------------
  points_1d <- reactiveVal(data.frame(x = c(-1.1, 1.1)))

  observeEvent(input$plot1_click, {
    x <- input$plot1_click$x
    points_1d() |>
      add_row(x = x) |>
      points_1d()
  })

  output$plot1 <- renderPlot({
    points_1d() |>
      ggplot(aes(x)) +
      geom_density(fill = "magenta") +
      coord_cartesian(
        xlim = c(-1, 1), ylim = c(0.1, 1) # arbitrary limits
      ) +
      labs(
        title = "Density of User Clicks in 1-D",
        x = "", y = ""
      ) +
      theme(
        legend.position = "None",
        axis.ticks = element_blank(),
        axis.text = element_blank()
      )
  })

  # 2-D plot logic ------------------------------

  # Point data frame
  points_2d <- reactiveVal(data.frame(x = c(-1.1, 1.1), y = c(-1.1, 1.1)))

  # updates point dataframe on click
  observeEvent(input$plot2_click, {
    x <- input$plot2_click$x
    y <- input$plot2_click$y
    points_2d() |>
      add_row(x = x, y = y) |>
      points_2d()
  })

  # Renders 2-D density plot
  output$plot2 <- renderPlot({
    points_2d() |>
      ggplot(aes(x, y)) +
      geom_density_2d_filled() +
      geom_point() +
      coord_cartesian(
        xlim = c(-1, 1), ylim = c(-1, 1)
      ) +
      labs(
        title = "Density of User Clicks in 2-D",
        x = "", y = ""
      ) +
      theme(
        legend.position = "None",
        axis.ticks = element_blank(),
        axis.text = element_blank()
      )
  })
}

shinyApp(ui, server)
