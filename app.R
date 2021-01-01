library(readr)
library(binostics)
library(tidyverse)
library(gridExtra)
library(shinyWidgets)

big_epa_cars_2019 <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-15/big_epa_cars.csv") %>%
  filter(year == 2019)

my_vars <- c("barrels08", "cylinders", "city08", "highway08", "feScore", "fuelCost08", "co2TailpipeGpm", "youSaveSpend")

ui <- fluidPage(

  fluidRow(HTML("<center>
                <h1>Visualising Scagnostics</h1>
                <p style='font-size:20px'>This application visualises scatterplots of the combinations of a few variables of the
                <a href='https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-10-15'>big_epa_cars dataset</a>
                and calculates the scagnostics on each combination of variables.</p>
                </center>"),
          div(style = "height:50px;")),


  fluidRow(
    column(1),
    column(3, selectInput("v1", label = "Select x Variable", choices = my_vars, selected = "barrels08")),
    column(3, selectInput("v2", label = "Select y Variable", choices = my_vars, selected = "city08")),
    column(1),
    column(4, circleButton(inputId = "MoreInfo", icon = icon("info")),
           "Descriptions of Variables and Interpretation of Scagnostics")
  ),

  fluidRow(
    column(1),
    column(10, plotOutput("scagnosticsplots")),
    column(1))

)



server <- function(input, output, session) {

  observe({
    if(!is.null(input$v2))
      updateSelectInput(session, "v1",
                        choices = my_vars[!(my_vars %in% input$v2)],
                        selected = isolate(input$v1) )
  })

  observe({
    if(!is.null(input$v1))
      updateSelectInput(session, "v2",
                        choices = my_vars[!(my_vars %in% input$v1)],
                        selected = isolate(input$v2) )
  })

  output$scagnosticsplots <- renderPlot({

    p1 <- ggplot(big_epa_cars_2019,
                 aes(x = get(input$v1),
                     y = get(input$v2))) +
      geom_point() +
      theme_bw() +
      labs(x = input$v1,
           y = input$v2)


    s <- scagnostics(big_epa_cars_2019[[input$v1]], big_epa_cars_2019[[input$v2]])$s
    df_s <- tibble(scag = names(s), value = s) %>%
      mutate(scag = fct_reorder(scag, value))

    p2 <- ggplot(df_s, aes(x=value, y=scag)) +
      geom_point(size=4, colour="orange") +
      geom_segment(aes(x=value, xend=0,
                       y=as.numeric(scag),
                       yend=as.numeric(scag)), colour="orange") +
      theme_bw() +
      labs(x = "Scagnostic value",
           y = "")

    grid.arrange(p1, p2, ncol=2)

  })
  
  observeEvent(input$MoreInfo, {
    showModal(modalDialog(
      HTML("
      <h4><b>Descriptions of Variables</b></h4>
           <p><b>barrels08 : </b>Annual Petroleum Consumption (barrels)</p>
           <p><b>cylinders : </b>Number of cylinders</p>
           <p><b>city08 : </b>Fuel Efficiency in City (MPG)</p>
           <p><b>highway08 : </b>Fuel Efficiency in Highways (MPG)</p>
           <p><b>feScore : </b>EPA Fuel Economy Score</p>
           <p><b>fuelCost08 : </b>Annual Cost for Fuel</p>
           <p><b>co2TailpipeGpm : </b>CO2 emmitted (grams/mile)</p>
           <p><b>youSaveSpend : </b>Saving/Spending over 5 years compared to an average car($)</p>
      </br></br>
      <h4><b>Interpretation of Scagnostics</b></h4>
      <a href='https://research.tableau.com/sites/default/files/Wilkinson_Infovis-05.pdf'>Graph-Theoretic Scagnostics</a></br>
      <a href='https://www.cs.uic.edu/~tdang/file/ScagExplorer.pdf'>ScagExplorer: Exploring Scatterplots by Their Scagnostics</a></p>"),
      easyClose = TRUE
    ))
  })


}


shinyApp(ui, server)
