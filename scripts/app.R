# Shiny app for project

# Read in shiny library
library(shiny)
library(ggplot2)

setwd("/Users/sburtner/Documents/SOC294/msm_data")
msmdata <- as.tibble(read.dta('msm_data.dta'))

ui <- fluidPage(
  headerPanel("samnet"),
  h5(strong("samnet"), "is the sexual activity network and structural equation model
      created from data of men who have sex with mex in the 2014 Mobile Study stemming
     from Seattle, Washington."),
  h2("Exploratory data analysis"),
  sidebarPanel(
    selectInput("selectVar", "Select a variable",
                choices = c("Age" = "age",
                            "Employment" = "employment", 
                            "Education" = "education", 
                            "Income" = "income"))
  ),
  mainPanel(
    plotOutput('plot1')
  )
)

server <- function(input, output) {
  
  selectedData <- reactive({
    msmdata[, c("hivstatus", input$selectVar)]
  })
  
  output$plot1 <- renderPlot({
    ggplot(selectedData(), aes_string(input$selectVar)) +
      geom_bar(aes(fill = hivstatus)) +
      coord_flip() +
      guides(fill = guide_legend(title = "HIV status"))
  })
}

shinyApp(ui = ui, server = server)