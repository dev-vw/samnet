# Shiny app for project

# Read in shiny library
library(shiny)
library(ggplot2)

setwd("/Users/sburtner/Documents/SOC294/msm_data")
msmdata <- as.tibble(read.dta('msm_data.dta'))

ui <- fluidPage(
  headerPanel("s  a  m  n  e  t"),
  h5("by Vania Wang and Susan Burtner"),
  h5("SOC294: Network Science, Final Project"),
  br(),
  h4(strong("samnet"), "is the sexual activity network and structural equation model
created from responses of men who have sex with men taken from the 2014 Mobile Study stemming
     from Seattle, Washington."),
  br(),
  p(strong("Motivation."), "It is both time and effort intensive to reveal and record sexual
activity networks. However, this information is invaluable, particularly when seeking to measure
    the rate and spread of sexually transmitted diseases..."),
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
  ),
  p("Interesting findings from the exploratory data analysis include the age
    at which men in this survey come out as gay, which is right around puberty."),
  br(),
  h2("Network construction"),
  p("This network was created through an R package called 'ergm'."),
  h3("A brief primer to exponential random graph models (ERGMs)"),
  h3("The process of generating ERGMs from egocentric data"),
  h3("A tale of three sexual network visualizations"),
  br(),
  h2("Structural equation modeling"),
  h3("How do we measure social support? Or discrimination?"),
  h3("Constructing the SEM"),
  br(),
  h2("Discussion"),
  h2("Future Work"),
  p("Thank you for your attention! Please email us with any questions. <sab00@umail.ucsb.edu>"),
  p(strong("This website was created using Shiny, an R package."))
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