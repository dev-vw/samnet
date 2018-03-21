##' Shiny app for Winter 2018 NS2 project
##' @authors Su Burtner and Vania Wang
##' -------------------------------------

## Reads in the requisite libraries
library(shiny)
library(ggplot2)
library(tidyverse)
library(foreign)
library(networkD3)

## VW: sets the current working directory
setwd("/Users/vaniawang/Google Drive/grad school/PhD UCSB/projects/msm_seattle/raw_data/Data/")

## SB: sets the current working directory 
## setwd("/Users/sburtner/Documents/SOC294/msm_data")

## reads in the dataset
msmdata <- as.tibble(read.dta('msm_data.dta'))

## loads the nodes and links datasets for the three networks
links1 <- readRDS("links1.rds")
nodes1 <- readRDS("nodes1.rds")
links2 <- readRDS("links2.rds")
nodes2 <- readRDS("nodes2.rds")
links3 <- readRDS("links3.rds")
nodes3 <- readRDS("nodes3.rds")

## instantiating the 'ui' parameter
ui <- fluidPage(headerPanel("S  A  M  N  E  T"),
                h4(strong("samnet"), "is a sexual network and structural equation model abstracted from responses of a 2014 survey of men who have sex with men from Seattle, Washington State"),
                p(h5("Vania Wang and Susan Burtner"),
                  h5("SOC294: Network Science 2, Final Project")),
                br(),
                
                h2("I. Project Motivation"),
                p("Can sexual bridging contribute to the spread of HIV in concentrated epidemics? In countries that show concentrated HIV epidemics, infections are typically confined within defined key populations. How much of infection spread can be contributed by sexual relationships between key populations and the general population?"),
                br(),

                h2("II. Project Goals"),
                h4(tags$ol(tags$li("To construct sexual networks MSM and women based on egocentric data"),
                           tags$li("To explore the relationship between social support, discrimination, and how these factors influence sexual behavior"),
                           tags$li("To set-up a theoretical framework and network model on which to develop a dynamic HIV transmission model for the MSM and female population of Seattle"))),
                br(),
                
                h2("III. Exploratory data analysis"),
                sidebarPanel(
                    selectInput("selectVar", "Select a variable",
                                choices = c("Age" = "age",
                                            "Employment" = "employment", 
                                            "Education" = "education", 
                                            "Income" = "income"))),          
                mainPanel(plotOutput('plot1')),
                p("Interesting findings from the exploratory data analysis include the age at which men in this survey come out as gay, which is right around puberty."),
                br(),

                h2("IV. Network construction using ERGMs"),
                h3("A brief primer to exponential random graph models (ERGMs)"),
                h4("What are ERGMs?"),
                tags$ul(tags$li("A class of statistical models for social networks"),
                        tags$li("Think of it as logistic regression on networks while considering the conditional dependencies between edges"),
                        tags$li("The outcome in question is the probability of two 'actors' or nodes given certain predictors and network statistics")),
                
                h4("ERGM-building steps"),
                tags$ol(tags$li("Theorize about configurations"),
                        tags$li("Think about which predictors we are interested in."),
                        tags$li("Provide a set of summary statistics.")),
                
                h4("The power of an ERGM vs logistic regression model"),
                tags$ul(tags$li("ERGMs help you examine the role of dependence in a model while techniques like logistic regression assume independence among its predictors"),
                        tags$li("For example, if person A views person B as a friend...")),
                br(),

                h3("The process of generating ERGMs from egocentric data"),
                h4("Sociocentric vs egocentric network surveys"),
                tags$ul(tags$li("Sociocentric survey designs create complete networks"),
                        tags$li("Egocentric survey designs collect information from the respondents only")),
                h4("What should the node set (n) be?"),
                tags$ul(tags$li("It should reflect the population of interest, namely the demographic makeup of Seattle"),
                        tags$li("I used 2010 census data and population size estimates from Jeremy Grey et al for raw numbers on MSM and adult female numbers in King County, Washington Stat")),
                h4("How do you calculate the number of edges for this network?"),
                tags$ol(tags$li("find the mean number of female sexual partners per respondent"),
                        tags$li("find the mean number of male sexual partners per respondent"),
                        tags$li("multiply these means by the number of male nodes in the network and add them together")),
                h4("What are the rules that we will impose on this network?"),
                tags$ol(tags$li("sexual partnerships can happen between males"),
                        tags$li("sexual partnerships can happen between males and females"),
                        tags$li("sexual partnerships cannot happen between females and females")),
                br(),

                h3("ERGM model results"),
                h4("The parsimonious model"),
                mainPanel(img(src = "ergm1.png", height = 240, width = 800)),
                br(),
                h4("Let's consider sexual ties"),
                mainPanel(img(src = "ergm2.png", height = 240, width = 800)),
                br(),
                h4("Let's consider sexual ties and HIV status"),
                mainPanel(img(src = "ergm3.png", height = 240, width = 800)),
                br(),
                
                h3("A tale of three sexual network visualizations"),
                sidebarLayout(
                    sidebarPanel(
                        sliderInput("opacity", "Opacity (not for Sankey)",
                                    0.6, min = 0.1,
                                    max = 1, step = .1)
                    ),
                    mainPanel(
                        tabsetPanel(
                            tabPanel("No predictors",
                                     forceNetworkOutput("force1")),
                            tabPanel("Nodemix",
                                     forceNetworkOutput("force2")),
                            tabPanel("Nodemix + hivstatus",
                                     forceNetworkOutput("force3"))))
                ),
                br(),
                
                h2("V. Structural equation modeling"),
                h3("How do we measure social support? Or discrimination?"),
                h3("Constructing the SEM"),
                br(),
                
                h2("VI. Discussion"),
                br(),
                
                h2("VII. Future Work"),
                br(),
                
                h2("VIII. References"),
                br(),
                
                p("Thank you for your attention! Please email us with any questions. <vaniawang@ucsb.edu> <sab00@umail.ucsb.edu>"),
                p(strong("This website was created using Shiny, an R package.")))

## instantiating the 'server' parameter
server <- function(input, output) {

    output$force1 <- renderForceNetwork({
        forceNetwork(Links = links1,
                     Nodes = nodes1,
                     NodeID = "name",
                     Group = "group",
                     opacity = input$opacity,
                     zoom = TRUE)
    })
    
    output$force2 <- renderForceNetwork({
        forceNetwork(Links = links2,
                     Nodes = nodes2,
                     NodeID = "name",
                     Group = "group",
                     opacity = input$opacity,
                     zoom = TRUE)
    })
    

    output$force3 <- renderForceNetwork({
        forceNetwork(Links = links3,
                     Nodes = nodes3,
                     NodeID = "name",
                     Group = "group",
                     opacity = input$opacity,
                     zoom = TRUE)
    })

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

## launches the app!
shinyApp(ui = ui, server = server)
