## --------------------------------------------------------------------------------------##
##
## Script name: 02_Shiny_placeholder_Example.R
##
## Purpose of the script:
##
## Author: Chinmay Deval
##
## Created On: 2022-01-21
##
## Copyright (c) Chinmay Deval, 2022
## Email: chinmay.deval91@gmail.com
##
## --------------------------------------------------------------------------------------##
##  Notes:
##   
##
## --------------------------------------------------------------------------------------##

## --------------------------clear environment and console-------------------------------##
rm(list = ls())
cat("\014")

## ----------------------------------Load packages---------------------------------------##
library(shiny)


## --------------------------------------------------------------------------------------##
# Define UI for application that draws a histogram
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(sliderInput("samplesize","Sample Size:",min = 100,max = 10000,value = 1000)),
    mainPanel(plotOutput("distPlot"))
  )
)

## --------------------------------------------------------------------------------------##
# Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot <- renderPlot({
    hist(rnorm(input$samplesize),col='darkorchid',xlab="Sample",main="Standard Normally Distributed Sample")},
    height=300
  )
}

## --------------------------------------------------------------------------------------##
# Run the application 
shinyApp(ui = ui, server = server)