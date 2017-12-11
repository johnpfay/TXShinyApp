#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.

#Nicola Hill
#This script is the user interface (ui) portion of the web application.
    #The script loads appropriate files/packages and sets up the side bar and tabes


#1. Install packages-----
library(tidyverse)
library(lubridate)
library(plyr)
library(dplyr)
library(ggplot2)
library(scales)
library(dplyr)
library(shiny) 
library(DT)
library(shinythemes)


#Scripting the user interface-----
shinyUI(fluidPage(theme = shinytheme("cerulean"), 
  headerPanel("Texas Industrial Water Usage"), 
  
  sidebarPanel( 
    dateRangeInput("dateRange","Date Range",
                   start = "1970-01-01",
                   end = "2015-01-01"), 
    
    checkboxInput(inputId = "smoother",
                  label = "Add smoother?",
                  value = FALSE),

    radioButtons(inputId = "WaterType",
                 label = "Chose Type of Water",
                 choices = list("Surface Water" = "SurfaceWater", 
                          "Groundwater" = "Groundwater",
                          "Reuse Water"= "Reuse",
                          "All"= "All")),
    
    uiOutput("reactCounties") #list out counties in Texas
  ),
  
  mainPanel(
   tabsetPanel( id= "theTabs",
      tabPanel("Summary", 
               textOutput("Summary")), 
      
      tabPanel("Texas Map",
               plotOutput("TexasMap")),
      
      tabPanel("Historical Trends", 
               plotOutput("HistoricalTrends")),
      
      tabPanel("Largest Users", 
               DT::dataTableOutput("LargestUsers")),
      
      tabPanel("Credits", 
               textOutput("Credits"))
  )
 )
))

  