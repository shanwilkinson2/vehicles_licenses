# load packages #####################################

library(shiny)
library(dplyr)
library(shinydashboard)

# load data #########################################
# for testing - setwd("./vehicles_licenses")

licenses <- readRDS("pcode_dist_driving_license.RDS")

# UI ################################################

ui <- dashboardPage(skin = "red",
  dashboardHeader(title = "Driving licenses & vehicle data"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "License holders", tabName = "licenses", icon = icon("user")
      ),
      menuItem(
        "Vehicles", tabName = "vehicles", icon = icon("car")
      ),
      menuItem(
        "About", tabName = "about", icon = icon("circle-info")
      )
    )
  ),
  dashboardBody(
    
  )
)

    

# server ##########################################################
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
