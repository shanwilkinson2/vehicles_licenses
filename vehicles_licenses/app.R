# load packages #####################################

library(shiny)
library(dplyr)
library(shinydashboard)

# load data #########################################
# for testing - setwd("./vehicles_licenses")

licenses <- readRDS("pcode_dist_driving_license.RDS")

# UI ################################################

ui <- dashboardPage(skin = "red", 
  dashboardHeader(title = "Driving licenses & vehicle data",
                  titleWidth = 330),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "License holders", tabName = "licenses", icon = icon("user")
      ),
      menuItem(
        "Vehicles", tabName = "vehicles", icon = icon("car")
      ),
      menuItem(
        "About", tabName = "about", icon = icon("info-circle")
      )
    ),
    
    # select pcode district
      # server side autocomplete as v many
      # not working yet
    selectizeInput(inputId = "select_pcode_dist", 
                label = "Select postcode district:", 
                choices = NULL,
                multiple = TRUE
    ),
    em("(delete selected to type & search)")
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "licenses",
        h2("License holders"),
        plotlyOutput("licenses_chart"),
        downloadButton("license_data", "Get the data (csv)")
      ),
      tabItem(
        tabName = "vehicles",
        h2("Vehicles")
      ),
      tabItem(
        tabName = "about",
        h2("About the data"),
        p("Data on driving license holders by postcode sector from DfT via"),
        a("data.gov.uk",
          href = "https://data.gov.uk/dataset/d0be1ed2-9907-4ec4-b552-c048f6aec16a/gb-driving-licence-data",
          target = "_blank"
          )
      )
    )
  )
)

    

# server ##########################################################
server <- function(input, output) {

  # generate options for pcode dist dropdown
  server <- function(input, output, session) {
    updateSelectizeInput(session, "select_pcode_dist", 
                         choices = licenses$pcode_district, 
                         server = TRUE
                         )
  }
  
  # generate data for download button
  output$license_data <- downloadHandler(filename = "license_holders.csv",
                                      # create file for downloading
                                      content = function(file){
                                        write.csv(licenses
                                                  , file)
                                      })
  
  # make chart of license holders over time
  output$licenses_chart <- renderPlotly({
    chart_data <- sheet_data %>%
      filter(pcode_district %in% c("BL1", "BL2", "BL3")) %>%
      group_by(pcode_district)
    
    chart_data %>%
      plot_ly() %>%
      add_lines(x = ~file_date, y = ~full_total, color = ~pcode_district) %>%
      layout(
        xaxis = list(title = "Date",
                     tickvals = format(chart_data$file_date, format = "%B %Y"),
                     tickangle = -45),
        yaxis = list(title = "Number of full license holders"), 
        title = "Driving license holders over time"
      )
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
