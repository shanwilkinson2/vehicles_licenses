# load packages #####################################

library(shiny)
library(dplyr)
library(shinydashboard)
library(plotly)

# load data #########################################
# for testing - setwd("./vehicles_licenses")

licenses <- readRDS("pcode dist driving licenses.RDS")

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
    selectizeInput(inputId = "select_pcode_dist", 
                label = "Select postcode district:", 
                choices = NULL,
                multiple = TRUE
    ),
    em("(delete selected or type & search)")
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
          ),
        p("Population is usual residents (all ages) from 2011 census. No mid year estimates are available for postcode district. Best fit may be added to this app at a later date."),
        a("Populations from Nomis",
          href = "https://www.nomisweb.co.uk/census/2011/ks101ew",
          target = "_blank"),
        p("Code for this app is on my github"),
        a("link", 
          href = "https://github.com/shanwilkinson2/vehicles_licenses",
          target = "_blank")
      )
    )
  )
)

    

# server ##########################################################
server <- function(input, output, session) {

  # generate options for pcode dist dropdown
    updateSelectizeInput(session = session, 
                         inputId = "select_pcode_dist", 
                         choices = licenses$pcode_district, 
                         server = TRUE,
                         selected = c("BL3", "BL5", "BL7")
                         )
  
  
  # generate data for download button
  output$license_data <- downloadHandler(filename = "license_holders.csv",
                                      # create file for downloading
                                      content = function(file){
                                        write.csv(licenses
                                                  , file)
                                      })
  
  # make chart of license holders over time
  
    chart_data <- reactive({licenses %>%
      filter(pcode_district %in% input$select_pcode_dist) %>%
      # filter(pcode_district %in% c("BL1", "BL2", "BL3")) %>%
      group_by(pcode_district)
    })
    
    output$licenses_chart <- renderPlotly({
    chart_data() %>%
      plot_ly() %>%
      add_lines(x = ~file_date, y = ~full_licenses_per_resident, color = ~pcode_district) %>%
      layout(
        xaxis = list(title = "Date",
                     tickvals = format(chart_data()$file_date, format = "%B %Y"),
                     tickangle = -45),
        yaxis = list(title = "Full license holders per resident"), 
        title = "Driving license holders over time"
      )
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
