# load packages #####################################

library(shiny)
library(dplyr)
library(shinydashboard)
library(plotly)
library(glue)

# load data #########################################
# for testing - setwd("./vehicles_licenses")

drivers <- readRDS("pcode dist driving licenses.RDS")
points <- readRDS("pcode dist points.RDS")

points2 <- points %>%
  # Total is the same but not on all files 
  select(-Total) %>%
  group_by(file_date, pcode_district)%>%
  mutate(calc_total = sum(num_drivers, na.rm = TRUE)) %>%
  # join in num drivers
  left_join(drivers %>% 
              select(pcode_district, file_date, full_licenses_total = full_total),
            by = c("file_date", "pcode_district")) 


# UI ################################################

ui <- dashboardPage(skin = "red", 
  dashboardHeader(title = "Driving licenses & vehicle data",
                  titleWidth = 330),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Drivers", tabName = "drivers", icon = icon("user")
      ),
      menuItem(
        "Points", tabName = "points", icon = icon("triangle-exclamation")
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
        tabName = "drivers",
        h2("Drivers"),
        plotlyOutput("drivers_chart"),
        downloadButton("drivers_data", "Get the data (csv)")
      ),
      tabItem(
        tabName = "points",
        h2("Points"),
        infoBoxOutput("max_points_box"),
        # select number of points
        uiOutput("points_slider"),
        # sliderInput(inputId = "select_num_points", 
        #             label = "Select number of points:", 
        #             min= 1, max= max(points_chart_data1()$num_points),
        #             value =c(6, max(points_chart_data1()$num_points))
        #             ),
        p("Runs very slowly....... Add slider to adjust number of points"),
        plotlyOutput("points_chart"),
        downloadButton("points_data", "Get the data (csv)")
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
                         choices = drivers$pcode_district, 
                         server = TRUE,
                         selected = c("BL3", "BL5", "BL7")
                         )
  
  
  # generate data for download buttons
  output$license_data <- downloadHandler(filename = "license_holders.csv",
                                      # create file for downloading
                                      content = function(file){
                                        write.csv(drivers
                                                  , file)
                                      })
  
  output$points_data <- downloadHandler(filename = "penalty_points.csv",
                                         # create file for downloading
                                         content = function(file){
                                           write.csv(points2
                                                     , file)
                                         })
  
  # make chart of license holders over time
  
    drivers_chart_data <- reactive({drivers %>%
      filter(pcode_district %in% input$select_pcode_dist) %>%
      group_by(pcode_district)
    })
    
    output$drivers_chart <- renderPlotly({
    drivers_chart_data() %>%
      plot_ly() %>%
      add_lines(x = ~file_date, y = ~full_licenses_per_resident, color = ~pcode_district) %>%
      layout(
        xaxis = list(title = "Date",
                     #tickvals = format(drivers_chart_data()$file_date, format = "%B %Y"),
                     tickangle = -45),
        yaxis = list(title = "Full license holders per resident"), 
        title = "Driving license holders over time"
      )
    
  })
  
  # generate reactive slider 
    output$points_slider <- renderUI({
      sliderInput("points_slider", "Points slider",
                  min = min(points_chart_data1()$num_points), 
                  max = max(points_chart_data1()$num_points),
                  value = c(6, max(points_chart_data1()$num_points)))
    })
    
  # generate data for points chart
    points_chart_data1 <- reactive({
      points2 %>%
      filter(pcode_district %in% input$select_pcode_dist) 
    })
    
    output$max_points_box <- renderInfoBox({
      infoBox("Max points:", 
              max(points_chart_data1()$num_points, na.rm = TRUE),
              icon = icon("arrow-circle-up"),
              color = "red"
              )
      })
    
    points_chart_data2 <- reactive({
      points_chart_data1() %>%
        group_by(file_date, pcode_district, full_licenses_total) %>%
        filter(num_points >=12) %>%
        summarise(points_total = sum(num_drivers, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(per_10k_licenses = points_total/full_licenses_total*10000) %>%
        group_by(pcode_district) 
    })
    
  # generate points chart
    output$points_chart <- renderPlotly({
    plot_ly(points_chart_data2()) %>%
      add_lines(x = ~file_date, y = ~per_10k_licenses, color = ~pcode_district) %>%
      layout(
        xaxis = list(title = "Date",
                     #tickvals = format(chart_data$file_date, format = "%B %Y"),
                     tickangle = -45),
        yaxis = list(title = "Rate with selected points over time"), 
        title = glue("12+ points per 10,000 full license holders")
      )  
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
