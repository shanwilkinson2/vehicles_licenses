# license holders

library(dplyr)
library(ggplot2)
library(plotly)

# load data (generated in 'get_data.R')
  points <- readRDS("pcode dist points.RDS")

#######################################################
 
  points2 <- points %>%
    # Total is the same but not on all files 
    select(-Total) %>%
    group_by(file_date, pcode_district)%>%
    mutate(calc_total = sum(num_drivers, na.rm = TRUE)) %>%
    # join in num drivers
    left_join(drivers %>% 
                select(pcode_district, file_date, full_licenses_total = full_total),
              by = c("file_date", "pcode_district")) 
  
   
##################################################################  
  
  chart_data <- points2 %>%
    group_by(file_date, pcode_district, full_licenses_total) %>%
    filter(num_points >=12) %>%
    summarise(points_total = sum(num_drivers, na.rm = TRUE)) %>%
    mutate(per_10k_licenses = points_total/full_licenses_total*10000) 

  
  chart_data2 <- chart_data %>%
    filter(pcode_district %in% c("BL1", "BL2", "BL3", "WN1")) %>%
    group_by(pcode_district)
  
  chart_data2 %>%
    plot_ly() %>%
    add_lines(x = ~file_date, y = ~per_10k_licenses, color = ~pcode_district) %>%
    layout(
      xaxis = list(title = "Date",
                   tickvals = format(chart_data2$file_date, format = "%B %Y"),
                   tickangle = -45),
      yaxis = list(title = "Rate of 12+ points over time"), 
      title = "12+ points per 10,000 full license holders"
    )  
  