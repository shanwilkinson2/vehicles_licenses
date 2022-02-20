# visualise license data

library(plotly)

sheet_data <- read.csv("pcode dist driving licenses.csv")

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
