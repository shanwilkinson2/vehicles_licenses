# read in penalty points

library(dplyr)
library(ggplot2)
library(readxl)

# read in list of what's downloaded, generated in "get_data.R"
  # read list of files
    excel_files <- readRDS("excel_files.RDS")

  # read list of sheet names per file
    sheet_names2 <- readRDS("sheet_names2.RDS")
  
  # read driver info  
    drivers <- readRDS("pcode dist driving licenses.RDS")

##############################################################

##### read in penalty points ###

# different number of rows to skip in earlier files & on different sheets

for(i in 1: nrow(excel_files)){
  
  file_name_path <- paste0("./licenses/", excel_files[i,1])
  file_name_date <- sheet_names2 %>%
    filter(filename == excel_files[i,1]) %>%
    head(1) %>%
    pull(file_date_date)
  selected_sheet <- "DRL0132"
  sheet_to_read <- sheet_names2 %>%
    filter(filename == excel_files[i,1] & sheetname_code == selected_sheet
    )
  
  # read in sheet
  data <- read_excel(file_name_path,
                     sheet = sheet_to_read[1, "sheetname"],
                     # more stuff at the top in earlier files
                     skip = case_when(
                       file_name_date == "2012-11-01" ~25,
                       file_name_date == "2013-07-01" ~25,
                       file_name_date < "2017-01-01"  ~27,
                       TRUE ~23
                     )) %>%
    mutate(file_date = file_name_date) %>% 
    janitor::clean_names()
  
  # first row is another heading, so is second col
  data <- data[-1, -2] 
  names(data)[1] <- "pcode_district" 
  data <- data %>%
    #select(pcode_district:file_date) %>%
    mutate_at(vars(-c(file_date, pcode_district)), as.numeric)
  
  if(i == 1) {
    sheet_data <- data
  } else {
    sheet_data <- bind_rows(sheet_data, data)
  }
  
  if(i == nrow(excel_files)) {
    rm(data)
    rm(file_name_date)
    rm(file_name_path)
    sheet_data <- sheet_data #%>%
      #janitor::clean_names()
  }
}

# test    
    
  data %>%
    filter(pcode_district == "BL01") %>%
    # select(-`Total`) %>%
    tidyr::pivot_longer(cols = -c(pcode_district, file_date, Total),
                        names_to = "num_points",
                        values_to = "num_drivers") %>%
    View()


  # make postcodes in the usual format (e.g not BL01 but BL1, to include e.g. W1A)
  sheet_data2 <- sheet_data %>%
    mutate(pcode_dist1 = stringr::str_extract(pcode_district, "[:alpha:]+"),
           pcode_dist_a = stringr::str_remove(pcode_district, "^[:alpha:]+"),
           pcode_dist2 = stringr::str_extract(pcode_dist_a, "[:digit:]+"),
           pcode_dist2 = as.numeric(pcode_dist2),
           pcode_dist3 = stringr::str_extract(pcode_dist_a, "[:alpha:]"),
           pcode_dist3 = tidyr::replace_na(pcode_dist3, ""),
           pcode_district = paste0(pcode_dist1, pcode_dist2, pcode_dist3)
    ) %>%
    select(-c(pcode_dist1, pcode_dist_a, pcode_dist2, pcode_dist3))

# tidy data
sheet_data3 <- sheet_data2 %>%
  select(-c(sum, driver_count)) %>%
  tidyr::pivot_longer(cols = -c(pcode_district, file_date),
                      names_to = "num_points",
                      values_to = "num_drivers") %>%
  filter(num_drivers >0 & 
           num_points != "total" & 
           pcode_district != "TotalNA"
         ) %>%
  mutate(num_points = stringr::str_remove(num_points, "x"),
         num_points = stringr::str_remove(num_points, "_[:alnum:]+"),
         num_points = as.numeric(num_points),
         pcode_district = toupper(pcode_district)
         )

# join in num drivers - from 'license holders.R'
  drivers2 <- drivers %>%
    select(pcode_district, file_date, provisional_total, full_total) %>%
    mutate(full_provisional_total = provisional_total + full_total,
           file_date = as.Date(file_date))

  
  # increasing number of rows???
  sheet_data4 <- left_join(sheet_data3, drivers2,
                           by = c("pcode_district" = "pcode_district", 
                                  "file_date" = "file_date"))
  
# write file 
 saveRDS(sheet_data3, "pcode dist penalty points.RDS")

 # check how many unique combinations of pcode dist & file date
 penalty_pts_test <- sheet_data2 %>% 
   mutate(pcode_date = paste(pcode_district, file_date),
          source = "penalty points")  %>%
   select(pcode_date, source)
 
 drivers_test <- drivers %>%
   mutate(pcode_date = paste(pcode_district, file_date),
          source = "drivers")  %>%
   select(pcode_date, source)

 missing_rows <- anti_join(drivers_test, penalty_pts_test,
                           by = "pcode_date") 
 