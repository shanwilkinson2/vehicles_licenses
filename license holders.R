# read in license holders

library(dplyr)
library(ggplot2)
library(readxl)

# read in driving license data generated below
drivers <- readRDS("pcode dist driving licenses.RDS")

# save for app
saveRDS(sheet_data2, "./vehicles_licenses/pcode_dist_driving_license.RDS")

################################################################

# read in list of what's downloaded, generated in "get_data.R"
  # read list of files
    excel_files <- readRDS("excel_files.RDS")
  
  # read list of sheet names per file
    sheet_names2 <- readRDS("sheet_names2.RDS")

# read in driving licence holders per postcode 
# different number of rows to skip in earlier files & on different sheets

for(i in 1: nrow(excel_files)){
  
  file_name_path <- paste0("./licenses/", excel_files[i,1])
  file_name_date <- sheet_names2 %>%
    filter(filename == excel_files[i,1]) %>%
    head(1) %>%
    pull(file_date_date)
  selected_sheet <- "DRL0102"
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
                       TRUE ~9
                     )) %>%
    mutate(file_date = file_name_date)
  
  # first row is another heading
  data <- data[-1,] 
  names(data)[1] <- "pcode_district" 
  
  if(i == 1) {
    sheet_data <- data
  } else {
    sheet_data <- bind_rows(sheet_data, data)
  }
  
  if(i == nrow(excel_files)) {
    rm(data)
    rm(file_name_date)
    rm(file_name_path)
    sheet_data <- sheet_data[,1:8] %>%
      janitor::clean_names()
  }
}

# test - pcode district has zero if it doesn't need it 
# e.g. BL1 is BL01
# 2016 cells are shifted down with 2 extra rows in postcode district col
# 2013 - some issue

sheet_data %>%
  filter(pcode_district %in% c("BL01", "BL02", "BL03",
                               "BL04", "BL05", "BL06", "BL07",
                               "BL07", "BL08", "BL09",
                               "WN1", "M46")) %>%
  group_by(pcode_district)  %>%
  #View()
  ggplot() +
  geom_line(aes(x = file_date, y = full_total, color = pcode_district))

# # early dates are missing - need to skip more rows
# sheet_data %>%
#   filter(file_date == "2013-07-01") %>%
#   arrange(desc(file_date)) %>%
#   View()

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



# write file 
# data.table::fwrite(sheet_data2, "pcode dist driving licenses.csv")



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
    mutate(file_date = file_name_date)
  
  # first row is another heading, so is second col
  data <- data[-1, -2] 
  names(data)[1] <- "pcode_district" 
  
  if(i == 1) {
    sheet_data <- data
  } else {
    sheet_data <- bind_rows(sheet_data, data)
  }
  
  if(i == nrow(excel_files)) {
    rm(data)
    rm(file_name_date)
    rm(file_name_path)
    sheet_data <- sheet_data[,1:8] %>%
      janitor::clean_names()
  }
}

data %>%
  filter(pcode_district == "BL01") %>%
  # select(-`Total`) %>%
  tidyr::pivot_longer(cols = -c(pcode_district, file_date, Total),
                      names_to = "num_points",
                      values_to = "num_drivers") %>%
  View()


# make postcodes in the usual format (e.g not BL01 but BL1)
sheet_data2 <- sheet_data %>%
  mutate(pcode_dist_letters = stringr::str_extract(pcode_district, "[:alpha:]+"),
         pcode_dist_numbers = stringr::str_extract(pcode_district, "[:digit:]+"),
         pcode_dist_numbers = as.numeric(pcode_dist_numbers),
         pcode_district = paste0(pcode_dist_letters, pcode_dist_numbers)
  ) %>%
  select(-c(pcode_dist_letters, pcode_dist_numbers))


# write file 
# saveRDS(sheet_data2, "pcode dist driving licenses.RDS")

