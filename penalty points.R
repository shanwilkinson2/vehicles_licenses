# read in penalty points

library(dplyr)
library(ggplot2)

# read in list of what's downloaded, generated in "get_data.R"
  # read list of files
    excel_files <- readRDS("excel_files.RDS")

  # read list of sheet names per file
    sheet_names2 <- readRDS("sheet_names2.RDS")

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
    mutate(file_date = file_name_date)
  
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
    sheet_data <- sheet_data %>%
      janitor::clean_names()
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


# make postcodes in the usual format (e.g not BL01 but BL1)
sheet_data2 <- sheet_data %>%
  mutate(pcode_dist_letters = stringr::str_extract(pcode_district, "[:alpha:]+"),
         pcode_dist_numbers = stringr::str_extract(pcode_district, "[:digit:]+"),
         pcode_dist_numbers = as.numeric(pcode_dist_numbers),
         pcode_district = paste0(pcode_dist_letters, pcode_dist_numbers)
  ) %>%
  select(-c(pcode_dist_letters, pcode_dist_numbers))


# write file 
# data.table::fwrite(sheet_data2, "pcode dist driving licenses.csv")

