# download data

library(XML)
library(dplyr)
library(readxl)
library(ggplot2)

#######################################################

drivers <- readRDS("pcode dist driving licenses.RDS")
points <- readRDS("pcode dist points.RDS")

########################################################

# read in list of what's downloaded, generated below
  # read list of files
  excel_files <- readRDS("excel_files.RDS")

  # read list of sheet names per file
  sheet_names2 <- readRDS("sheet_names2.RDS")
  
########################################################
  
########################################################

# driving license holders
# https://data.gov.uk/dataset/d0be1ed2-9907-4ec4-b552-c048f6aec16a/gb-driving-licence-data

# setup to download files
   url <- "https://data.gov.uk/dataset/d0be1ed2-9907-4ec4-b552-c048f6aec16a/gb-driving-licence-data"
   doc <- htmlParse(readLines(url), 
                    asText = TRUE)
   links <- xpathSApply(doc, "//a/@href")
   free(doc)
   
   links2 <- data.frame(link = links) %>%
     filter(grepl("https://data.dft.gov.uk/driving-licence-data/", link))

# make new folder to put them in      
   dir.create("licenses")

# need to download files first then read

   for(i in 1:nrow(links2)){
   
       download.file(links2$link[i], 
                   destfile = paste0(getwd(), "/licenses", stringr::str_sub(links2$link[i], 45, -1) ),
                   mode = "wb")
     
   }

# get list of filenames & file types that have been downloaded
    excel_files <- list.files(path="./licenses", pattern = ".xls") %>%
      as.data.frame()
    
    names(excel_files) = "filename"
   
    excel_files <- excel_files %>%
      mutate(filetype = stringr::str_extract(filename, "\\.xlsx?$"))

 # get all sheet names
 
    for(i in 1:nrow(excel_files)){
      
       file_name_path <- paste0("./licenses/", excel_files[i,1])
       
       if(i == 1){
          sheet_names <- data.frame(
             filename = excel_files[i, 1],
             sheetname = excel_sheets(file_name_path)
          )
       } else {
          
          sheet_names <- sheet_names %>%
             bind_rows(
                data.frame(
                  filename = excel_files[i, 1],
                  sheetname = excel_sheets(file_name_path)
                )
          )
          
       }
       
    }
    
    # separate out the date & sheet name without date
    
    sheet_names2 <- sheet_names %>%
       mutate(sheetname_code = stringr::str_sub(sheetname, 1, 7),
              file_date = stringr::str_remove(filename, "\\.xlsx?$"),
              file_date = stringr::str_remove(file_date, "driving-licence-data-"),
              file_date = paste(1, file_date, sep = "-"),
              file_date_date = lubridate::dmy(file_date)
              )
    
    
    
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
    # saveRDS(sheet_data2, "./vehicles_licenses/pcode dist driving licenses.RDS")

 #######################################################################
    
  ##### read in penalty points 
    
    # different number of rows to skip in earlier files & on different sheets
    
    for(i in 1: nrow(excel_files)){
      
      print(paste("file number", i, excel_files[i, 1]))
      
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
                           file_name_date == "2017-06-01" ~22,
                           file_name_date == "2021-10-01" ~24,
                           file_name_date == "2022-02-01" ~24,
                           file_name_date < "2017-01-01"  ~27,
                           TRUE ~23
                         )) %>%
        mutate(file_date = file_name_date)
      
      # first row is another heading, so is second col
      data <- data[-1, -2] 
      names(data)[1] <- "pcode_district" 
      # make number of drivers with each number of points numeric
      data <- data %>%
        mutate(across(.cols = -c(pcode_district, file_date), .fns = as.numeric))
      print(names(data))
      
      if(i == 1) {
        sheet_data <- data
      } else {
        sheet_data <- bind_rows(sheet_data, data)
        #print(summary(sheet_data))
      }
      
      if(i == nrow(excel_files)) {
        return(sheet_data)
      }
    }
    
    # make postcodes in the usual format (e.g not BL01 but BL1)
    sheet_data2 <- sheet_data %>%
      filter(pcode_district != "District|Sum") %>%
      mutate(pcode_dist_letters = stringr::str_extract(pcode_district, "[:alpha:]+"),
             pcode_dist_numbers = stringr::str_extract(pcode_district, "[:digit:]+"),
             pcode_dist_numbers = as.numeric(pcode_dist_numbers),
             pcode_district = paste0(pcode_dist_letters, pcode_dist_numbers)
      ) %>%
      select(-c(pcode_dist_letters, pcode_dist_numbers)) %>%
      tidyr::pivot_longer(cols = -c(pcode_district, file_date, Total),
                          names_to = "num_points",
                          values_to = "num_drivers") %>%
      mutate(num_points = as.numeric(num_points)) %>%
      filter(num_points != "Sum" 
      ) 
    
    # save file
    saveRDS(sheet_data2, "./vehicles_licenses/pcode dist points.RDS")

    # doing some checks
    sheet_data2 %>%
      filter(pcode_district == "BL1") %>%
      # select(-`Total`) %>%
      tidyr::pivot_longer(cols = -c(pcode_district, file_date, Total),
                          names_to = "num_points",
                          values_to = "num_drivers") %>%
      mutate(num_points = as.numeric(num_points)) %>%
      filter(num_points != "Sum" &
             num_drivers != 0
             ) %>%
      na.omit() %>%
      View()
    
    
    
    
    sheet_data2 %>% 
      group_by(file_date) %>%
      slice(1) %>%
      View()
    
    # write file 
    # saveRDS(sheet_data2, "pcode dist driving licenses.csv")
    
##################################################################    
    
  # get list of filenames & file types that have been downloaded
    excel_files <- list.files(path="./licenses", pattern = ".xls") %>%
      as.data.frame()
    
    names(excel_files) = "filename"
    
    excel_files <- excel_files %>%
      mutate(filetype = stringr::str_extract(filename, "\\.xlsx?$"))
    
    # write list of file names
    saveRDS(excel_files, "excel_files.RDS")

    
  # get all sheet names
    
    for(i in 1:nrow(excel_files)){
      
      file_name_path <- paste0("./licenses/", excel_files[i,1])
      
      if(i == 1){
        sheet_names <- data.frame(
          filename = excel_files[i, 1],
          sheetname = excel_sheets(file_name_path)
        )
      } else {
        
        sheet_names <- sheet_names %>%
          bind_rows(
            data.frame(
              filename = excel_files[i, 1],
              sheetname = excel_sheets(file_name_path)
            )
          )
        
      }
      
    }
    
    # separate out the date & sheet name without date
    
    sheet_names2 <- sheet_names %>%
      mutate(sheetname_code = stringr::str_sub(sheetname, 1, 7),
             file_date = stringr::str_remove(filename, "\\.xlsx?$"),
             file_date = stringr::str_remove(file_date, "driving-licence-data-"),
             file_date = paste(1, file_date, sep = "-"),
             file_date_date = lubridate::dmy(file_date)
      )
    
   # save file
    saveRDS(sheet_names2, "sheet_names2.RDS")
    