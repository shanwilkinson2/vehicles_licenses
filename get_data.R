# get data

library(XML)
library(dplyr)
library(readxl)
library(ggplot2)

# read in driving license data generated below
sheet_data <- read.csv("pcode dist driving licenses.csv")
  # save for app
    saveRDS(sheet_data, "./vehicles_licenses/pcode_dist_driving_license.RDS")

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

    
    # write file 
    # data.table::fwrite(sheet_data, "pcode dist driving licenses.csv")

    # test - pcode district has zero if it doesn't need it 
      # e.g. BL1 is BL01
      # 2016 cells are shifted down with 3 extra rows in postcode district col
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

    # early dates are missing - need to skip more rows
    sheet_data %>%
      filter(file_date == "2013-07-01") %>%
      arrange(desc(file_date)) %>%
      View()
    
    
        