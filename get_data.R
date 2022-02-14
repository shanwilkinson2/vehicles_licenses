# get data

library(XML)
library(dplyr)
library(readxl)


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
    file_name_path <- paste0("./licenses/", excel_files[i,1])
    selected_sheet <- "DRL0102"
    sheet_to_read <- sheet_names2 %>%
       filter(filename == excel_files[i,1] & sheetname_code == selected_sheet
            
            )
    
    data <- read_excel(file_name_path,
                       sheet = sheet_to_read[1, "sheetname"],
                       skip = 9)
    # first row is another heading
       data[-1,] %>%
          View()
    