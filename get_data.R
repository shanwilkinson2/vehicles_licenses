# get data

library(XML)
library(dplyr)
library(readxl)


# driving license holders
# https://data.gov.uk/dataset/d0be1ed2-9907-4ec4-b552-c048f6aec16a/gb-driving-licence-data


url <- "https://data.gov.uk/dataset/d0be1ed2-9907-4ec4-b552-c048f6aec16a/gb-driving-licence-data"
doc <- htmlParse(readLines(url), 
                 asText = TRUE)
links <- xpathSApply(doc, "//a/@href")
free(doc)

links2 <- data.frame(link = links) %>%
  filter(grepl("https://data.dft.gov.uk/driving-licence-data/", link))

dir.create("licenses")

## need to download files first then read
# download.file(links2$link[2], 
#               destfile = paste0(getwd(), "/licenses", stringr::str_sub(links2$link[2], 45, -1) ))
# 

for(i in 1:nrow(links2)){
  #tryCatch(
    download.file(links2$link[i], 
                destfile = paste0(getwd(), "/licenses", stringr::str_sub(links2$link[i], 45, -1) ),
                mode = "wb")
  #, 
  #error = function(e) print(paste(file, 'did not work out')) 
 # )  
  
}

# get list of filenames & file types
 excel_files <- list.files(path="./licenses", pattern = ".xls") %>%
   as.data.frame()
 
 names(excel_files) = "filename"

 excel_files <- excel_files %>%
   mutate(filetype = stringr::str_extract(filename, "\\.xlsx?$"))

 # get-all sheet names
 
 for(i in 1:nrow(excel_files)){
   
   file_name_path <- paste0("./licenses/", excel_files[i,1])
   
   sheet_names <- excel_sheets(file_name_path)
   
 }