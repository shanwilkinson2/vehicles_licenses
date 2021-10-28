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
                destfile = paste0(getwd(), "/licenses", stringr::str_sub(links2$link[i], 45, -1) ))
  #, 
  #error = function(e) print(paste(file, 'did not work out')) 
 # )  
  
           }

  
purrr::map()