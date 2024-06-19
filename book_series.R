library(readxl)
library(rvest)
library(dplyr)
library(tidyr)
library(RSelenium)
library(stringr)

### Setup Selenium with the newest chrome version ### 
### follow this answer about how to download latest chromedriver
rD <- RSelenium::rsDriver(browser = "chrome",
                          chromever =
                            system2(command = "wmic",
                                    args = 'datafile where name="C:\\\\Program Files (x86)\\\\Google\\\\Chrome\\\\Application\\\\chrome.exe" get Version /value',
                                    stdout = TRUE,
                                    stderr = TRUE) %>%
                            stringr::str_extract(pattern = "(?<=Version=)\\d+\\.\\d+\\.\\d+\\.") %>%
                            magrittr::extract(!is.na(.)) %>%
                            stringr::str_replace_all(pattern = "\\.",
                                                     replacement = "\\\\.") %>%
                            paste0("^",  .) %>%
                            stringr::str_subset(string =
                                                  binman::list_versions(appname = "chromedriver") %>%
                                                  dplyr::last()) %>%
                            as.numeric_version() %>%
                            max() %>%
                            as.character())

remDr <- rD$client

#read manually created excel about book series
data <- read_excel("book_series.xlsx")

#get information about each book one by one
for(i in seq_len(nrow(data))) {
 
url <- data$url[i]

remDr$navigate(url)

webElems1 <- remDr$findElement("css", ".Text__title1")

data[i, "book_name"] <- webElems1$getElementText()[[1]][1]

webElems2 <- remDr$findElement("css", ".BookPageMetadataSection__ratingStats")

ratings_text <- webElems2$getElementText()[[1]][1]

parts <- unlist(strsplit(ratings_text, "\n| ratings| reviews"))

data[i, "avg_rating"] <- parts[1]
data[i, "no_of_ratings"] <-  gsub(",", "", parts[2])

webElems3 <- remDr$findElement(using = "css", "meta[property='og:image']")

data[i, "image_source"] <-  webElems3$getElementAttribute("content")[[1]]

Sys.sleep(5)
   
}

### Close Selenium
remDr$close()
rD$server$stop()

## Download book covers (downloads covers to your computer, used later in plots)
for(i in seq_len(nrow(data))) {
  
image_url <- as.character(data[i,7])
  
data[i, "image_name"] <- paste0(gsub(" ","_",str_to_lower(gsub("[^[:alnum:][:space:]]","",data$book_name[i]))),".jpg")

download.file(image_url, destfile = data$image_name[i] ,mode = "wb")
  
}
