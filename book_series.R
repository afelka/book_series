#loading libraries
library(readxl)
library(rvest)
library(dplyr)
library(tidyr)
library(RSelenium)
library(stringr)
library(ggplot2)
library(ggimage)

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

# create a dataset with first book from each series and keep only series and no_of_ratings
first_book_of_series <- data %>% filter(book_number == 1) %>% droplevels() %>%
                        select(series, no_of_ratings) %>% 
                        rename(first_book_no_of_ratings = no_of_ratings)

#merge with all books and create index by dividing no_of_ratings by first_book_no_of_ratings
data2 <- data %>% left_join(first_book_of_series, by = "series") %>% 
                  mutate(index = round((as.numeric(no_of_ratings) / 
                                        as.numeric(first_book_no_of_ratings))*100,0)) %>%
                  filter(index >=10)

# create scaled_index for size of images and label
data2 <- data2 %>%
  mutate(scaled_index = round(((index - min(index)) / (max(index) - min(index)))/20,4),
         label = ifelse(book_number > 1, paste0("Index: ", index), NA)) 
  
#Create ggplot2
gg <- ggplot(data2, aes(x = book_number, y = series)) +
  geom_image(aes(image = image_name), size = data2$scaled_index) +
  geom_text(aes(label = label), vjust = 2.1, hjust = 0.5, size = 3, na.rm = TRUE)  +
  theme_classic() +
  scale_x_continuous(breaks = seq(1, max(data2$book_number), 1),
                     labels = scales::ordinal_format()) + 
  labs(title = "No of Goodreads Ratings Index per Book Series per Books Published",
    x = "Book Number", y = "Series Name") +
  theme(axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        axis.title = element_text(size = 10))

#Save the plot
ggsave("goodreads_ratings_index_per_book_series.png", plot = gg, width = 8, height = 5, bg = "white")