#libraries.
library(dplyr)
library(rvest)
library(readr)

#load data
dat <-read_csv("C:\\Users\\ghilmanfat\\OneDrive - PT Telekomunikasi Selular\\Tsel work\\20. Device Segmentation\\device_list_null.csv")
#dat <- data.frame(device_name="SAMSUNG G532GDS")

dat <- dat %>% filter(cnt>1000)
#dat <- dat[1:20,]

# Function for getting website.
getWebsite <- function(name)
{
  url = URLencode(paste0("https://www.google.com/search?q=",name))
  
  page <- read_html(url)
  
  results <- page %>% 
    html_nodes("cite") %>% # Get all notes of type cite. You can change this to grab other node types.
    html_text()
  
  results <- results[grepl("gsmarena", results)]
  result <- results[1]
  
  print(paste(name, "->", result))
  return(as.character(result)) 
}

summary_device=list()
for( i in 1:nrow(dat)){
  possibleError = tryCatch({
    url = URLencode(paste0("https://www.google.com/search?q=",dat$device_name[i]))
    page <- read_html(url)
  },
  error = function(e) e)
  if(inherits(possibleError, "error")){
    print(paste(dat$device_name[i], "error"))
    Sys.sleep(10)
    next
  } 
  
  results <- page %>% 
    html_nodes("cite") %>% # Get all notes of type cite. You can change this to grab other node types.
    html_text()
  
  results <- results[grepl("gsmarena", results)]
  summary_device[[i]] <- data.frame(device_id=dat$device_name[i], websites=results[1])
  
  # crawl politely
  Sys.sleep(sample(seq(1, 7, by=2), 1))
  print(paste(i, "crawled device", dat$device_name[i], results[1]))
}

websites <-bind_rows(summary_device)
#websites <- data.frame(Website = sapply(dat$device_name,getWebsite))
websites$phone_name <- gsub("^.+gsmarena.com/(.+)-[0-9]+.php$", "\\1", websites$websites)
