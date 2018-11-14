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
    html_nodes("cite") %>% 
    html_text()
  
  results <- results[grepl("gsmarena", results)]
  result <- results[1]
  
  print(paste(name, "->", result))
  return(as.character(result)) 
}

summary_device = list()
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
    html_nodes("cite") %>% 
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
websites$cnt <- dat$cnt
sum(is.na(websites$websites))
sum(websites[is.na(websites$websites),]$cnt) #22322810

# crawl gsmarena from the link we scrape from google
summary_gsmarena = list()
for( i in 1:nrow(websites)){
  if(is.na(websites$websites[i])) next
  
  possibleError = tryCatch({
    url = URLencode(paste0(websites$websites[i]))
    page <- read_html(url)
  },
  error = function(e) e)
  if(inherits(possibleError, "error")){
    print(paste(websites$phone_name[i], "error"))
    Sys.sleep(10)
    next
  } 
  result = data.frame()
  for (scope in (page %>% html_nodes(css = "div#specs-list table"))){
    scope_nm = scope %>% html_nodes(xpath = ".//th") %>% html_text()
    scope_nm = scope_nm[1]
    for(ttl in (scope %>% html_nodes(xpath = ".//td[@class='ttl']"))){
      ttl_value = ttl %>% html_text()
      nfo_value = ttl %>% html_nodes(xpath = "following-sibling::td[@class='nfo']") %>% html_text()
      result = rbind(result, data.frame(phone_id= websites$device_id[i], specs = scope_nm, feature = ttl_value, detail = nfo_value))
    }
  }
  summary_gsmarena[[i]]= result
  
  # crawl politely
  Sys.sleep(sample(seq(1, 7, by=2), 1))
  print(paste(i, "crawled device", websites$phone_name[i]))
}

idx_null = which(sapply(summary_gsmarena, is.null))
for( i in idx_null){
  if(is.na(websites$websites[i])) next
  
  possibleError = tryCatch({
    url = URLencode(paste0(websites$websites[i]))
    page <- read_html(url)
  },
  error = function(e) e)
  if(inherits(possibleError, "error")){
    print(paste(i, websites$phone_name[i], "error"))
    Sys.sleep(10)
    next
  } 
  result = data.frame()
  for (scope in (page %>% html_nodes(css = "div#specs-list table"))){
    scope_nm = scope %>% html_nodes(xpath = ".//th") %>% html_text()
    scope_nm = scope_nm[1]
    for(ttl in (scope %>% html_nodes(xpath = ".//td[@class='ttl']"))){
      ttl_value = ttl %>% html_text()
      nfo_value = ttl %>% html_nodes(xpath = "following-sibling::td[@class='nfo']") %>% html_text()
      result = rbind(result, data.frame(phone_id= websites$device_id[i], specs = scope_nm, feature = ttl_value, detail = nfo_value))
    }
  }
  summary_gsmarena[[i]]= result
  
  # crawl politely
  Sys.sleep(sample(seq(1, 7, by=2), 1))
  print(paste(i, "crawled device", websites$phone_name[i]))
}

specs = bind_rows(summary_gsmarena)
write_csv(specs, "C:\\Users\\ghilmanfat\\OneDrive - PT Telekomunikasi Selular\\Tsel work\\20. Device Segmentation\\device_specs.csv")
length(unique(specs$phone_id))
