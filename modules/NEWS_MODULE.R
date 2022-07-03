

boeWebConnectr::boe_web_config()

################### Import Core RSS feeds ################### 
# myFT feed
feedFT <- feed.extract("https://www.ft.com/myft/following/96504782-e3c6-497d-a29d-08596a8e964d.rss")


# Extract topic tags from the FT feed
feedFT <- feedFT$items
feedFT <- feedFT %>% dplyr::mutate(topic = gsub('^.*href\\f*|\\f*"</a>.*$', '', description))
feedFT <- feedFT %>% dplyr::mutate(topic = gsub('^.*">\\s*|\\s*</a>.*$', '', topic))
feedFT <- feedFT %>% dplyr::mutate(description = gsub(pattern = "width=900\">",replacement = "width=200\"><br>",description))
feedFT <- feedFT %>% dplyr::filter(grepl(pattern = "emerging markets", x = feedFT$topic, ignore.case = T))
# Clean and aggregate the extracted data frames
feedFT$hash <- NULL

feedFT <- feedFT %>% 
   dplyr::mutate(title = paste0("<a href='", link,"' target='_blank'>", title,"</a>")) %>% 
   dplyr::mutate(description = stringr::str_replace(string = description, pattern = ".*<br>", replacement = ""), 
                 description = stringr::str_replace(string = description, pattern = "<br />", replacement = "")) %>% 
   dplyr::mutate(date = substr(date,1,10)) %>% 
   dplyr::mutate(month = format(as.Date(date), "%b-%y")) %>% 
   dplyr::select("month",  dplyr::everything()) %>% 
   dplyr::arrange(desc(date))

feedFT$link  <- paste0("<a href='",feedFT$link,"'>",feedFT$link,"</a>")

# google news -------------------------------------------------------------

feedEM <- feed.extract("https://news.google.com/rss/search?q=emerging%20market%20&hl=en-GB&gl=GB&ceid=GB%3Aen")

feedEM <- feedEM$items %>%
   dplyr::mutate(title = paste0("<a href='", link,"' target='_blank'>", title,"</a>")) %>%
   dplyr::mutate(description = gsub(".*</a>","",description)) %>%
   dplyr::mutate(description = gsub("&nbsp;&nbsp;<font color=\"#6f6f6f\">","",description)) %>%
   dplyr::mutate(description = gsub("</font>","",description))  %>%
   dplyr::mutate(date = substr(date,1,10)) %>%
   dplyr::mutate(title = gsub("(.*) - .*", "\\1</a>", title)) %>%
   dplyr::mutate(month = format(as.Date(date), "%b-%y"))  %>%
   dplyr::select("month",  dplyr::everything()) %>%
   dplyr::arrange(desc(date))

feedEM$link  <- paste0("<a href='",feedEM$link,"'>",feedEM$link,"</a>")

boeWebConnectr::unset_boe_web_config()



