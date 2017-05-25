library("rvest")
library("tidyverse")
library(stringr)
library(RSelenium)
'%|%' <- function(a,b) paste0(a,b)

#On terminal run
# docker run -d -p 5556:4444 selenium/standalone-chrome

#start the selenium session
browser <- remoteDriver(port = 5556, browser = "chrome")
browser$open()

years <- 2007:2017
type <- "bouldering"
baseURL <- "http://www.ifsc-climbing.org/index.php/world-competition/categories"

browser$setImplicitWaitTimeout(milliseconds = 5000)
browser$setTimeout(type = "page load", milliseconds = 10000)

baseURL <- "http://www.ifsc-climbing.org/index.php/world-competition/categories"

#---- define datasets----
ifsc <- data_frame(
  final_rank = numeric(0),
  last = character(0),
  first = character(0),
  nation = character(0),
  final = character(0),
  semifinal = character(0),
  qual1 = character(0),
  qual2 = character(0),
  comp_id = character(0),
  comp_city = character(0),
  comp_cnt = character(0),
  sex = character(0),
  year = integer(0)
)

rm("ifsc_heat")

failed <- tibble(yr = numeric(0), link = character(0))

years <- 2007:2017

for(yr in years){
  print("processing yr = " %|% yr)
  browser$navigate(baseURL %|% "#!year=" %|% yr %|% "&filter[cat_id]=69&filter[cup]=!")
  Sys.sleep(3)
  
  #Find all competitions
  comps <- browser$findElements(using = "css", ".cats li")
  
  #Take only bouldering competitions
  is_bld <- sapply(comps, function(x) x$getElementText()) %>% 
    unlist %>% str_detect("bouldering")
  
  #Grab the links to the individual bouldering competitions.
  #Some comps have links missing, which is dealt with by checking for the length of the returned list
  atags <- sapply(comps[is_bld], function(x) {
    atag <- x$findChildElements("css", "a")
    if(length(atag) > 0){
      atag[[1]]
    }else{
      list()
    }
  })
  
  #Take only links that are not missing
  exist <- sapply(atags, length) > 0
  hrefs <- sapply(atags[exist], function(x) x$getElementAttribute("href")) %>% unlist
  print("number of bouldering competitions found: " %|% length(hrefs))
  
  comp_id <- hrefs %>% str_match("comp=([0-9]+)&") %>% .[,2]
  cat <- hrefs %>% str_match("cat=([0-9])") %>% .[,2]
  sex <- ifelse(cat == "5", "women", "men")
  #i=10
  
  #Loop over non-missing bouldering competitions within a single year
  for(i in seq_along(hrefs)){
    print("    i = " %|% i)
    browser$navigate(hrefs[i])
    Sys.sleep(3)
    
    #Get information about the location of the competitions
    place_node <- browser$findElements(using = "css", ".compHeader")
    place_text <- place_node[[1]]$getElementText()[[1]]
    
    #Skip Asian Championships
    if(str_detect(place_text, "IFSC Asian Championship")) {
      print("Skipping IFSC Asian Championship...")
      next
    }
    
    #Grab the place and country
    place <- place_text %>% str_match("- (.*) \\(([A-Z]{3})\\)") %>% .[2:3]
    
    if(all(is.na(place))){
      failed <- failed %>% bind_rows(tibble(yr = yr, link = links[i]))
      print("failed")
      next
    }
    print("    place = " %|% place[1] %|% " - " %|% place[2] %|% " " %|% sex[i])
    
    #Grab the table with general results
    data_node <- browser$findElements(using = "css", ".DrTable")[[1]]
    
    data <- data_node$getElementAttribute("outerHTML")[[1]] %>% 
      read_html() %>% 
      html_table() %>% 
      .[[1]]
    
    if(identical(place, c("Vail", "USA")) && sex[i] == "men" && yr == "2008"){
      data <- data[,-5] #Erroneous column in the dataset. Remove
    }
    if(identical(place, c("Eindhoven", "NED")) && sex[i] == "men" && yr == "2010"){
      data <- data[,-5] #Erroneous column in the dataset. Remove
    }
    if(identical(place, c("Hall", "AUT")) && sex[i] == "women" && yr == "2009"){
      data <- data[,-which(colnames(data) == "S-Final")] #Erroneous column in the dataset. Remove
    }
    
    #Remove starting number because it's not always present
    if("StartNr" %in% colnames(data)){
      data <- data[,-which(colnames(data) == "StartNr")]
    }
    
    #if the number of columns is not 7 or 8, then I don't know how to handle it
    if(!ncol(data) %in% 7:8) {
      print(">>>>>>>")
      print("yr: " %|% yr %|% ", i = " %|% i %|% ", place = " %|% place[1] %|% " - " %|% place[2])
      print("datacols: " %|% ncol(data))
      print("<<<<<<<")
      next
    }
    
    if(ncol(data) == 7){
      colnames(data) <- c("final_rank", "last", "first", "nation", "final","semifinal","qual1")  
    }else if(ncol(data) == 8){
      colnames(data) <- c("final_rank", "last", "first", "nation", "final","semifinal","qual1","qual2")  
    }else{
      print("error: should not have got here")
      print("yr: " %|% yr %|% ", i = " %|% i %|% ", place = " %|% place[1] %|% " - " %|% place[2])
      print("datacols: " %|% ncol(data))
    }
    
    #Add general results to the data frame
    subdf <- as_tibble(data) %>%
      mutate(
        comp_id = comp_id[i],
        comp_city = place[1],
        comp_cnt = place[2],
        sex = sex[i],
        year = yr
      )
    
    ifsc <- ifsc %>% bind_rows(subdf)
    
    #Grab the links to finals, semifinals and quals
    link_tags <- browser$findElements("css", "#ifsc_calendar > ul.listToc > li > a")
    link_text <- link_tags %>% sapply(function(x) x$getElementText()[[1]])
    link_href <- link_tags %>% sapply(function(x) x$getElementAttribute("href")[[1]])
    print("    Found " %|% length(link_href) %|% " stages")
    
    #loop over heats
    for(j in seq_along(link_href)){
      print("   -> going to " %|% link_text[j])
      
      if(identical(place, c("Vail", "USA")) && sex[i] == "men" && j == 1 && yr == "2008"){
        next #skip this erroneous empty dataset
      }
      if(identical(place, c("Eindhoven", "NED")) && sex[i] == "men" && j == 1 && yr == "2010"){
        next #skip this erroneous empty dataset
      }
      
      browser$navigate(link_href[j])
      Sys.sleep(3)
      
      data_node <- browser$findElements(using = "css", ".DrTable")[[1]]
      
      data <- data_node$getElementAttribute("outerHTML")[[1]] %>% 
        read_html() %>% 
        html_table() %>% 
        .[[1]]  
      
      #The following monstrosity is to parse out the number of attempts
      #For each bonus and each top
      boulderdiv <- browser$findElements("css", "td.boulder > div")
      bouldertext <- boulderdiv %>% sapply(function(x) {
        x$findChildElements("css", "div") %>% sapply(function(y) y$getElementAttribute("class")[[1]])})
      
      bouldertext <- bouldertext %>% lapply(function(x) x[str_detect(x, "boulder")]) 
      
      #Encode the values and collapse into a single string.
      #Some of the 0s will become NAs but that's an easy replace later
      seq <- sapply(bouldertext, function(x) {
        paste(c("boulderTop" = "t", "boulderBonus" = "b", "boulderNone" = "0")[x], collapse="")
      })
      
      if("StartNr" %in% colnames(data)){
        data <- data[,-which(colnames(data) == "StartNr")]
      }
      
      #If the previous heat result is not given, then still add it to match
      #the columns with rest of the datsets
      if(ncol(data) == 6){
        data$prev.heat <- NA
      }
      
      data$seq <- seq
      data$comp_id <- comp_id[i]
      data$sex <- sex[i]
      data$year <- yr
      data$heat <- link_text[j]
      
      colnames(data) <- c("rank", "last", "first", "nation", "result", "sum", 
                          "prev.heat", "seq", "comp_id", "sex", "year", "heat")
      
      if(!exists("ifsc_heat")){
        ifsc_heat <- as_data_frame(data)
      }else{
        ifsc_heat <- bind_rows(ifsc_heat, as_data_frame(data))
      }
    }
  }
}

#saveRDS(ifsc, "dat/ifsc_all.RDs")
#saveRDS(ifsc_heat, "dat/ifsc_heat_all.RDs")


#---- KNOWN ERRORS: ----
# 1) Moscow 2010 men, qual1 and qual2 are identical, but on IFSC website 
#    they are not identical
# 

# ---- Get world cup results for each year ----
browser <- remoteDriver(port = 5556, browser = "chrome")
browser$open()
browser$setImplicitWaitTimeout(milliseconds = 5000)
browser$setTimeout(type = "page load", milliseconds = 10000)

years <- str_sub(1999:2017, 3, 4)
sex <- c("M", "F")
rm("rankings")

for(yr in years) {
  for (s in sex) {
    cat("\nYear: 20" %|% yr %|% ", " %|% s %|% "\n")
    URL   <-
      "http://www.ifsc-climbing.org/index.php/world-competition#!type=ranking&cat=ICC_" %|%
      s %|% "B&cup=" %|% yr %|% "_WC"
    
    cat("Navigating...")
    browser$navigate(URL)
    cat("Done\n")
    
    cat("Sleeping 3 seconds...")
    Sys.sleep(3)
    cat("Done\n")
    
    cat("Grabbing the data...")
    data_node <-
      browser$findElements(using = "css", ".DrTable")[[1]]
    
    data <- data_node$getElementAttribute("outerHTML")[[1]] %>%
      read_html() %>%
      html_table() %>%
      .[[1]]
    
    data$sex <- switch(s, M = "men", F = "women")
    data$year <- as.numeric("20" %|% yr)
    colnames(data) <-
      c("rank", "last", "first", "nation", "points", "sex", "year")
    cat("Done\n")
    
    cat("Saving data...")
    if (!exists("rankings")) {
      rankings <- as_data_frame(data)
    } else{
      rankings <- bind_rows(rankings, as_data_frame(data))
    }
    
    cat("Done\n")
    cat("All done\n")
  }
}

rankings <- rankings %>% 
  mutate(name = str_c(first, last, sep = " "))
rankings <- rankings %>% mutate(year = ifelse(year == 2099, 1999, year))
#saveRDS(rankings, "dat/ifsc_rankings-until_Hachioji.RDs")

