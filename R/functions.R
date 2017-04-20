# functions:

inul <- function(x){
        x <- if(is.null(x)){
                0
        }else{
                x
        }
        return(x)
}


library(jsonlite)
library(dplyr)
get_data <- function(congress, billrange, debug = F){
        # this function gets some of the data on congressional laws from the 
        # house of representatives from govtrack.us
        
        congress <- congress
        
        # making a list to store unstructured data
        house.l <- list()
        
        # creating a data frame to store structured data
        house.df <- as_data_frame(matrix(rep(NA, 21*max(billrange)), 
                                            ncol = 21))
        
        # forloop to collect data
        
        for (i in billrange){
                if (debug == T){
                  print(paste("congress: ", congress, "bill", i))
                }
                # getting URL to scrape based off of the congress, and bill number
                jsonfile <- paste("https://www.govtrack.us/data/congress/", congress,
                                  "/bills/hr/hr",i,"/data.json", sep = "")
                
                # storing data into a list
                house.l[[i]] <- jsonlite::fromJSON(jsonfile)
                
                #storing when it was introduced to the house
                temp <- house.l[[i]]$actions %>%
                        dplyr::select(acted_at, text) 
                
                introduced <- "Introduced in House"
                housecjud <- "Referred to the House Committee on the Judiciary."
                subcrime <- "Referred to the Subcommittee on Crime, Terrorism, Homeland Security, and Investigations."
                
                # introduced to house 
                house.df[i,1] <- inul(sum(ifelse(temp$text == introduced,1,0)) > 0)
                house.df[i,2] <- inul(house.l[[i]]$introduced_at)
                
                # Referred to the House Committee on the Judiciary.
                house.df[i,3] <- inul(sum(ifelse(temp$text == housecjud,1,0)) > 0)
                #house.df[i,4] <- temp[which(ifelse(temp$text == housecjud,T,F)),]$acted_at
                
                # Referred to the Subcommittee on Crime, Terrorism, 
                # Homeland Security, and Investigations.
                house.df[i,5] <- inul(sum(ifelse(temp$text == subcrime,1,0)) > 0)
                #house.df[i,6] <- temp[which(ifelse(temp$text == subcrime,T,F)),]$acted_at
                
                house.df[i,7] <- house.l[[i]]$subjects_top_term  %>%
                        inul()
                house.df[i,8] <- house.l[[i]]$status %>%
                        inul()
                house.df[i,9] <- house.l[[i]]$sponsor$title %>%
                        inul()
                house.df[i,10] <- house.l[[i]]$sponsor$state %>%
                        inul()
                house.df[i,11] <- house.l[[i]]$sponsor$name %>%
                        inul()
                house.df[i,12] <- house.l[[i]]$short_title %>%
                        inul()
                house.df[i,13] <- house.l[[i]]$introduced_at %>%
                        inul()
                house.df[i,14] <- house.l[[i]]$history$vetoed %>%
                        inul()
                house.df[i,15] <- house.l[[i]]$history$enacted %>%
                        inul()
                house.df[i,16] <- house.l[[i]]$history$awaiting_signature %>%
                        inul()
                house.df[i,17] <- house.l[[i]]$history$active %>%
                        inul()
                house.df[i,18] <- house.l[[i]]$bill_type %>%
                        inul()
                house.df[i,19] <- house.l[[i]]$history$house_passage_result %>%
                        inul()
                house.df[i,20] <- house.l[[i]]$number %>%
                        inul()
                house.df[i,21] <- house.l[[i]]$congress %>%
                        inul()
                
        }
        
        colnames(house.df) <- c("introhouse", "introhousedate", "refhousejud", "refhousejuddate",
                                   "subcomterror", "subcomterrordate", "subjects_top_term", "status",
                                   "sponsor_title", "sponsor_state", 
                                   "sponsor_name", "short_title", "introduced_at", "h_vetoed",
                                   "h_enacted", "h_awaitingsig", "h_active", "bill_type", 
                                   "h_house_passage_result", "number", "congress")
        
        return(house.df)
}


get_raw_data <- function(congress, billrange, debug = F){
  # this function gets some of the data on congressional laws from the 
  # house of representatives from govtrack.us
  
  congress <- congress
  
  # making a list to store unstructured data
  house.l <- list()
  
  # creating a data frame to store structured data
  house.df <- as_data_frame(matrix(rep(NA, 21*max(billrange)), 
                                   ncol = 21))
  
  # forloop to collect data
  
  for (i in billrange){
    if (debug == T){
      print(paste("congress: ", congress, "bill", i))
    }
    # getting URL to scrape based off of the congress, and bill number
    jsonfile <- paste("https://www.govtrack.us/data/congress/", congress,
                      "/bills/hr/hr",i,"/data.json", sep = "")
    
    # storing data into a list
    house.l[[i]] <- jsonlite::fromJSON(jsonfile)
    
    return(house.l)
  }
}
