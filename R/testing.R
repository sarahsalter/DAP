library(jsonlite)
library(dplyr)


search_querry <- "https://www.govtrack.us/congress/bills/browse?text=health+care#sort=relevance"

jsonfile <- "https://www.govtrack.us/data/congress/113/bills/hr/hr4014/data.json"

mydata <- jsonlite::fromJSON(jsonfile)
mydata

# search_querry <- "https://www.govtrack.us/congress/bills/browse?text=health+care#sort=relevance"

# veterans healthcare
library(jsonlite)
library(dplyr)

no_cols <- 21

house_vet_healt113 <- 1:5893
house_vet_healt114 <- 2:6536
house_vet_healt115 <- 19:2003 # bills from 115th congress

# EXAMPLE
# https://www.govtrack.us/data/congress/113/bills/hr/hr50/data.json

introduced <- "Introduced in House"
housecjud <- "Referred to the House Committee on the Judiciary."
subcrime <- "Referred to the Subcommittee on Crime, Terrorism, Homeland Security, and Investigations."

congress <- 114
house115.l <- list()
house115.df <- as_data_frame(matrix(rep(NA, no_cols*(max(house_vet_healt114))), 
                      ncol = no_cols))

for (i in house_vet_healt114){
        print(i)
        jsonfile <- paste("https://www.govtrack.us/data/congress/", congress,
        "/bills/hr/hr",i,"/data.json", sep = "")
        
        house115.l[[i]] <- jsonlite::fromJSON(jsonfile)
        
        #storing when it was introduced to the house
        temp <- house115.l[[i]]$actions %>%
                dplyr::select(acted_at, text) 
        
        # introduced to house 
        house115.df[i,1] <- sum(ifelse(temp$text == introduced,1,0)) > 0 %>%
                inul()
        house115.df[i,2] <- temp[which(ifelse(temp$text == introduced,T,F)),]$acted_at %>%
                inul()
        

        # Referred to the House Committee on the Judiciary.
        house115.df[i,3] <- sum(ifelse(temp$text == housecjud,1,0)) > 0 
        #house115.df[i,4] <- temp[which(ifelse(temp$text == housecjud,T,F)),]$acted_at
        
        # Referred to the Subcommittee on Crime, Terrorism, 
        # Homeland Security, and Investigations.
        house115.df[i,5] <- sum(ifelse(temp$text == subcrime,1,0)) > 0
        #house115.df[i,6] <- temp[which(ifelse(temp$text == subcrime,T,F)),]$acted_at
        
        house115.df[i,7] <- house115.l[[i]]$subjects_top_term  %>%
                inul()
        house115.df[i,8] <- house115.l[[i]]$status  %>%
                inul()
        house115.df[i,9] <- house115.l[[i]]$sponsor$title  %>%
                inul()
        house115.df[i,10] <- house115.l[[i]]$sponsor$state  %>%
                inul()
        house115.df[i,11] <- house115.l[[i]]$sponsor$name  %>%
                inul()
        
        house115.df[i,12] <- house115.l[[i]]$short_title  %>%
                inul()
        
        house115.df[i,13] <- house115.l[[i]]$introduced_at  %>%
                inul()
        house115.df[i,14] <- house115.l[[i]]$history$vetoed %>%
                inul()
        house115.df[i,15] <- house115.l[[i]]$history$enacted %>%
                inul()
        house115.df[i,16] <- house115.l[[i]]$history$awaiting_signature %>%
                inul()
        house115.df[i,17] <- house115.l[[i]]$history$active %>%
                inul()
        house115.df[i,18] <- house115.l[[i]]$bill_type %>%
                inul()
        house115.df[i,19] <- house115.l[[i]]$history$house_passage_result %>%
                inul()

        house115.df[i,20] <- house115.l[[i]]$number %>%
                inul()
        house115.df[i,21] <- house115.l[[i]]$congress %>%
                inul()
        
}

colnames(house115.df) <- c("introhouse", "introhousedate", "refhousejud", "refhousejuddate",
                           "subcomterror", "subcomterrordate", "subjects_top_term", "status",
                           "sponsor_title", "sponsor_state", 
                           "sponsor_name", "short_title", "introduced_at", "h_vetoed",
                           "h_enacted", "h_awaitingsig", "h_active", "bill_type", 
                           "h_house_passage_result", "number", "congress")



