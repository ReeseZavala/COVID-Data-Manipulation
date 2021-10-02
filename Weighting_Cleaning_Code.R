rm(list = ls())
library(readr)
library(tidyverse)


###   FUNCTION CODE   ###
Clean <- function(country, wd){
  
  
  ####################################################################
  ################      USER INPUT NECESSARY      ####################
  ####################################################################
                                                                     #
  setwd(wd)                                                          #
  ## Choose ONE: "Brazil" "Colombia" "Chile" "Mexico"                #
  WhichCountry<- country                                             #    
                                                                     #
  ####################################################################
  
  
  ##Universal variable for country data
  country <- read.csv(paste0(WhichCountry, ".csv"), header = T)
  ### Code to eliminate non-finishers 
  country <- country[2:nrow(country),]
  country <- country[which(country$DEM09 > 0 | country$DEM09 == -99),]
  
  
  ### Create vector of Weights for group ID
  #### First, hardcoding desired totals as calculated before survey
  if(WhichCountry == "Chile"){
    countryWT <- c(39, 77, 115, 88, 202, 72, 187, 195, 32, 40, 106, 88, 100, 176, 66, 158, 221, 38)
    }else if(WhichCountry == "Colombia"){
      countryWT <- c(24, 41, 112, 88, 141, 131, 160, 234, 71, 37, 55, 147, 72, 111, 109, 129, 254, 84)
    }else if(WhichCountry == "Brazil"){
      countryWT <- c(8, 65, 124, 40, 199, 129, 71, 298, 60, 20, 70, 137, 42, 175, 124, 53, 314, 71)
    }else if(WhichCountry == "Mexico"){
      countryWT <- c(25, 74, 138, 38, 193, 125, 91, 290, 54, 31, 73, 149, 51, 151, 82, 122, 264, 49)
    }
  
  
  ### Next, create these categories in dataset 
  country <- country[which(country$userSEL > 0),]
  for (i in 1:nrow(country)){
    country$userSEL[i] <- as.numeric(substr(country$userSEL[i], 1,1))
  }
  
  
  male <- ifelse (country$userGender==1, "M", "F")
  age  <- ifelse (country$userAge >= 56, "O"
                  , ifelse (country$userAge <= 35, "Y", "M"))
  status <- ifelse (country$userSEL==1, "L"
                    , ifelse (country$userSEL==2, "M", "H"))
  
  groupID <- paste0 (male, age, status)
  groupID <- factor(groupID, levels = c("FOH", "FOM", "FOL", "FMH", "FMM", "FML", "FYH", "FYM", "FYL",
                                        "MOH", "MOM", "MOL", "MMH", "MMM", "MML", "MYH", "MYM", "MYL")) 
  
  desired <- cbind(countryWT, c("FOH", "FOM", "FOL", "FMH", "FMM", "FML", "FYH", "FYM", "FYL",
                     "MOH", "MOM", "MOL", "MMH", "MMM", "MML", "MYH", "MYM", "MYL"))
  
  ## Corresponding weight to group ID
  surveyWT <- NULL
  for(i in 1:nrow(country)){
   #Temp holding desired number
   hold <- as.numeric(desired[which(desired[,2] == groupID[i]),1])
   #Dividing into n_groupID 
   surveyWT <- c(surveyWT, nrow(country[country$groupID == country$groupID[i],])/hold)
  }
  
  ## Adding weights and Group IDs to the data
  country <- country %>%
    mutate(groupID = groupID)%>%
    mutate(surveyWT = surveyWT)
  
  write.csv(country, paste0(WhichCountry, "_weighted.csv"))

}

##############################
####   RUNNING FUNCTION   ####
##############################


## Vector of countries of interest
countries <- c("Brazil", "Colombia", "Chile", "Mexico")

## For loop to write CSV for each country
for(i in countries){
Clean(i, "C:\\Users\\rzava\\Box Sync\\COVID survey\\Data\\Wave 1\\")
}

