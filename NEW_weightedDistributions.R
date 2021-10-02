rm(list = ls())
library(foreign)
library(haven)
#####################
####   REESE     ####
#####################

setwd("C:/Users/rzava/Box Sync/COVID survey/")



#####################
####  GUILLERMO  ####
#####################

#setwd ("~/Dropbox/CovidMcDonnellGrant/qualtricsSurveys/")







### Implied Netquest SEL based exclusively on education
# points	years	reconverted points	
# 0	0	0	E
# 0	0	0	E
# 10	5	19.8019802	E
# 22	6	43.56435644	E
# 23	8	45.54455446	E
# 31	9	61.38613861	D
# 35	11	69.30693069	D
# 43	12	85.14851485	D
# 59	15	116.8316832	C-
# 73	16	144.5544554	C 
# 101	20	200	C+/AB

## Brazil with weights
## The scale here is so bad, so it seemed unreasonable to find points values
## corresponding to each year.  However, these are the cutoffs:

# points      years     AdjPoints     Grade  SEL
#   1           7        14.285        D/E   LOW
#   2	          11	     28.571	       C1    MID
#   4	          12	     31.168        B2    HIGH
#   4	          14	     57.142      	 A      |
#   7	          20	     100	         A      V

## Clearly, this scale is far less linear than Mexico's, but these cutoffs look correct

# For Colombia: 11 years and 12 == HS grad

## Now, run the function to get our distributions.  Each country is hard-coded with the years-of-education cutoff
#  which are in accordance with the breakdowns above.  Chile follows Mexico's breakdown, and as does Colombia  




disDist <- function(country){
  if (country == 'Brazil'|country == 'brazil'){
    MX_dta <- read_dta("Brazil LAPOP AmericasBarometer 2019 v1.0_W.dta")
    MX_dta[is.na(MX_dta)] <- 0
    #Gender Marginal Male
    Mal <- sum(MX_dta$sex == 1)/nrow(MX_dta)
    #Gender/Age Joint Male
    MO <-  sum(MX_dta$sex == 1 & MX_dta$q2 >= 55)/nrow(MX_dta)
    MM <-  sum(MX_dta$sex == 1 & MX_dta$q2 > 35 & MX_dta$q2 < 55)/nrow(MX_dta)
    MY <-  sum(MX_dta$sex == 1 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18)/nrow(MX_dta)
    #Gender/Age/Edu joint Male
    MOH <-  sum(MX_dta$sex == 1 & MX_dta$q2 >= 55 & MX_dta$ed > 11)/nrow(MX_dta)
    MOM <-  sum(MX_dta$sex == 1 & MX_dta$q2 >= 55 & (MX_dta$ed >= 8 & MX_dta$ed <= 11))/nrow(MX_dta)
    MOL <-  sum(MX_dta$sex == 1 & MX_dta$q2 >= 55 & MX_dta$ed < 8)/nrow(MX_dta)
    MMH <-  sum(MX_dta$sex == 1 & MX_dta$q2 > 35 & MX_dta$q2 < 55 & MX_dta$ed > 11)/nrow(MX_dta)
    MMM <-  sum(MX_dta$sex == 1 & MX_dta$q2 > 35 & MX_dta$q2 < 55 & (MX_dta$ed >= 8 & MX_dta$ed <= 11))/nrow(MX_dta)
    MML <-  sum(MX_dta$sex == 1 & MX_dta$q2 > 35 & MX_dta$q2 < 55 & MX_dta$ed < 8)/nrow(MX_dta)
    MYH <-  sum(MX_dta$sex == 1 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18 & MX_dta$ed > 11)/nrow(MX_dta)
    MYM <-  sum(MX_dta$sex == 1 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18 & (MX_dta$ed >= 8 & MX_dta$ed <= 11))/nrow(MX_dta)
    MYL <-  sum(MX_dta$sex == 1 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18 & MX_dta$ed < 8)/nrow(MX_dta)
    
    #Gender Marginal Female
    Fem <- sum(MX_dta$sex == 2)/nrow(MX_dta)
    #Gender/Age Joint Female
    FO <-  sum(MX_dta$sex == 2 & MX_dta$q2 >= 55)/nrow(MX_dta)
    FM <-  sum(MX_dta$sex == 2 & MX_dta$q2 > 35 & MX_dta$q2 < 55)/nrow(MX_dta)
    FY <-  sum(MX_dta$sex == 2 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18)/nrow(MX_dta)
    #Gender/Age/Edu joint Female
    FOH <-  sum(MX_dta$sex == 2 & MX_dta$q2 >= 55 & MX_dta$ed > 11)/nrow(MX_dta)
    FOM <-  sum(MX_dta$sex == 2 & MX_dta$q2 >= 55 & (MX_dta$ed >= 8 & MX_dta$ed <= 11))/nrow(MX_dta)
    FOL <-  sum(MX_dta$sex == 2 & MX_dta$q2 >= 55 & MX_dta$ed < 8)/nrow(MX_dta)
    FMH <-  sum(MX_dta$sex == 2 & MX_dta$q2 > 35 & MX_dta$q2 < 55 & MX_dta$ed > 11)/nrow(MX_dta)
    FMM <-  sum(MX_dta$sex == 2 & MX_dta$q2 > 35 & MX_dta$q2 < 55 & (MX_dta$ed >= 8 & MX_dta$ed <= 11))/nrow(MX_dta)
    FML <-  sum(MX_dta$sex == 2 & MX_dta$q2 > 35 & MX_dta$q2 < 55 & MX_dta$ed < 8)/nrow(MX_dta)
    FYH <-  sum(MX_dta$sex == 2 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18 & MX_dta$ed > 11)/nrow(MX_dta)
    FYM <-  sum(MX_dta$sex == 2 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18 & (MX_dta$ed >= 8 & MX_dta$ed <= 11))/nrow(MX_dta)
    FYL <-  sum(MX_dta$sex == 2 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18 & MX_dta$ed < 8)/nrow(MX_dta)
    
    End <- rbind(cbind(MOH, MOM, MOL, MMH, MMM, MML, MYH, MYM, MYL), cbind(FOH, FOM, FOL, FMH, FMM, FML, FYH, FYM, FYL))
    colnames(End) <- c("OH", "OM", "OL", "MH", "MM", "ML", "YH", "YM", "YL")
    rownames(End) <- c("M", "F")
    print(End)
  }
  else if (country == 'Mexico'|country == 'mexico'){
    MX_dta <- read_dta("Mexico LAPOP AmericasBarometer 2019 v1.0_W.dta")
    MX_dta[is.na(MX_dta)] <- 0
    #Gender Marginal Male
    Mal <- sum(MX_dta$sex == 1)/nrow(MX_dta)
    #Gender/Age Joint Male
    MO <-  sum(MX_dta$sex == 1 & MX_dta$q2 >= 55)/nrow(MX_dta)
    MM <-  sum(MX_dta$sex == 1 & MX_dta$q2 > 35 & MX_dta$q2 < 55)/nrow(MX_dta)
    MY <-  sum(MX_dta$sex == 1 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18)/nrow(MX_dta)
    #Gender/Age/Edu joint Male
    MOH <-  sum(MX_dta$sex == 1 & MX_dta$q2 >= 55 & MX_dta$ed > 12)/nrow(MX_dta)
    MOM <-  sum(MX_dta$sex == 1 & MX_dta$q2 >= 55 & (MX_dta$ed >= 9 & MX_dta$ed <= 12))/nrow(MX_dta)
    MOL <-  sum(MX_dta$sex == 1 & MX_dta$q2 >= 55 & MX_dta$ed < 8)/nrow(MX_dta)
    MMH <-  sum(MX_dta$sex == 1 & MX_dta$q2 > 35 & MX_dta$q2 < 55 & MX_dta$ed > 12)/nrow(MX_dta)
    MMM <-  sum(MX_dta$sex == 1 & MX_dta$q2 > 35 & MX_dta$q2 < 55 & (MX_dta$ed >= 9 & MX_dta$ed <= 12))/nrow(MX_dta)
    MML <-  sum(MX_dta$sex == 1 & MX_dta$q2 > 35 & MX_dta$q2 < 55 & MX_dta$ed < 9)/nrow(MX_dta)
    MYH <-  sum(MX_dta$sex == 1 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18 & MX_dta$ed > 12)/nrow(MX_dta)
    MYM <-  sum(MX_dta$sex == 1 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18 & (MX_dta$ed >= 9 & MX_dta$ed <= 12))/nrow(MX_dta)
    MYL <-  sum(MX_dta$sex == 1 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18 & MX_dta$ed < 9)/nrow(MX_dta)
    
    #Gender Marginal Female
    Fem <- sum(MX_dta$sex == 2)/nrow(MX_dta)
    #Gender/Age Joint Female
    FO <-  sum(MX_dta$sex == 2 & MX_dta$q2 >= 55)/nrow(MX_dta)
    FM <-  sum(MX_dta$sex == 2 & MX_dta$q2 > 35 & MX_dta$q2 < 55)/nrow(MX_dta)
    FY <-  sum(MX_dta$sex == 2 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18)/nrow(MX_dta)
    #Gender/Age/Edu joint Female
    FOH <-  sum(MX_dta$sex == 2 & MX_dta$q2 >= 55 & MX_dta$ed > 12)/nrow(MX_dta)
    FOM <-  sum(MX_dta$sex == 2 & MX_dta$q2 >= 55 & (MX_dta$ed >= 9 & MX_dta$ed <= 12))/nrow(MX_dta)
    FOL <-  sum(MX_dta$sex == 2 & MX_dta$q2 >= 55 & MX_dta$ed < 9)/nrow(MX_dta)
    FMH <-  sum(MX_dta$sex == 2 & MX_dta$q2 > 35 & MX_dta$q2 < 55 & MX_dta$ed > 12)/nrow(MX_dta)
    FMM <-  sum(MX_dta$sex == 2 & MX_dta$q2 > 35 & MX_dta$q2 < 55 & (MX_dta$ed >= 9 & MX_dta$ed <= 12))/nrow(MX_dta)
    FML <-  sum(MX_dta$sex == 2 & MX_dta$q2 > 35 & MX_dta$q2 < 55 & MX_dta$ed < 9)/nrow(MX_dta)
    FYH <-  sum(MX_dta$sex == 2 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18 & MX_dta$ed > 12)/nrow(MX_dta)
    FYM <-  sum(MX_dta$sex == 2 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18 & (MX_dta$ed >= 9 & MX_dta$ed <= 12))/nrow(MX_dta)
    FYL <-  sum(MX_dta$sex == 2 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18 & MX_dta$ed < 9)/nrow(MX_dta)
    
    End <- rbind(cbind(MOH, MOM, MOL, MMH, MMM, MML, MYH, MYM, MYL), cbind(FOH, FOM, FOL, FMH, FMM, FML, FYH, FYM, FYL))
    colnames(End) <- c("OH", "OM", "OL", "MH", "MM", "ML", "YH", "YM", "YL")
    rownames(End) <- c("M", "F")
    print(End)
  }
  else if(country == 'Chile'|country == 'chile'){
    MX_dta <- read_dta("Chile LAPOP AmericasBarometer 2019 v1.0_W.dta")
    MX_dta[is.na(MX_dta)] <- 0
    #Gender Marginal Male
    Mal <- sum(MX_dta$sex == 1)/nrow(MX_dta)
    #Gender/Age Joint Male
    MO <-  sum(MX_dta$sex == 1 & MX_dta$q2 >= 55)/nrow(MX_dta)
    MM <-  sum(MX_dta$sex == 1 & MX_dta$q2 > 35 & MX_dta$q2 < 55)/nrow(MX_dta)
    MY <-  sum(MX_dta$sex == 1 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18)/nrow(MX_dta)
    #Gender/Age/Edu joint Male
    MOH <-  sum(MX_dta$sex == 1 & MX_dta$q2 >= 55 & MX_dta$ed > 12)/nrow(MX_dta)
    MOM <-  sum(MX_dta$sex == 1 & MX_dta$q2 >= 55 & (MX_dta$ed >= 9 & MX_dta$ed <= 12))/nrow(MX_dta)
    MOL <-  sum(MX_dta$sex == 1 & MX_dta$q2 >= 55 & MX_dta$ed < 9)/nrow(MX_dta)
    MMH <-  sum(MX_dta$sex == 1 & MX_dta$q2 > 35 & MX_dta$q2 < 55 & MX_dta$ed > 12)/nrow(MX_dta)
    MMM <-  sum(MX_dta$sex == 1 & MX_dta$q2 > 35 & MX_dta$q2 < 55 & (MX_dta$ed >= 9 & MX_dta$ed <= 12))/nrow(MX_dta)
    MML <-  sum(MX_dta$sex == 1 & MX_dta$q2 > 35 & MX_dta$q2 < 55 & MX_dta$ed < 9)/nrow(MX_dta)
    MYH <-  sum(MX_dta$sex == 1 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18 & MX_dta$ed > 12)/nrow(MX_dta)
    MYM <-  sum(MX_dta$sex == 1 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18 & (MX_dta$ed >= 9 & MX_dta$ed <= 12))/nrow(MX_dta)
    MYL <-  sum(MX_dta$sex == 1 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18 & MX_dta$ed < 9)/nrow(MX_dta)
    
    #Gender Marginal Female
    Fem <- sum(MX_dta$sex == 2)/nrow(MX_dta)
    #Gender/Age Joint Female
    FO <-  sum(MX_dta$sex == 2 & MX_dta$q2 >= 55)/nrow(MX_dta)
    FM <-  sum(MX_dta$sex == 2 & MX_dta$q2 > 35 & MX_dta$q2 < 55)/nrow(MX_dta)
    FY <-  sum(MX_dta$sex == 2 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18)/nrow(MX_dta)
    #Gender/Age/Edu joint Female
    FOH <-  sum(MX_dta$sex == 2 & MX_dta$q2 >= 55 & MX_dta$ed > 12)/nrow(MX_dta)
    FOM <-  sum(MX_dta$sex == 2 & MX_dta$q2 >= 55 & (MX_dta$ed >= 9 & MX_dta$ed <= 12))/nrow(MX_dta)
    FOL <-  sum(MX_dta$sex == 2 & MX_dta$q2 >= 55 & MX_dta$ed < 9)/nrow(MX_dta)
    FMH <-  sum(MX_dta$sex == 2 & MX_dta$q2 > 35 & MX_dta$q2 < 55 & MX_dta$ed > 12)/nrow(MX_dta)
    FMM <-  sum(MX_dta$sex == 2 & MX_dta$q2 > 35 & MX_dta$q2 < 55 & (MX_dta$ed >= 9 & MX_dta$ed <= 12))/nrow(MX_dta)
    FML <-  sum(MX_dta$sex == 2 & MX_dta$q2 > 35 & MX_dta$q2 < 55 & MX_dta$ed < 9)/nrow(MX_dta)
    FYH <-  sum(MX_dta$sex == 2 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18 & MX_dta$ed > 12)/nrow(MX_dta)
    FYM <-  sum(MX_dta$sex == 2 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18 & (MX_dta$ed >= 9 & MX_dta$ed <= 12))/nrow(MX_dta)
    FYL <-  sum(MX_dta$sex == 2 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18 & MX_dta$ed < 9)/nrow(MX_dta)
    
    End <- rbind(cbind(MOH, MOM, MOL, MMH, MMM, MML, MYH, MYM, MYL), cbind(FOH, FOM, FOL, FMH, FMM, FML, FYH, FYM, FYL))
    colnames(End) <- c("OH", "OM", "OL", "MH", "MM", "ML", "YH", "YM", "YL")
    rownames(End) <- c("M", "F")
    print(End)
  }
  else if (country == 'Colombia'|country == 'colombia'){
    MX_dta <- read_dta("Colombia LAPOP AmericasBarometer 2018 v1.0_W.dta")
    MX_dta[is.na(MX_dta)] <- 0
    #Gender Marginal Male
    Mal <- sum(MX_dta$sex == 1)/nrow(MX_dta)
    #Gender/Age Joint Male
    MO <-  sum(MX_dta$sex == 1 & MX_dta$q2 >= 55)/nrow(MX_dta)
    MM <-  sum(MX_dta$sex == 1 & MX_dta$q2 > 35 & MX_dta$q2 < 55)/nrow(MX_dta)
    MY <-  sum(MX_dta$sex == 1 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18)/nrow(MX_dta)
    #Gender/Age/Edu joint Male
    MOH <-  sum(MX_dta$sex == 1 & MX_dta$q2 >= 55 & MX_dta$ed > 11)/nrow(MX_dta)
    MOM <-  sum(MX_dta$sex == 1 & MX_dta$q2 >= 55 & (MX_dta$ed >= 9 & MX_dta$ed <= 11))/nrow(MX_dta)
    MOL <-  sum(MX_dta$sex == 1 & MX_dta$q2 >= 55 & MX_dta$ed < 9)/nrow(MX_dta)
    MMH <-  sum(MX_dta$sex == 1 & MX_dta$q2 > 35 & MX_dta$q2 < 55 & MX_dta$ed > 11)/nrow(MX_dta)
    MMM <-  sum(MX_dta$sex == 1 & MX_dta$q2 > 35 & MX_dta$q2 < 55 & (MX_dta$ed >= 9 & MX_dta$ed <= 11))/nrow(MX_dta)
    MML <-  sum(MX_dta$sex == 1 & MX_dta$q2 > 35 & MX_dta$q2 < 55 & MX_dta$ed < 9)/nrow(MX_dta)
    MYH <-  sum(MX_dta$sex == 1 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18 & MX_dta$ed > 11)/nrow(MX_dta)
    MYM <-  sum(MX_dta$sex == 1 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18 & (MX_dta$ed >= 9 & MX_dta$ed <= 11))/nrow(MX_dta)
    MYL <-  sum(MX_dta$sex == 1 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18 & MX_dta$ed < 9)/nrow(MX_dta)
    
    #Gender Marginal Female
    Fem <- sum(MX_dta$sex == 2)/nrow(MX_dta)
    #Gender/Age Joint Female
    FO <-  sum(MX_dta$sex == 2 & MX_dta$q2 >= 55)/nrow(MX_dta)
    FM <-  sum(MX_dta$sex == 2 & MX_dta$q2 > 35 & MX_dta$q2 < 55)/nrow(MX_dta)
    FY <-  sum(MX_dta$sex == 2 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18)/nrow(MX_dta)
    #Gender/Age/Edu joint Female
    FOH <-  sum(MX_dta$sex == 2 & MX_dta$q2 >= 55 & MX_dta$ed > 11)/nrow(MX_dta)
    FOM <-  sum(MX_dta$sex == 2 & MX_dta$q2 >= 55 & (MX_dta$ed >= 9 & MX_dta$ed <= 11))/nrow(MX_dta)
    FOL <-  sum(MX_dta$sex == 2 & MX_dta$q2 >= 55 & MX_dta$ed < 9)/nrow(MX_dta)
    FMH <-  sum(MX_dta$sex == 2 & MX_dta$q2 > 35 & MX_dta$q2 < 55 & MX_dta$ed > 11)/nrow(MX_dta)
    FMM <-  sum(MX_dta$sex == 2 & MX_dta$q2 > 35 & MX_dta$q2 < 55 & (MX_dta$ed >= 9 & MX_dta$ed <= 11))/nrow(MX_dta)
    FML <-  sum(MX_dta$sex == 2 & MX_dta$q2 > 35 & MX_dta$q2 < 55 & MX_dta$ed < 9)/nrow(MX_dta)
    FYH <-  sum(MX_dta$sex == 2 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18 & MX_dta$ed > 11)/nrow(MX_dta)
    FYM <-  sum(MX_dta$sex == 2 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18 & (MX_dta$ed >= 9 & MX_dta$ed <= 11))/nrow(MX_dta)
    FYL <-  sum(MX_dta$sex == 2 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18 & MX_dta$ed < 9)/nrow(MX_dta)
    
    End <- rbind(cbind(MOH, MOM, MOL, MMH, MMM, MML, MYH, MYM, MYL), cbind(FOH, FOM, FOL, FMH, FMM, FML, FYH, FYM, FYL))
    colnames(End) <- c("OH", "OM", "OL", "MH", "MM", "ML", "YH", "YM", "YL")
    rownames(End) <- c("M", "F")
    print(End)  
    
  }
}


MX_dta <- read_dta("Brazil LAPOP AmericasBarometer 2019 v1.0_W.dta")
unique(MX_dta$)
