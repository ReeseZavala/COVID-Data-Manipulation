library(tidyverse)
library(foreign)
library(haven)
#MX_dta <- read_dta("C:/Users/rzava/Box Sync/COVID survey/Mexico LAPOP AmericasBarometer 2019 v1.0_W.dta")
#MX_dta <- read_dta("C:/Users/rzava/Box Sync/COVID survey/Colombia LAPOP AmericasBarometer 2018 v1.0_W.dta")
#MX_dta <- read_dta("C:/Users/rzava/Box Sync/COVID survey/Chile LAPOP AmericasBarometer 2019 v1.0_W.dta")
MX_dta <- read_dta("C:/Users/rzava/Box Sync/COVID survey/Brazil LAPOP AmericasBarometer 2019 v1.0_W.dta")
#head(MX_dta)
#colnames(MX_dta)
MX_dta[is.na(MX_dta)] <- 0

age <- "q2"
Education <- "ed"
Gender <- "sex"


#Gender Marginal Male
Mal <- sum(MX_dta$sex == 1)/nrow(MX_dta)
#Gender/Age Joint Male
MO <-  sum(MX_dta$sex == 1 & MX_dta$q2 >= 55)/nrow(MX_dta)
MM <-  sum(MX_dta$sex == 1 & MX_dta$q2 > 35 & MX_dta$q2 < 55)/nrow(MX_dta)
MY <-  sum(MX_dta$sex == 1 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18)/nrow(MX_dta)
#Gender/Age/Edu joint Male
MOH <-  sum(MX_dta$sex == 1 & MX_dta$q2 >= 55 & MX_dta$ed > 12)/nrow(MX_dta)
MOM <-  sum(MX_dta$sex == 1 & MX_dta$q2 >= 55 & (MX_dta$ed == 11 |MX_dta$ed == 12))/nrow(MX_dta)
MOL <-  sum(MX_dta$sex == 1 & MX_dta$q2 >= 55 & MX_dta$ed < 11)/nrow(MX_dta)
MMH <-  sum(MX_dta$sex == 1 & MX_dta$q2 > 35 & MX_dta$q2 < 55 & MX_dta$ed > 12)/nrow(MX_dta)
MMM <-  sum(MX_dta$sex == 1 & MX_dta$q2 > 35 & MX_dta$q2 < 55 & (MX_dta$ed == 11 |MX_dta$ed == 12))/nrow(MX_dta)
MML <-  sum(MX_dta$sex == 1 & MX_dta$q2 > 35 & MX_dta$q2 < 55 & MX_dta$ed < 11)/nrow(MX_dta)
MYH <-  sum(MX_dta$sex == 1 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18 & MX_dta$ed > 12)/nrow(MX_dta)
MYM <-  sum(MX_dta$sex == 1 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18 & (MX_dta$ed == 11 |MX_dta$ed == 12))/nrow(MX_dta)
MYL <-  sum(MX_dta$sex == 1 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18 & MX_dta$ed < 11)/nrow(MX_dta)

#Gender Marginal Female
Fem <- sum(MX_dta$sex == 2)/nrow(MX_dta)
#Gender/Age Joint Female
FO <-  sum(MX_dta$sex == 2 & MX_dta$q2 >= 55)/nrow(MX_dta)
FM <-  sum(MX_dta$sex == 2 & MX_dta$q2 > 35 & MX_dta$q2 < 55)/nrow(MX_dta)
FY <-  sum(MX_dta$sex == 2 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18)/nrow(MX_dta)
#Gender/Age/Edu joint Female
FOH <-  sum(MX_dta$sex == 2 & MX_dta$q2 >= 55 & MX_dta$ed > 12)/nrow(MX_dta)
FOM <-  sum(MX_dta$sex == 2 & MX_dta$q2 >= 55 & (MX_dta$ed == 11 |MX_dta$ed == 12))/nrow(MX_dta)
FOL <-  sum(MX_dta$sex == 2 & MX_dta$q2 >= 55 & MX_dta$ed < 11)/nrow(MX_dta)
FMH <-  sum(MX_dta$sex == 2 & MX_dta$q2 > 35 & MX_dta$q2 < 55 & MX_dta$ed > 12)/nrow(MX_dta)
FMM <-  sum(MX_dta$sex == 2 & MX_dta$q2 > 35 & MX_dta$q2 < 55 & (MX_dta$ed == 11 |MX_dta$ed == 12))/nrow(MX_dta)
FML <-  sum(MX_dta$sex == 2 & MX_dta$q2 > 35 & MX_dta$q2 < 55 & MX_dta$ed < 11)/nrow(MX_dta)
FYH <-  sum(MX_dta$sex == 2 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18 & MX_dta$ed > 12)/nrow(MX_dta)
FYM <-  sum(MX_dta$sex == 2 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18 & (MX_dta$ed == 11 |MX_dta$ed == 12))/nrow(MX_dta)
FYL <-  sum(MX_dta$sex == 2 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18 & MX_dta$ed < 11)/nrow(MX_dta)

End <- rbind(cbind(MOH, MOM, MOL, MMH, MMM, MML, MYH, MYM, MYL), cbind(FOH, FOM, FOL, FMH, FMM, FML, FYH, FYM, FYL))

#Gender/Age/EconStatus joint Male
MOT <-  sum(MX_dta$sex == 1 & MX_dta$q2 >= 55 & MX_dta$idio2 > 2)/nrow(MX_dta)
MOC <-  sum(MX_dta$sex == 1 & MX_dta$q2 >= 55 &  MX_dta$idio2 == 2)/nrow(MX_dta)
MOB <-  sum(MX_dta$sex == 1 & MX_dta$q2 >= 55 &  MX_dta$idio2 < 2)/nrow(MX_dta)
MMT <-  sum(MX_dta$sex == 1 & MX_dta$q2 > 35 & MX_dta$q2 < 55 &  MX_dta$idio2 > 2)/nrow(MX_dta)
MMC <-  sum(MX_dta$sex == 1 & MX_dta$q2 > 35 & MX_dta$q2 < 55 & MX_dta$idio2 == 2)/nrow(MX_dta)
MMB <-  sum(MX_dta$sex == 1 & MX_dta$q2 > 35 & MX_dta$q2 < 55 & MX_dta$idio2 < 2)/nrow(MX_dta)
MYT <-  sum(MX_dta$sex == 1 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18 &  MX_dta$idio2 > 2)/nrow(MX_dta)
MYC <-  sum(MX_dta$sex == 1 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18 & MX_dta$idio2 == 2)/nrow(MX_dta)
MYB <-  sum(MX_dta$sex == 1 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18 & MX_dta$idio2 < 2)/nrow(MX_dta)
#Gender/Age/EconStatus joint Female
FOT <-  sum(MX_dta$sex == 2 & MX_dta$q2 >= 55 &  MX_dta$idio2 > 2)/nrow(MX_dta)
FOC <-  sum(MX_dta$sex == 2 & MX_dta$q2 >= 55 & MX_dta$idio2 == 2)/nrow(MX_dta)
FOB <-  sum(MX_dta$sex == 2 & MX_dta$q2 >= 55 & MX_dta$idio2 < 2)/nrow(MX_dta)
FMT <-  sum(MX_dta$sex == 2 & MX_dta$q2 > 35 & MX_dta$q2 < 55 &  MX_dta$idio2 > 2)/nrow(MX_dta)
FMC <-  sum(MX_dta$sex == 2 & MX_dta$q2 > 35 & MX_dta$q2 < 55 & MX_dta$idio2 == 2)/nrow(MX_dta)
FMB <-  sum(MX_dta$sex == 2 & MX_dta$q2 > 35 & MX_dta$q2 < 55 &  MX_dta$idio2 < 2)/nrow(MX_dta)
FYT <-  sum(MX_dta$sex == 2 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18 &  MX_dta$idio2 > 2)/nrow(MX_dta)
FYC <-  sum(MX_dta$sex == 2 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18 & MX_dta$idio2 == 2)/nrow(MX_dta)
FYB <-  sum(MX_dta$sex == 2 & MX_dta$q2 <= 35 & MX_dta$q2 >= 18 & MX_dta$idio2 < 2)/nrow(MX_dta)

Ender <- rbind(cbind(MOT, MOC, MOB, MMT, MMC, MMB, MYT, MYC, MYB), cbind(FOT, FOC, FOB, FMT, FMC, FMB, FYT, FYC, FYB))
Ender
