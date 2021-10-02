### Quotas
set.seed(5)
setwd ("C:/Users/rzava/Box Sync/COVID survey/jointdist Data")

ipumsData <- read.csv("C:/Users/rzava/Box Sync/COVID survey/jointdist data/ipumsi_00002.csv.gz", sep=",", header=T)
# 
# country076 <- ipumsData[ipumsData$COUNTRY=="76",]
# country152 <- ipumsData[ipumsData$COUNTRY=="152",]
# country170 <- ipumsData[ipumsData$COUNTRY=="170",]
 country484 <- ipumsData[ipumsData$COUNTRY=="484",]
# 
#country076 <- country076[sample (1:nrow (country076), 500000),]
# country152 <- country152[sample (1:nrow (country152), 500000),]
# country170 <- country170[sample (1:nrow (country170), 500000),]
 country484 <- country484[sample (1:nrow (country484), 500000),]
# 
#write.table (country076, "Brazil.csv", sep=",", row.names=F)
# write.table (country152, "Chile.csv", sep=",", row.names=F)
# write.table (country170, "Colombia.csv", sep=",", row.names=F)
 write.table (country484, "Mexico.csv", sep=",", row.names=F)
# 
# 
# 

## Mexico data

# Read in data. Names are Mexico.csv, Brazil.csv, Chile.csv, Colombia.csv
# Mexico <- read.table ("Brazil.csv", header=T, sep=",")
Mexico <- read.table ("Mexico.csv", header=T, sep=",")
age.breaks <- c(35,55)    # set age.breaks identically to the ones we use for quotas
school.breaks <- c(8,15)  # set school.education breaks to correspond to "implied" SEL breaks, as per the following table

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

# In the previous table, the first column corresponds to points awarded to years of education in Netquest's Mexico formula
# The second column corresponds to implied years spent in school
# For example, 35 points are awarded for "Preparatoria incompleta";
# "Preparatoria incompleta" can mean anything from 10 to 11 years, so I use 11 as an equivalent
# Column 3 corresponds to "implied SEL points" for Mexico. The only
# "real" magnitude here is 200 (points larger that 204 place respondents in the A/B category)
# To calculate the rest of the "implied SEL points", simply solve for the missing fraction.
# For example, 144.5544 obtains when solving x = 200*73/101.
# Going back to Netquest's SEL table for Mexico, simply use the "letter grade"
# that corresponds to "implied SEL points". We cannot use the entire
# point system because Netquest also looks at some other variables that
# we don't have.
# I then chose school.breaks = 8, and 15 to separate those in E, from 
# those in D, from those in C- to A/B

### Couldn't decide whether YRSCHOOL values larger than 90 mean anything,
### or are simply missing values. When the next line is commented out, 
### we assume that these high values correspond to a lot of time in school
Mexico$YRSCHOOL <- ifelse (Mexico$YRSCHOOL>90, NA, Mexico$YRSCHOOL)
Mexico <- Mexico[!is.na(Mexico$YRSCHOOL),]
# Create factor equivalents of sex, age, and school, to be used in expand.grid
sex.factor <- as.factor (Mexico$SEX)
age.factor <- as.factor (ifelse (Mexico$AGE <= 35, 1, 
                      ifelse (Mexico$AGE >= 55, 3, 2)))
school.factor <- as.factor (ifelse (Mexico$YRSCHOOL <=10, 1,
                         ifelse (Mexico$YRSCHOOL > 12, 3, 2)))

# Mex.grid contains the 18 categories created by combining values of sex, age, and school
Mex.grid <- expand.grid (sex=c(1,2), age=c(1,2,3), school=c(1,2,3))

# The following code snippet calculates the number of respondents in 
# each of the 18 categories. It also calculates the number of weighted
# respondents, where weights come from variable PERWT
number.respondents <- number.wt.respondents <- c()
for (i in 1:2){
  for (j in 1:3){
    for (k in 1:3){
      temp <- Mexico[sex.factor==i & age.factor==j & school.factor==k,]
      number.respondents <- c(number.respondents, nrow(temp))
      number.wt.respondents <- c(number.wt.respondents, sum(temp$PERWT))
    }
  }
}

# Rather than counting absolute numbers, we add to Mex.grid variables
# that capture the share (in percent) of respondents in each of the 18
# categories
Mex.grid$fraction.respondents <- 100*number.respondents/sum(number.respondents)
Mex.grid$fraction.wt.respondents <- 100*number.wt.respondents/sum(number.wt.respondents)



