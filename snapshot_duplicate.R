#cleaned up and editied version of snapshot.R

##################### Packages -----------

library(data.table)
library(ltm)
#library(stringr)
#library(aod)
#library(brglm)
library(Hmisc)
#library(reshape2)
library(tidyr)
library(plyr)
library(RColorBrewer)
require(gridExtra)
library(car)
library(ggplot2)
library(xlsx)

####################### Read in files ----------------------
liege_CD4 <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/Ad hoc Projects/HIV/Data/Belgium/Université de Liège -Sart Tilman/csv/ULG_CD4_3.csv', header = T, na.strings=c(""))
liege_CD4_percent <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/Ad hoc Projects/HIV/Data/Belgium/Université de Liège -Sart Tilman/csv/ULG_CD4_PERCENT_3.csv', header = T, na.strings=c(""))
liege_death <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/Ad hoc Projects/HIV/Data/Belgium/Université de Liège -Sart Tilman/csv/ULG_DEATH_3.csv', header = T, na.strings=c(""))
liege_BAS <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/Ad hoc Projects/HIV/Data/Belgium/Université de Liège -Sart Tilman/csv/ULG_BAS_3.csv', header = T, na.strings=c(""), stringsAsFactors = FALSE)
liege_CEP <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/Ad hoc Projects/HIV/Data/Belgium/Université de Liège -Sart Tilman/csv/ULG_CEP_3.csv', header = T, na.strings=c(""))
liege_artcodes <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/Ad hoc Projects/HIV/Data/Belgium/ART_standardization.csv', header = T, na.strings=c(""))
liege_art <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/Ad hoc Projects/HIV/Data/Belgium/Université de Liège -Sart Tilman/csv/ULG_ART_3.csv', header = T, na.strings=c(""))
liege_VL <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/Ad hoc Projects/HIV/Data/Belgium/Université de Liège -Sart Tilman/csv/ULG_RNA_3.csv', header = T, na.strings=c(""))
liege_egfr <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/Ad hoc Projects/HIV/Data/Belgium/Université de Liège -Sart Tilman/csv/ULG_EGFR_3.csv', header = T, na.strings=c(""))
liege_liver <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/Ad hoc Projects/HIV/Data/Belgium/Université de Liège -Sart Tilman/csv/ULG_LAB_VIRO_3.csv', header = T, na.strings=c(""))
liege_hcv <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/Ad hoc Projects/HIV/Data/Belgium/Université de Liège -Sart Tilman/csv/HCV_only.csv', header = T, na.strings=c(""))

pierre_px <- read.csv2('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/Ad hoc Projects/HIV/Data/Belgium/St Pierre/PATIENTS.csv',
                       header = TRUE, quote = "\"", dec = ",", na.strings=c(""))

pierre_events <- read.csv2('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/Ad hoc Projects/HIV/Data/Belgium/St Pierre/EVENTS.csv',
                           header = TRUE, quote = "\"", dec = ",", na.strings=c(""))

erasme_base <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/Ad hoc Projects/HIV/Data/Belgium/Erasme/Erasme_base.csv', header = T, na.strings=c(""))
erasme_ART <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/Ad hoc Projects/HIV/Data/Belgium/Erasme/Erasme_ART.csv', header = T, na.strings=c(""))

erasme_cd4 <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/Ad hoc Projects/HIV/Data/Belgium/Erasme/Erasme_CD4.csv', header = T, na.strings=c(""))

#aggregate the different CM dataframes
erasme_cancer <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/Ad hoc Projects/HIV/Data/Belgium/Erasme/Erasme_cancer.csv', header = T, na.strings=c(""))
erasme_cvd <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/Ad hoc Projects/HIV/Data/Belgium/Erasme/Erasme_cvd.csv', header = T, na.strings=c(""))
erasme_diabetes <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/Ad hoc Projects/HIV/Data/Belgium/Erasme/Erasme_diabetes.csv', header = T, na.strings=c(""))
erasme_hta <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/Ad hoc Projects/HIV/Data/Belgium/Erasme/Erasme_hta.csv', header = T, na.strings=c(""))
erasme_liver <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/Ad hoc Projects/HIV/Data/Belgium/Erasme/Erasme_liver.csv', header = T, na.strings=c(""))
erasme_renal <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/Ad hoc Projects/HIV/Data/Belgium/Erasme/Erasme_renal.csv', header = T, na.strings=c(""))
erasme_bmi <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/Ad hoc Projects/HIV/Data/Belgium/Erasme/Erasme_bmi.csv', header = T, na.strings=c(""), sep=',')
erasme_new <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/Ad hoc Projects/HIV/Data/Belgium/Erasme/Erasme_new_data.csv', header = T, na.strings=c(""), sep=',')
erasme_nadir <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/Ad hoc Projects/HIV/Data/Belgium/Erasme/Erasme_nadircd4.csv', header = T, na.strings=c(""), sep=',')

ghent_px1 <-read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/Ad hoc Projects/HIV/Data/Belgium/Ghent/Ghent_new_data.csv', header = T, na.strings=c("UNK"))
ghent_px2 <-read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/Ad hoc Projects/HIV/Data/Belgium/Ghent/Ghent_new_data3.csv', header = T, na.strings=c("UNK"))
ghent_ICP <-read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/Ad hoc Projects/HIV/Data/Belgium/Ghent/Ghent_ICP.csv', header = T, na.strings=c("UNK"))


##### Standard ID names --------

#first, rename the id variable in all data frames to PATIENT_ID

id <- function(df) {
  colnames(df)[1] <- "PATIENT_ID"
  df
}

liege_CD4 <- id(liege_CD4)
liege_CD4_percent <- id(liege_CD4_percent)
liege_death <- id(liege_death)
liege_BAS <- id(liege_BAS)
liege_CEP <- id(liege_CEP)
liege_artcodes <- id(liege_artcodes)
liege_art <- id(liege_art)
liege_VL <- id(liege_VL)
liege_egfr <- id(liege_egfr)
liege_liver <- id(liege_liver)
liege_hcv <- id(liege_hcv)

erasme_nadir <- id(erasme_nadir)
erasme_cancer <- id(erasme_cancer)
erasme_cvd <- id(erasme_cvd)
erasme_liver <- id(erasme_liver)
erasme_new <- id(erasme_new)
erasme_renal <- id(erasme_renal)
erasme_hta <- id(erasme_hta)
erasme_diabetes <- id(erasme_diabetes)
erasme_bmi<- id(erasme_bmi)
erasme_cd4 <- id(erasme_cd4)
erasme_ART <- id(erasme_ART)
erasme_base <- id(erasme_base)

pierre_px <- id(pierre_px)
pierre_events <- id(pierre_events)

#change Ghent "current legal sex" to GENDER
names(ghent_px2)[4] <- "GENDER"

##################### Overall functions ----------------------

comb <- function(df1, df2) {
  master <- merge(df1, df2, by = "PATIENT_ID", all = TRUE)
  master
}

comb_F <- function(df1, df2) {
  master <- merge(df1, df2, by = "PATIENT_ID", all = FALSE)
  master
}

#bin the age groups by 5 years
breaks <- seq(0,95, by=5)
age_groups <- c("0-5","6-10","11-15","16-20","21-25", "26-30","31-35","36-40","41-45",
                "46-50","51-55", "56-60", "61-65","66-70", "71-75", "76-80",
                "81-85", "86-90","90-95")

#write a function to bin age groups
binning_ages <- function(df) {
  df$age_binned <- cut(df$age, breaks, include.lowest=T)
  levels(df$age_binned) <- age_groups
  return(df)
}

#write a function to create a new column that is conditional on whether the sum column => 1

dichotomous <- function(DT) {
  DT$sum_col <- rowSums(DT[,2:7, with=FALSE])
  DT$var_yes <- ifelse(DT$sum_col>=1, 1, 0)
  DT
}

### age function
#from http://r.789695.n4.nabble.com/Calculate-difference-between-dates-in-years-td835196.html
age_years <- function(first, second) {
  df <- data.frame(first, second) 
  age <- as.numeric(format(df[,2],format="%Y")) - as.numeric(format(df[,1],format="%Y")) 
  first <- as.Date(paste(format(df[,2],format="%Y"),"-",format(df[,1],format="%m-%d"),sep="")) 
  age[which(first > df[,2])] <- age[which(first > df[,2])] - 1 
  age 
}

today <- Sys.Date()

#get percentages of region of origin
ethnic_freq <- function(df) {
  site_region <- data.frame(table(df$REGION_OF_ORIGIN))
  site_region$Freq <- (site_region$Freq/sum(site_region$Freq))*100
  site_region
}

#change NA to 0 and create a variable for polypathology
varto0 <- function(df) {
  df[c("dia", "cvd", "ac", "hl", "liver", "lung", "hcv", "ckd", "hyp")][is.na(df[c("dia", "cvd", "ac", "hl", "liver", "lung", "hcv", "ckd", "hyp")])] <- 0
  df$poly <- df$dia + df$cvd + df$ac + df$hl + df$liver + df$lung + df$hcv + df$ckd
  df
  }

#############################################################################################
#Liege
###################### Liege treatment ------------------

#make dates into as.Date and to change the fake dates to NA

names(liege_BAS)[11] <- 'DATE.START.ART'
liege_BAS[, cols <- grep("DATE", names(liege_BAS))] <- lapply(liege_BAS[, cols <- grep("DATE", names(liege_BAS))], as.Date, format = "%m/%d/%Y")

#subtract birth date from today's date, make new column for age
liege_BAS$sys_date <- Sys.Date()

liege_BAS$age <- age_years(liege_BAS$BIRTH_DATE, liege_BAS$sys_date)

#find the number of patients on ART
liege_BAS$started_art <- ifelse(liege_BAS$DATE.START.ART >= "1911-11-11", 1, 0)

#didn't work for NA so make those into 0
liege_BAS$started_art[is.na(liege_BAS$started_art)] <- 0

#now make the placeholder date NA so it doesn't throw off the averages in the calcs
liege_BAS$DATE.START.ART[liege_BAS$DATE.START.ART == "1911-11-11"] <- NA
liege_BAS$art_duration_weeks <- difftime(liege_BAS$sys_date, liege_BAS$DATE.START.ART, 
                                         units="weeks")

liege_BAS$art_duration_months <- (liege_BAS$art_duration_weeks)/4.348125 

liege_BAS$art_duration_years <- (liege_BAS$art_duration_months)/12
liege_BAS$art_duration_years <- as.numeric(liege_BAS$art_duration_years)

#remove unnecessary cols from liege_BAS
liege_BAS <- liege_BAS[, -c(22, 23)]

#standardize spelling in ethnic
liege_BAS$ETHNIC <- gsub("africain", "Africain", liege_BAS$ETHNIC)
liege_BAS$ETHNIC <- gsub("caucasien", "Caucasien", liege_BAS$ETHNIC)

liege_CEP$CLIN_EVENT_DATE <- as.Date(liege_CEP$CLIN_EVENT_DATE, format = "%m/%d/%Y")
liege_CEP$CLIN_EVENT_DATE[liege_CEP$CLIN_EVENT_DATE == "1911-11-11"]<- NA

#make "other" gender NA
liege_BAS$GENDER <- gsub("O", NA, liege_BAS$GENDER)

########### Liege master DF -------

#liege_master will include dead patients
#For current living patients under follow up, use liege_not_LTFU

liege_master <- comb(liege_BAS, liege_death)

liege_master$GENDER <- gsub("F", "Female", liege_master$GENDER)
liege_master$GENDER <- gsub("M", "Male", liege_master$GENDER)

######################## Liege ethnic var -------------------
#rename the country variable in Liege_not_LTFU to match pierre_not_LTFU's region of origin

liege_master <- within(liege_master, COUNTRY[COUNTRY == 'Belgique' | COUNTRY == 'Italie'|
                                               COUNTRY == 'France'| COUNTRY == 'Espagne'| 
                                               COUNTRY == 'Grece' | COUNTRY == 'Royaume Uni' |
                                               COUNTRY == 'Portugal' | COUNTRY == 'Albanie' |
                                               COUNTRY == 'Pays-Bas'] <- 'Western Europe')

liege_master <- within(liege_master, COUNTRY[COUNTRY == 'Turquie' | COUNTRY == 'Inde'|
                                               COUNTRY == 'Armenie'| COUNTRY == 'Indonesie'| 
                                               COUNTRY == 'Japon' | COUNTRY == 'Thailande' |
                                               COUNTRY == 'Liban' | COUNTRY == 'Ouzbekistan' |
                                               COUNTRY == 'Kirghizistan' | COUNTRY == 'Chine'] <- 'Asia')

liege_master <- within(liege_master, COUNTRY[COUNTRY == 'Slovaquie' | COUNTRY == 'Russie' |
                                               COUNTRY == 'Roumanie' | COUNTRY == 'Pologne' |
                                               COUNTRY == 'Lituanie' | COUNTRY == 'Ukraine' |
                                               COUNTRY == 'Georgie'] <- 'Eastern Europe')

liege_master <- within(liege_master, COUNTRY[COUNTRY == 'Mali' | COUNTRY == 'Rwanda'|
                                               COUNTRY == 'Ouganda'| COUNTRY == 'Niger'| 
                                               COUNTRY == 'Kenya'| COUNTRY == 'RDC'|
                                               COUNTRY == 'Sierra Leone'| COUNTRY == 'Tchad'|
                                               COUNTRY == 'Zambie'| COUNTRY == 'Liberia' |
                                               COUNTRY == 'Cap Vert'| COUNTRY == 'Djibouti' |
                                               COUNTRY == 'Congo-Brazzaville' | COUNTRY == 'Cameroun' |
                                               COUNTRY == 'Burundi' | COUNTRY == 'Gabon' |
                                               COUNTRY == 'Burkina Faso' | COUNTRY == 'Benin' |
                                               COUNTRY == 'Tanzanie' | COUNTRY == 'Republique Centrafricaine' |
                                               COUNTRY == 'Angola' | COUNTRY == 'Ghana' |
                                               COUNTRY == 'Togo' | COUNTRY == 'Afrique du sud' |
                                               COUNTRY == 'Senegal' | COUNTRY == 'Ethiopie' |
                                               COUNTRY == 'Somalie' | COUNTRY == 'Mauritanie' |
                                               COUNTRY == 'Guinee-Conakry' | COUNTRY == 'Guinee Equatoriale' |
                                               COUNTRY == 'Guinee'| COUNTRY == 'RDC (Goma)' |
                                               COUNTRY == 'Erythree'] <- 'Sub-Saharan Africa')

liege_master <- within(liege_master, COUNTRY[COUNTRY == 'Cuba' | COUNTRY == 'Haiti'] <- 'Latin America/Caribbean')

liege_master <- within(liege_master, COUNTRY[COUNTRY == 'Bresil' | COUNTRY == 'Perou' |
                                               COUNTRY == 'Equateur' | COUNTRY == 'Uruguay' |
                                               COUNTRY == 'Venezuela'] <- 'Latin America/Caribbean')

liege_master <- within(liege_master, COUNTRY[COUNTRY == 'Maroc' | COUNTRY == 'Algerie'] <- 'North Africa/Middle East')


liege_master <- within(liege_master, COUNTRY[COUNTRY == 'Canada' | COUNTRY == 'Etats-Unis'] <- 'North America')

liege_master$COUNTRY<- gsub("Cote d'Ivoire", "Sub-Saharan Africa", liege_master$COUNTRY)
liege_master$COUNTRY<- gsub("Inconnu", "Unknown", liege_master$COUNTRY)

#change ethnicity variable name so ethnic_freq function can be used later
colnames(liege_master)[which(names(liege_master) == "COUNTRY")] <- "REGION_OF_ORIGIN"


############## Liege BMI -------------

### find BMI for Liege patients
liege_master$HEIGHT[liege_master$HEIGHT == "999"] <- NA
liege_master$WEIGHT[liege_master$WEIGHT == "999"] <- NA

liege_master$HEIGHT <- liege_master$HEIGHT*liege_master$HEIGHT
liege_master$BMI <- (liege_master$WEIGHT/liege_master$HEIGHT)*10000


##### Liege HCV ------

#make new col w binary response
liege_hcv$hcv <- 1
liege_hcv <- liege_hcv[, c(1, 7)]

liege_master <- comb(liege_master, liege_hcv)

########## Liege CD4 -----------

#find nadir CD4 count for each subject
liege_CD4_nadir <- aggregate(liege_CD4$CD4_VALUE ~ liege_CD4$PATIENT_ID, liege_CD4, min)
colnames(liege_CD4_nadir)[1:2] <- c('PATIENT_ID', 'CD4_NADIR')

#find mean CD4 count for each subject
liege_CD4_avg <- aggregate(liege_CD4$CD4_VALUE ~ liege_CD4$PATIENT_ID, liege_CD4, mean)
colnames(liege_CD4_avg)[1:2] <- c('PATIENT_ID', 'CD4_AVG')

#find most recent CD4 count
#convert dates from a factor to a date
liege_CD4$CD4_DATE <- as.Date(liege_CD4$CD4_DATE, format = "%m/%d/%Y")
liege_CD4_recent <- aggregate(liege_CD4$CD4_DATE ~ liege_CD4$PATIENT_ID, liege_CD4, max)
colnames(liege_CD4_recent)[1:2] <- c('PATIENT_ID', 'CD4_DATE')
liege_CD4_recent <- merge(liege_CD4, liege_CD4_recent, by = c("PATIENT_ID", "CD4_DATE"), all=FALSE)

names(liege_CD4_recent)[3] <- 'CD4_RECENT'

#merge into one master CD4 dataframe
liege_CD4_all <- comb(liege_CD4_nadir, liege_CD4_avg)
liege_CD4_all <- comb(liege_CD4_all, liege_CD4_recent)

#find mean recent CD4 count for px not LTFU
liege_master <- comb(liege_master, liege_CD4_all)

####################### Liege NICMs --------------------------


#reshape to wide format 
#create new col that indicates that the person has the disease
liege_CEP$pos <- 1
liege_events_wide <- dcast(liege_CEP, PATIENT_ID ~ CLIN_EVENT_ID)

#make new cvd col that is a combo of stroke, acs, icp
liege_events_wide$cvd_sum <- liege_events_wide$STR + liege_events_wide$ACS + liege_events_wide$ICP
liege_events_wide$cvd <- ifelse(liege_events_wide$cvd_sum >= 1, 1, 0)
liege_master <- comb(liege_master, liege_events_wide)

liege_nadm_wide <- dcast(liege_CEP, PATIENT_ID ~ CLIN_EVENT_SPECIFICATION)
liege_nadm_wide <- liege_nadm_wide[, -c(3, 4, 8)]
colnames(liege_nadm_wide)[2:5] <- c("ac", "hl", "liver", "lung")
liege_master <- comb(liege_master, liege_nadm_wide)

######## liege eGFR ------

#liege eGFR calc
#conditions: 
## for earliest date in column 2 for each PATIENT_ID, if egfr in column 3 is above 60:
### check if most recent egfr is <60
#if egfr for earliest date is <60, check if there was a decline of 25% or more between that 
# and most recent measurement. 

liege_egfr$DATE <- as.Date(liege_egfr$DATE, format = "%m/%d/%Y")

liege_egfr_base <- ddply(liege_egfr, .(PATIENT_ID), function(x)x[which.min(x$DATE), ])
liege_egfr_recent <- ddply(liege_egfr, .(PATIENT_ID), function(x)x[which.max(x$DATE), ])

liege_egfr_diff <- comb(liege_egfr_base, liege_egfr_recent)
liege_egfr_diff <- as.data.table(liege_egfr_diff)

#VALUE.x and VALUE.y have to be numeric
liege_egfr_diff[VALUE.x == "> 60", VALUE.x := "60"]
liege_egfr_diff[VALUE.y == "> 60", VALUE.y := "60"]

liege_egfr_diff$VALUE.x <- as.numeric(as.character(liege_egfr_diff$VALUE.x))
liege_egfr_diff$VALUE.y <- as.numeric(as.character(liege_egfr_diff$VALUE.y))

liege_egfr_diff$pos <- ifelse(liege_egfr_diff$VALUE.y < 60 & liege_egfr_diff$VALUE.x >= 60, 1, 0)
liege_egfr_diff$pos2 <- ifelse(liege_egfr_diff$VALUE.y <= 0.25*liege_egfr_diff$VALUE.x, 1, 0)

liege_egfr_diff$ckd_tmp <- ifelse(liege_egfr_diff$pos == 1 | liege_egfr_diff$pos2 == 1, 1, 0)

liege_egfr_diff <- as.data.frame(liege_egfr_diff)
liege_egfr_diff <- liege_egfr_diff[, c(1, 10)]

#combine with px in liege_master who have esrd

liege_master <- comb(liege_master, liege_egfr_diff)
liege_master$ckd <- ifelse(liege_master$ESRD == 1 | liege_master$ckd_tmp == 1, 1, 0)

#################### Liege LTFU -----------------
#rename variables and clean up the df
liege_master2 <- subset(liege_master, select = -c(cvd_sum, HEPC, ESRD, ckd_tmp, NADM, STR, ICP, ACS, ASCI, COPD, FRA, OSTEOPOROSIS, HIV_TYPE))
names(liege_master2)[30] <- "dia"
names(liege_master2)[16] <- "hyp"


liege_master2 <- varto0(liege_master2)
liege_master2 <- binning_ages(liege_master2)

#living, not LTFU px
liege_recent <- subset(liege_master2, LAST_VIS_DATE > "2014-05-31")
liege_not_LTFU <- subset(liege_recent, STATUS == "Follow up")


#living and dead but not LTFU
liege_FU_all <- subset(liege_recent, STATUS != "Contact lost")
liege_FU_all <- subset(liege_FU_all, STATUS != "Transferred")

#find patients that are LTFU
liege_LTFU <- subset(liege_px, STATUS == "Contact lost")

liege_dead <- subset(liege_FU_all, STATUS == "Death")

ethnic_freq(liege_not_LTFU)

##### Liege smoking -------

table(liege_not_LTFU$SMOKING)

##### Liege hyp -------

table(liege_not_LTFU$DRUG_HTA)

### Liege drug graphs ---------

#subset to just ART drugs that have no end date (i.e. are presumably still being taken)
liege_art2015 <- subset(liege_art, is.na(ART_END_DATE))

liege_art_merge <- merge(liege_artcodes, liege_art2015, by = "drug_code", all.x=TRUE, all.y=TRUE)
liege_art_merge2 <- liege_art_merge %>% count(art_name, wt = NULL)
liege_art_merge2 <- merge(liege_art_merge2, liege_artcodes, by = "art_name", all.x=TRUE, all.y=TRUE)

#end of Liege cleaning

###############################################################################################
#Pierre
####################### Pierre - changes to orig. data -----------------

pierre_px$DOB <- as.Date(pierre_px$DOB, format = "%d/%m/%Y")

pierre_px$DOB[pierre_px$DOB == "1911-11-11"] <- NA

pierre_px$sys_date <- as.Date(format(today, format = "%Y-%m-%d"))

#use previously defined age_years function
pierre_px$age <- age_years(pierre_px$DOB, pierre_px$sys_date)

############ Pierre CVD ----------------

#pierre cvd includes hypchol right now, need to remove
pierre_events <- subset(pierre_events, OUTCOME != "2 - CVD - HYPCHOL")

############ make pierre_events into wide format ------

#make a new col in pierre_events that is just 1
pierre_events$pointless_col <- 1

#clean up pierre_events by dropping unneeded cols
pierre_events2 <- subset(pierre_events, select=-c(EVENT, DURATION_PI_MONTHS_AT_EVENT, DURATION_NRTI_MONTHS_AT_EVENT, 
                                                  DURATION_NNRTI_MONTHS_AT_EVENT, DURATION_INTI_MONTHS_AT_EVENT, 
                                                  DURATION_FUSI_MONTHS_AT_EVENT, VL_AT_EVENT))

#jake function
pierre_events2$patient_row <- unlist(tapply(pierre_events2$pointless_col, 
                                           list(pierre_events2$PATIENT_ID), seq))

pierre_events2 <- within(pierre_events2, {
  row_num <- unlist(sapply(rle(PATIENT_ID)$lengths, seq))
})

pierre_events_wide <- reshape(pierre_events2, timevar = 'row_num', 
                                 idvar = 'PATIENT_ID', direction = 'wide')



#clean it up by dropping some columns

# pierre_events_recent <- subset(pierre_events_recent, select = -c(pointless_col.1, pointless_col.2, 
#                                                                  pointless_col.3, pointless_col.4, 
#                                                                  pointless_col.5, pointless_col.6, 
#                                                                  pointless_col.7, patient_row.1, 
#                                                                  patient_row.2, patient_row.3, 
#                                                                  patient_row.4, patient_row.5, 
#                                                                  patient_row.6, patient_row.7))
                               
                             

########################## Pierre Hepatitis -------------------------------


pierre_events_wide <- as.data.frame(lapply(pierre_events_wide, FUN = function(foo) recode(foo, "c('No', 'N')= 0; 
                                                                                              c('Yes', 'Y')= 1; 'UNK'= NA")))

for(i in c(7:17, 25:35, 43:53, 61:71, 79:89)) {
  pierre_events_wide[,i] <- as.numeric(as.character(pierre_events_wide[,i]))
}



pierre_events_wide$sum_hcv <- rowSums(pierre_events_wide[,c("HCV_AT_EVENT.1", "HCV_AT_EVENT.2", 
                                                                "HCV_AT_EVENT.3", "HCV_AT_EVENT.4", 
                                                                "HCV_AT_EVENT.5")], na.rm=T)

pierre_events_wide$hcv <- ifelse(pierre_events_wide$sum_hcv >= 1, 1, 0)

pierre_events_wide$sum_hbv <- rowSums(pierre_events_wide[,c("HBV_AT_EVENT.1", "HBV_AT_EVENT.2", 
                                                                "HBV_AT_EVENT.3", "HBV_AT_EVENT.4", 
                                                                "HBV_AT_EVENT.5")], na.rm=T)

pierre_events_wide$hbv <- ifelse(pierre_events_wide$sum_hbv >= 1, 1, 0)


################### Pierre CD4 ----------------

#find the minimum nadir cd4

for (i in 1:nrow(pierre_events_wide)) {
  pierre_events_wide$cd4_nadir[i] <- min(pierre_events_wide[i,c("CD4_NADIR_EVENT.1", 
                                                                    "CD4_NADIR_EVENT.2", 
                                                                    "CD4_NADIR_EVENT.3", 
                                                                    "CD4_NADIR_EVENT.4", 
                                                                    "CD4_NADIR_EVENT.5")], na.rm=TRUE)
}


#make the class of DTE_EVENT cols date rather than factor 
for(i in c(4, 22, 40, 58, 76)) {
  pierre_events_wide[,i] <- as.Date(pierre_events_wide[,i], format="%d/%m/%Y", origin="1900-01-01")
}

#find cd4 recent for most recent DTE_EVENT
#subset to just the date cols

pierre_events_cd4 <- pierre_events_wide[, c(1, 4, 7, 22, 25, 40, 43, 58, 61, 76, 79)]
date_cols <- colnames(pierre_events_cd4)[grep("DTE_EVENT",colnames(pierre_events_cd4))]
pierre_events_cd4$recent_cd4<-pierre_events_cd4[cbind(1:nrow(pierre_events_cd4),grep("DTE_EVENT",colnames(pierre_events_cd4))[apply(sapply(pierre_events_cd4[,date_cols],as.numeric),1,which.max)]+1)]

#eliminate unneeded columns before the merge

pierre_events_cd4 <- pierre_events_cd4[, c(1, 12)]

#merge with pierre_events_recent
pierre_events_wide <- comb(pierre_events_wide, pierre_events_cd4)

#don't do any calculations on pierre_events_recent until it is combined with not_LTFU


########### Pierre hyp ------------------
##treated for hypertension

pierre_events_wide$sum_hyp <- rowSums(pierre_events_wide[,c("TRT_HYPERTENSION_AT_EVENT.1", "TRT_HYPERTENSION_AT_EVENT.2", 
                                                                "TRT_HYPERTENSION_AT_EVENT.3", "TRT_HYPERTENSION_AT_EVENT.4", 
                                                                "TRT_HYPERTENSION_AT_EVENT.5")], na.rm=T)

pierre_events_wide$hyp_trt <- ifelse(pierre_events_wide$sum_hyp >= 1, 1, 0)

###### Pierre smoking -------------------

pierre_events_wide$sum_smoke <- rowSums(pierre_events_wide[,c("EVER_SMOKED_AT_EVENT.1", "EVER_SMOKED_AT_EVENT.2", 
                                                                "EVER_SMOKED_AT_EVENT.3", "EVER_SMOKED_AT_EVENT.4", 
                                                                "EVER_SMOKED_AT_EVENT.5")], na.rm=T)


pierre_events_wide$smoke <- ifelse(pierre_events_wide$sum_smoke >= 1, 1, 0)

########################## Pierre BMI ----------------------

#should be most recent BMI not avg as previously calculated

#subset to just the BMI cols

pierre_events_bmi <- pierre_events_wide[, c(1, 4, 12, 22, 30, 40, 48, 58, 66, 76, 84)]
date_cols <- colnames(pierre_events_bmi)[grep("DTE_EVENT",colnames(pierre_events_bmi))]
pierre_events_bmi$recent_bmi <- pierre_events_bmi[cbind(1:nrow(pierre_events_bmi),grep("DTE_EVENT",colnames(pierre_events_bmi))[apply(sapply(pierre_events_bmi[,date_cols],as.numeric),1,which.max)]+1)]

pierre_events_bmi_trim <- pierre_events_bmi[, c(1, 12)]

pierre_events_wide <- comb(pierre_events_wide, pierre_events_bmi_trim)

################### Pierre treatment ------------------
#Started treatment at event

pierre_events_wide$sum_trt <- rowSums(pierre_events_wide[,c("STARTED_TREATMENT_AT_EVENT.1", "STARTED_TREATMENT_AT_EVENT.2", 
                                                                  "STARTED_TREATMENT_AT_EVENT.3", "STARTED_TREATMENT_AT_EVENT.4", 
                                                                  "STARTED_TREATMENT_AT_EVENT.5")], na.rm=T)


pierre_events_wide$started_trt <- ifelse(pierre_events_wide$sum_trt >= 1, 1, 0)

######### Pierre NICMs ------------

pierre_events_wide$cvd <-apply(pierre_events_wide[,1:ncol(pierre_events_wide)], 
                                     MARGIN=1, function(x) {sum(x=="2 - CVD - STROKE" | x=="2 - CVD - MI" | x=="2 - CVD - Endart" | 
                                                                  x=="2 - CVD - BYPASS" | x=="2 - CVD - Angioplasty", 
                                                                na.rm=TRUE)})

pierre_events_wide$ac <-apply(pierre_events_wide[,1:ncol(pierre_events_wide)], 
                                MARGIN=1, function(x) {sum(x=="11 - ANAL Cancer", na.rm=TRUE)})

pierre_events_wide$lung <-apply(pierre_events_wide[,1:ncol(pierre_events_wide)], 
                                MARGIN=1, function(x) {sum(x=="10 - LUNG Cancer", na.rm=TRUE)})

pierre_events_wide$hl <-apply(pierre_events_wide[,1:ncol(pierre_events_wide)], 
                                MARGIN=1, function(x) {sum(x=="8 - Hodg. Lymp", na.rm=TRUE)})

pierre_events_wide$hyp <-apply(pierre_events_wide[,1:ncol(pierre_events_wide)], 
                                MARGIN=1, function(x) {sum(x=="3 - HYPERTENSION", na.rm=TRUE)})

pierre_events_wide$dia <-apply(pierre_events_wide[,1:ncol(pierre_events_wide)], 
                                 MARGIN=1, function(x) {sum(x=="4 - DIABETE", na.rm=TRUE)})

pierre_events_wide$ckd <-apply(pierre_events_wide[,1:ncol(pierre_events_wide)], 
                                 MARGIN=1, function(x) {sum(x=="1 - RENAL DISEASE", na.rm=TRUE)})

pierre_events_wide$liver <-apply(pierre_events_wide[,1:ncol(pierre_events_wide)], 
                               MARGIN=1, function(x) {sum(x=="6 - LIVER Cancer", na.rm=TRUE)})


######################### Pierre LTFU -----------

## Create dataframes of the not_LTFU px that should be used for 2015 snapshot

#first subset living patients

pierre_px$DTE_END_STUDY <- as.Date(pierre_px$DTE_END_STUDY, format = "%d/%m/%Y")

pierre_master <- comb(pierre_px, pierre_events_wide)

#clean up pierre_comb

pierre_master2 <- subset(pierre_master, select = c(PATIENT_ID, DOB, GENDER, REGION_OF_ORIGIN, DTE_START_STUDY, 
                                                  DTE_END_STUDY, DEATH, age,
                                                  age_binned, hcv, hbv, cd4_nadir, recent_cd4, hyp_trt, smoke, 
                                                  recent_bmi, started_trt, liver, cvd, ac, lung, hl, hyp, dia, ckd))


pierre_master2$hyp <- as.numeric(pierre_master2$hyp)
pierre_master2$ckd <- as.numeric(pierre_master2$ckd)

pierre_master2 <- varto0(pierre_master2)
pierre_master2 <- binning_ages(pierre_master2)

#region of origin standardization
pierre_master2$REGION_OF_ORIGIN <- gsub("North Africa", "North Africa/Middle East", pierre_master2$REGION_OF_ORIGIN)

pierre_alive <- subset(pierre_master2, DEATH == "0")

#subset into LTFU and FU

pierre_LTFU <- subset(pierre_alive, DTE_END_STUDY <= "2014-05-31")

#living and under follow up
pierre_not_LTFU <- subset(pierre_alive, DTE_END_STUDY > "2014-05-31")

#followed up with since 2014 but dead and alive
pierre_FU_all <- subset(pierre_master2, DTE_END_STUDY > "2014-05-31")

pierre_dead <- subset(pierre_FU_all, DEATH == "1")

#End of Pierre cleaning
#############################################################################################
#Erasme
################# Erasme master -------------------------

a <- as.Date(erasme_new$LAST_VISIT,format="%d/%m/%Y")
b <- as.Date(erasme_new$LAST_VISIT,format="%d-%m-%Y") 
a[is.na(a)] <- b[!is.na(b)] # Combine both while keeping their ranks
erasme_new$LAST_VISIT <- a # Put it back in dataframe

erasme_master <- comb(erasme_base, erasme_new)
erasme_master$DEATH_Date <- as.Date(erasme_master$DEATH_Date,format="%d-%m-%Y") 
names(erasme_master)[2] <- "FIRST_VIS_DATE"

## recalculate age and replace AGE variable

erasme_master$BIRTH_Date <- as.Date(erasme_master$BIRTH_Date, format = "%d-%m-%Y")

erasme_master$sys_date <- as.Date(format(today, format = "%Y-%m-%d"))

#use previously defined age_years function
erasme_master$age <- age_years(erasme_master$BIRTH_Date, erasme_master$sys_date)

#drop original AGE column

erasme_master <- erasme_master[, -c(5)]

##### erasme ethnic ---------

#rename the country variable in erasme_master to match pierre_not_LTFU's region of origin
erasme_master$Country_of_origin <- as.character(erasme_master$Country_of_origin)

erasme_master <- within(erasme_master, Country_of_origin[Country_of_origin == 'Belgique' | Country_of_origin == 'Italie'|
                                                           Country_of_origin == 'France'| Country_of_origin == 'Espagne'| 
                                                           Country_of_origin == 'Grece' | Country_of_origin == 'Royaume-Uni' |
                                                           Country_of_origin == 'Portugal' | Country_of_origin == 'Albanie' |
                                                           Country_of_origin == 'Pays-Bas' |  Country_of_origin == 'Martinique' |
                                                           Country_of_origin == 'Luxembourg' | Country_of_origin == 'Finlande' |
                                                           Country_of_origin == 'Allemagne'] <- 'Western Europe')

erasme_master <- within(erasme_master, Country_of_origin[Country_of_origin == 'Turquie' | Country_of_origin == 'Inde'|
                                                           Country_of_origin == 'Armenie'| Country_of_origin == 'Indonesie'| 
                                                           Country_of_origin == 'Japon' | Country_of_origin == 'Tha\xeflande' |
                                                           Country_of_origin == 'Liban' | Country_of_origin == 'Viet Nam' |
                                                           Country_of_origin == 'Coree, Republique Populaire Democratique De'] <- 'Asia')

erasme_master <- within(erasme_master, Country_of_origin[Country_of_origin == 'Slovaquie' | Country_of_origin == 'Russie'|
                                                           Country_of_origin == 'Roumanie' | Country_of_origin == 'Pologne' |
                                                           Country_of_origin == 'Lettonie' | Country_of_origin == 'Serbie'] <- 'Eastern Europe')

erasme_master <- within(erasme_master, Country_of_origin[Country_of_origin == 'Mali' | Country_of_origin == 'Rwanda'|
                                                           Country_of_origin == 'Ouganda'| Country_of_origin == 'Niger'| 
                                                           Country_of_origin == 'Kenya'| Country_of_origin == 'RDC'|
                                                           Country_of_origin == 'Sierra Leone'| Country_of_origin == 'Tchad'|
                                                           Country_of_origin == 'Zambie'| Country_of_origin == 'Liberia' |
                                                           Country_of_origin == 'Cap Vert'| Country_of_origin == 'Djibouti' |
                                                           Country_of_origin == 'Congo' | Country_of_origin == 'Cameroun' |
                                                           Country_of_origin == 'Burundi' | Country_of_origin == 'Gabon' |
                                                           Country_of_origin == 'Burkina Faso' | Country_of_origin == 'Benin' |
                                                           Country_of_origin == 'Tanzanie, Republique-Unie De' | Country_of_origin == 'Republique Centrafricaine' |
                                                           Country_of_origin == 'Angola' | Country_of_origin == 'Ghana' |
                                                           Country_of_origin == 'Togo' | Country_of_origin == 'Afrique du sud' |
                                                           Country_of_origin == 'Senegal' | Country_of_origin == 'ethiopie' |
                                                           Country_of_origin == 'Somalie' | Country_of_origin == 'Mauritanie' |
                                                           Country_of_origin == 'Guinee-Conakry' | Country_of_origin == 'Guinee Equatoriale' |
                                                           Country_of_origin == 'Guinee' | Country_of_origin == 'Nigeria' |
                                                           Country_of_origin == 'Maurice' | Country_of_origin == 'Guinee-Bissau' |
                                                           Country_of_origin == 'Congo, La Republique Democratique Du' | Country_of_origin == 'Cote DIvoire' |
                                                           Country_of_origin == 'Afrique Du Sud' |
                                                           Country_of_origin == 'Centrafricaine, Republique'] <- 'Sub-Saharan Africa')

erasme_master <- within(erasme_master, Country_of_origin[Country_of_origin == 'Cuba' | Country_of_origin == 'Jama\xefque' |
                                                           Country_of_origin == 'Haiti' | Country_of_origin == 'Mexique' |
                                                           Country_of_origin == 'Ha\xefti' | Country_of_origin == 'Barbade' |
                                                           Country_of_origin == 'Dominicaine, Republique'] <- 'Latin America/Caribbean')

erasme_master <- within(erasme_master, Country_of_origin[Country_of_origin == 'Bresil' | Country_of_origin == 'Perou' |
                                                           Country_of_origin == 'equateur' | Country_of_origin == 'Venezuela' |
                                                           Country_of_origin == 'Chili' | Country_of_origin == 'Colombie'] <- 'Latin America/Caribbean')

erasme_master <- within(erasme_master, Country_of_origin[Country_of_origin == 'Maroc' | Country_of_origin == 'Algerie'
                                                         | Country_of_origin == 'Isra\xebl'] <- 'North Africa/Middle East')


erasme_master <- within(erasme_master, Country_of_origin[Country_of_origin == 'Canada' | Country_of_origin == 'etats-Unis'] <- 'North America')
names(erasme_master)[7] <- "REGION_OF_ORIGIN"

##### Erasme cd4 ---------

erasme_master <- comb(erasme_master, erasme_cd4)

erasme_nadir$cd4_nadir_v <- as.numeric(erasme_nadir$cd4_nadir_v)
erasme_master <- comb(erasme_master, erasme_nadir)

####### erasme BMI --------------

#data input errors
erasme_bmi$heigh[erasme_bmi$heigh < 100] <- NA
erasme_bmi$weigh[erasme_bmi$weigh > 170] <- NA

erasme_bmi$heigh <- erasme_bmi$heigh*erasme_bmi$heigh
erasme_bmi$bmi <- (erasme_bmi$weigh/erasme_bmi$heigh)*10000
erasme_bmi$bmi[erasme_bmi$bmi > 60] <- NA

erasme_bmi_sorted <- erasme_bmi[order(-erasme_bmi$bmi),]
erasme_bmi_subset <- subset(erasme_bmi, bmi > 1)
nrow(erasme_bmi_subset)/nrow(erasme_bmi)
erasme_bmi$vis_d <- as.Date(erasme_bmi$vis_d, format="%d-%m-%Y") 

#take most recent bmi for each person
erasme_bmi <- as.data.table(erasme_bmi)

erasme_bmi_recent <- setDT(erasme_bmi)[,.SD[which.max(vis_d)],keyby=PATIENT_ID]

erasme_master <- comb(erasme_master, erasme_bmi_recent)

##### Erasme NICMs --------

#reshape erasme cvd 
#first create an unknown category for the NAs in col 3
erasme_cvd$code <- as.character(erasme_cvd$code)
erasme_cvd$code <- replace(erasme_cvd$code,which(is.na(erasme_cvd$code)),"Unknown")

#make new col that is just 1 for future binary response
erasme_cvd$cvd <- 1

#drop the date col 
erasme_cvd <- subset(erasme_cvd, select= -c(ae_date))

#reshape
erasme_cvd_wide <- reshape(erasme_cvd, timevar = 'code', 
                           idvar = 'PATIENT_ID', direction = 'wide')

#replace na with 0
erasme_cvd_wide[is.na(erasme_cvd_wide)] <- 0

#rename cols
colnames(erasme_cvd_wide) <- c("PATIENT_ID", "STROKE", "UNKNOWN", "MI", "ICP")

#sum the cols
erasme_cvd_wide$cvd_sum <- erasme_cvd_wide$STROKE + erasme_cvd_wide$UNKNOWN + erasme_cvd_wide$MI + 
  erasme_cvd_wide$MI + erasme_cvd_wide$ICP

erasme_cvd_wide$cvd <- ifelse(erasme_cvd_wide$cvd_sum >= 1, 1, 0)

#merge with erasme_not_LTFU
erasme_master <- comb(erasme_master, erasme_cvd_wide)

######## Erasme renal ----------

erasme_renal$ckd <- 1

#drop the date col 
erasme_renal <- subset(erasme_renal, select= -c(ae_date))

erasme_master <- comb(erasme_master, erasme_renal)

####### Erasme diabetes ------

erasme_diabetes$dia <- 1
erasme_diabetes <- subset(erasme_diabetes, select= -c(ae_date, CODE))

erasme_master <- comb(erasme_master, erasme_diabetes)

######### Erasme cancer -------

erasme_cancer <- subset(erasme_cancer, select = -c(ae_date, code))

erasme_cancer$specCode[erasme_cancer$specCode == 0] <- "ac"
erasme_cancer$specCode[erasme_cancer$specCode == 1] <- "hl"
erasme_cancer$nadm <- 1

erasme_cancer_wide <- reshape(erasme_cancer, timevar = "specCode", idvar = 'PATIENT_ID', direction = 'wide')
colnames(erasme_cancer_wide)[c(2:3)] <- c("hl", "ac")

erasme_master <- comb(erasme_master, erasme_cancer_wide)

###### Erasme HTA -------

erasme_hta$hyp <- 1
erasme_hta <- subset(erasme_hta, select = -c(ae_date, medical.history.report))

erasme_master <- comb(erasme_master, erasme_hta)
  
###### Erasme liver ------

#make new columns for HCV and HBV status

erasme_liver$hbv <- ifelse(erasme_liver$Medical.history.report == "Hepatite B chronique", 1, 0)
erasme_liver$hcv <- ifelse(erasme_liver$Medical.history.report == "Hepatite C chronique", 1, 0)

erasme_liver <- subset(erasme_liver, select = -c(ae_date, Medical.history.report, Notes))

#merge with erasme_master
erasme_master <- comb(erasme_master, erasme_liver)

########################### erasme years on ART -----------------

#find the maximum value of years on any ART for each patient
#recalculate art duration

#make the art end date sys_date if it is NA
erasme_ART$art_end.date <- as.Date(erasme_ART$art_end.date, "%d-%m-%Y")
erasme_ART$art_start.date <- as.Date(erasme_ART$art_start.date, format = "%d-%m-%Y")

erasme_ART$art_end.date[is.na(erasme_ART$art_end.date)] <- as.Date(today, format = "%d-%m-%Y")

erasme_ART$art_duration_weeks <- difftime(erasme_ART$art_end.date, erasme_ART$art_start.date, 
                                          units="weeks")

erasme_ART$art_duration_months <- (erasme_ART$art_duration_weeks)/4.348125 

erasme_ART$art_duration_years <- (erasme_ART$art_duration_months)/12

erasme_years_ART <- aggregate(art_duration_years ~ PATIENT_ID, data = erasme_ART, max)

erasme_master <- comb(erasme_master, erasme_years_ART)
names(erasme_master)[7] <- "REGION_OF_ORIGIN"
erasme_master$art_duration_years<- as.numeric(erasme_master$art_duration_years)

erasme_master$art_duration_years[is.na(erasme_master$art_duration_years)] <- 0
erasme_master$started_trt <- ifelse(erasme_master$art_duration_years > 0, 1, 0)

####### Erasme LTFU --------
for(i in c(24:31)) {
  erasme_master[,i] <- as.numeric(as.character(erasme_master[,i]))
}

#create col with 0s for liver
erasme_master$liver <- 0
erasme_master$lung <- 0
erasme_master <- varto0(erasme_master)
erasme_master <- binning_ages(erasme_master)

#limit it to px who have been followed up with since 2014

erasme_FU_all <- subset(erasme_master, LAST_VISIT > as.Date("2014-05-31"))
erasme_not_LTFU <- subset(erasme_FU_all, is.na(DEATH_Date))

erasme_LTFU <- subset(erasme_master, LAST_VISIT < as.Date("2014-06-01"))

erasme_FU_all$death <- ifelse(erasme_FU_all$DEATH_Date > "1900-01-01", 1, 0)
erasme_dead <- subset(erasme_FU_all, death == 1)

#do all calcs on erasme_not_LTFU

##############################################################################################

###### Ghent master ---------

#convert all necessary columns to date cols

ghent_px2[,c(2, 7, 12, 29)]<-lapply(ghent_px2[,c(2, 7, 12, 29)],as.Date, format = "%Y-%m-%d")

#ghent age

ghent_px2$sys_date <- as.Date(format(today, format = "%Y-%m-%d"))

#use previously defined age_years function
ghent_px2$age <- age_years(ghent_px2$DATE_BIRTH, ghent_px2$sys_date)

#remove column that was age at time of data collection
ghent_px2 <- ghent_px2[, !(colnames(ghent_px2) %in% "AGE")]

########## Ghent hyp -------

names(ghent_px1)[2] <- "OUTCOME"
ghent_px1$hyp <- ifelse(ghent_px1$OUTCOME == 6, 1, 0)
ghent_hyp <- ghent_px1[c(1, 16)]

ghent_hyp_wide <- dcast(ghent_hyp, PATIENT_ID ~ hyp)
ghent_hyp_wide_final <- ghent_hyp_wide[c(1, 3)]
names(ghent_hyp_wide_final)[2] <- "hyp"
ghent_hyp_wide_final$hyp <- ifelse(ghent_hyp_wide_final$hyp > 0, 1, 0)

#####Ghent recent -------

ghent_last_vis <- data.frame(PATIENT_ID = ghent_px1$PATIENT_ID, LAST_VISIT_DATE = as.Date(ghent_px1$LAST_VISIT_DATE))
ghent_last_vis <- ghent_last_vis[!duplicated(ghent_last_vis),]

ghent_master <- comb(ghent_px2, ghent_last_vis)
ghent_master <- comb(ghent_master, ghent_hyp_wide_final)

ghent_master$GENDER <- gsub("M", "Male", ghent_master$GENDER)
ghent_master$GENDER <- gsub("F", "Female", ghent_master$GENDER)

################## Ghent ethnic --------------

names(ghent_master)[4] <- "REGION_OF_ORIGIN"

ghent_master$REGION_OF_ORIGIN[ghent_master$REGION_OF_ORIGIN == 10] <- "Western Europe"
ghent_master$REGION_OF_ORIGIN[ghent_master$REGION_OF_ORIGIN == 20] <- "Western Europe"
ghent_master$REGION_OF_ORIGIN[ghent_master$REGION_OF_ORIGIN == 21] <- "Sub-Saharan Africa"
ghent_master$REGION_OF_ORIGIN[ghent_master$REGION_OF_ORIGIN == 22] <- "Latin America/Caribbean"
ghent_master$REGION_OF_ORIGIN[ghent_master$REGION_OF_ORIGIN == 30] <- "Latin America/Caribbean"
ghent_master$REGION_OF_ORIGIN[ghent_master$REGION_OF_ORIGIN == 40] <- "Asia"
ghent_master$REGION_OF_ORIGIN[ghent_master$REGION_OF_ORIGIN == 50] <- "North America"
ghent_master$REGION_OF_ORIGIN[ghent_master$REGION_OF_ORIGIN == 97] <- "Other/unknown"
ghent_master$REGION_OF_ORIGIN[ghent_master$REGION_OF_ORIGIN == 99] <- "Other/unknown"

names(ghent_master)[5] <- "TRANSMISSION"

#### Ghent years on ART -------

ghent_master$art_duration_weeks <- difftime(ghent_master$sys_date, ghent_master$START_DATE_ART, 
                                          units="weeks")

ghent_master$art_duration_months <- (ghent_master$art_duration_weeks)/4.348125 

ghent_master$art_duration_years <- (ghent_master$art_duration_months)/12
ghent_master$art_duration_years <- as.numeric(ghent_master$art_duration_years)
ghent_master$art_duration_years[is.na(ghent_master$art_duration_years)] <- 0
ghent_master$started_trt <- ifelse(ghent_master$art_duration_years > 0, 1, 0)

################### Ghent NICM ------------

ghent_hepc <- subset(ghent_px1, HCVRNA_AT.EVENT == "POS")
ghent_hepc$hcv <- 1
ghent_hepc <- ghent_hepc[, c(1, ncol(ghent_hepc))]

ghent_master <- comb(ghent_hepc, ghent_master)

ghent_master <- comb(ghent_master, ghent_ICP)

ghent_master$ICP <- as.numeric(ghent_master$ICP)

ghent_master$cvd <- ghent_master$MI + ghent_master$STROKE + ghent_master$ICP

#remove unneeded cols

ghent_master2 <- subset(ghent_master, select = -c(FRACTURES, MI, STROKE, HYPERTENSION, HYPERCHOLESTEROLAEMIA, 
                                                  ICP, X, NHL, COUNT_OUTCOME, MOST_RECENT_VIT_D_BLOOD_LEVEL..ng.mL., 
                                                  CC, LOST_TO_FOLLOW_UP))
colnames(ghent_master2)[c(12, 13, 15:17)] <- c("ckd", "dia", "hl", "lung", "ac")
ghent_master2$liver <- 0

ghent_master2 <- varto0(ghent_master2)
ghent_master2 <- binning_ages(ghent_master2)

ghent_master2$MOST_RECENT_CD4 <- as.character(ghent_master2$MOST_RECENT_CD4)
ghent_master2$MOST_RECENT_CD4 <- as.numeric(ghent_master2$MOST_RECENT_CD4)

################## Ghent LTFU -----------

ghent_FU_all <- subset(ghent_master2, LAST_VISIT_DATE > as.Date("2014-05-31"))

ghent_not_LTFU <- subset(ghent_FU_all, DEATH == 0)

ghent_LTFU <- subset(ghent_master2, LAST_VISIT_DATE < as.Date("2014-06-01"))

ghent_dead <- subset(ghent_FU_all, DEATH == 1)

################# all sites mortality ----------------------

cvd_conting = matrix(c(3,22,132,4755), nrow = 2)
fisher.test(cvd_conting)

renal_conting = matrix(c(7,24,451,5337), nrow = 2)
fisher.test(renal_conting)

dia_conting = matrix(c(5,20,344,5444), nrow = 2)
fisher.test(dia_conting)

hcv_conting = matrix(c(7,24,96,5692), nrow = 2)
fisher.test(hcv_conting)

#polypathologies?


hcv_conting = matrix(c(7,24,96,5692), nrow = 2)
fisher.test(hcv_conting)

#####

tab <- function(df) {
  base <- as.data.frame(table(age_binned = df$age_binned))
  base
}

pierre_tab <- tab(pierre_FU_all)
pierre_tab_death <- tab(pierre_dead)
liege_tab <- tab(liege_FU_all)
liege_tab_death <- tab(liege_dead)
erasme_tab <- tab(erasme_FU_all)
erasme_tab_death <- tab(erasme_dead)
ghent_tab <- tab(ghent_FU_all)
ghent_tab_death <- tab(ghent_dead)


avg_pops_df <- as.data.frame(cbind(liege_tab[1], liege_tab$Freq, pierre_tab$Freq, 
                                   erasme_tab$Freq, ghent_tab$Freq))

total_deaths_df <- as.data.frame(cbind(liege_tab[1], liege_tab_death$Freq, pierre_tab_death$Freq, 
                                       erasme_tab_death$Freq, ghent_tab_death$Freq))


colnames(avg_pops_df)[c(1:5)] <- (c("Age_groups", "Liege", "St. Pierre", "Erasme", "Ghent"))
colnames(total_deaths_df)[c(1:5)] <- (c("Age_groups", "Liege", "Pierre", "Erasme", "Ghent"))

write.xlsx(total_deaths_df, "/Users/cda/Desktop/total_deaths_df.xlsx")
write.xlsx(avg_pops_df, "/Users/cda/Desktop/avg_pops_df.xlsx")
#graph it 
#bring it back

new_crude_df_fixed <- read.csv('/Users/cda/Desktop/new_crude_df.csv', header = T, na.strings=c(""))

######### crudge rates
#make crude and adjusted rates
liege_tab$crude <- (liege_tab_death$Freq/liege_tab$Freq)*1000
pierre_tab$crude <- (pierre_tab_death$Freq/pierre_tab$Freq)*1000
erasme_tab$crude <- (erasme_tab_death$Freq/erasme_tab$Freq)*1000
ghent_tab$crude <- (ghent_tab_death$Freq/ghent_tab$Freq)*1000

avg_pops_df$avg <- (rowMeans(avg_pops_df[,-1]))
total_deaths_df$avg <- (rowMeans(total_deaths_df[2:5]))
total_deaths_df$sum <- rowSums(total_deaths_df[2:5])
avg_pops_df$sum <- rowSums(avg_pops_df[2:5])

liege_tab$adj <- (liege_tab_death$Freq/avg_pops_df$avg)*1000
pierre_tab$adj <- (pierre_tab_death$Freq/avg_pops_df$avg)*1000
erasme_tab$adj <- (erasme_tab_death$Freq/avg_pops_df$avg)*1000
ghent_tab$adj <- (ghent_tab_death$Freq/avg_pops_df$avg)*1000

total_deaths_df$total <- (total_deaths_df$avg/avg_pops_df$avg)*1000

mortal_tabs <- as.data.frame(cbind(liege_tab[1], liege_tab$adj, pierre_tab$adj, erasme_tab$adj, ghent_tab$adj, total_deaths_df$total))
colnames(mortal_tabs)[c(1:6)] <- c("Age", "Liege", "St. Pierre", "Erasme", "Ghent", "All")

mortal_tabs_long <- melt(mortal_tabs, id="Age")  # convert to long format
names(mortal_tabs_long)[2] <- "Cohort"

standardized_mortality <- ggplot(data=mortal_tabs_long,
                                 aes(x=Age, y=value, group = Cohort, colour=Cohort)) +
  geom_line(size=1.5) + ylab("Mortality rate per 1000") + ggtitle("Age-adjusted standardized mortality rates by cohort")


new_crude_df_fixed$Age <- factor(new_crude_df_fixed$Age, levels = new_crude_df_fixed$Age)

#poster graph, updated with new Ghent death data
new_graph_over_66 <- ggplot(data=new_crude_df_fixed, aes(x=Age, y=crude_mortality, group = 1)) +
  geom_line(size=1.5) + ylab("Mortality rate per 1000") + 
  xlab("Age groups") +
  ggtitle("Age-adjusted crude mortality rates") + 
  theme(legend.position = "bottom", plot.title = element_text(size = 30, face="bold"))

######## Look closer at 51-55

deaths_51 <- function(df) {
  new_df <- subset(df, age_binned == "51-55")
  new_df
}

erasme_51 <- deaths_51(erasme_dead)
liege_51 <- deaths_51(liege_dead)
pierre_51 <- deaths_51(pierre_dead)
ghent_51 <- deaths_51(ghent_dead)

#merge just hcv_yes from pierre_events_recent with dead
dead_hep_tmp <- pierre_events_recent[c(1, 115)]
#make the patient IDs integers
dead_hep_tmp$PATIENT_ID <- as.integer(dead_hep_tmp$PATIENT_ID)
dead_hep <- merge(dead_hep_tmp, pierre_dead, by="PATIENT_ID", all=FALSE)

#pierre dead under 60
pierre_60 <- subset(pierre_dead, age < 61)

################ combined mortality graph--------------

all_tabs <- as.data.frame(cbind(liege_tab$Freq, pierre_tab$Freq, erasme_tab$Freq, ghent_tab$Freq))
colnames(all_tabs) <- c("Liege", "Pierre", "Erasme", "Ghent")

all_tabs$sum <- rowSums(all_tabs)

all_tabs_death <- as.data.frame(cbind(liege_tab_death$Freq,
                                      pierre_tab_death$Freq, erasme_tab_death$Freq,
                                      ghent_tab_death$Freq))

colnames(all_tabs_death) <- c("Liege", "Pierre", "Erasme", "Ghent")
all_tabs_death$sum <- rowSums(all_tabs_death)
sum(all_tabs_death$sum)/sum(all_tabs$sum)

all_tabs$mortality <- (all_tabs_death$sum)/(all_tabs$sum)
sum(all_tabs$mortality, na.rm=TRUE)
all_tabs$mortality <- all_tabs$mortality*100000
all_tabs <- as.data.frame(cbind(liege_tab[1], all_tabs))

mort_total_plot <- ggplot(data=all_tabs,
                          aes(x=Var1, y=mortality, group=1)) +
  geom_line(size=1.5) + xlab("Age groups") + 
  ylab("Mortality rate (per 100)") + 
  ggtitle("Age-adjusted crude mortality rate, all cohorts") +
  theme(plot.title = element_text(size = 20)) + 
  theme(axis.title = element_text(size = 15))

#get the overall crude mortality rates for each cohort

(sum(all_tabs_death$Liege)/sum(all_tabs$Liege)) 
(sum(all_tabs_death$Pierre)/sum(all_tabs$Pierre)
sum(all_tabs_death$Erasme)/sum(all_tabs$Erasme)
sum(all_tabs_death$Ghent)/sum(all_tabs$Ghent)

#total pop mortality rate
(sum(all_tabs_death$sum)/sum(all_tabs$sum))*1000


####### Combined smoking data ------

#exclude Ghent because the reporting rate is so low

table(liege_not_LTFU$SMOKING)
251+112

table(erasme_not_LTFU$HAS_SMOKED)
273

table(pierre_smoke_not_LTFU$var_yes)
1212

(363+273+1212)
1848/(nrow(liege_not_LTFU)+nrow(pierre_not_LTFU)+nrow(erasme_not_LTFU))

###################### LTFU graphs -------------------------------

#LTFU graphs

#histograms

pierre_LTFU_histo <- ggplot(pierre_not_LTFU, binwidth = 5, aes(x=age, stat="count")) + 
  geom_histogram(data=subset(pierre_not_LTFU,LTFU == 0),fill = "red", 
                 alpha = 0.4, binwidth = 5) +
  geom_histogram(data=subset(pierre_LTFU,LTFU == 1),fill = "blue", 
                 alpha = 0.4, binwidth = 5) + 
  scale_x_continuous(breaks=seq(0,90,5)) + 
  ggtitle("St. Pierre") +
  theme(plot.title = element_text(size = 20), axis.title.y=element_blank(),
        axis.title.x=element_blank()) +
  coord_cartesian(xlim=c(0,90))

liege_LTFU_histo <- ggplot(liege_not_LTFU, binwidth = 5, aes(x=age, stat="count")) + 
  geom_histogram(data=subset(liege_not_LTFU,STATUS == "Follow up"),fill = "red", 
                 alpha = 0.4, binwidth = 5) +
  geom_histogram(data=subset(liege_LTFU,STATUS == "Contact lost"),fill = "blue", 
                 alpha = 0.4, binwidth = 5) + 
  scale_x_continuous(breaks=seq(0,90,5)) + 
  ggtitle("Liege") +
  theme(plot.title = element_text(size = 20), axis.title.y=element_blank(),
        axis.title.x=element_blank()) +
  coord_cartesian(xlim=c(0,90))

erasme_LTFU_histo <- ggplot(erasme_not_LTFU, binwidth = 5, aes(x=age, stat="count")) + 
  geom_histogram(data=subset(erasme_not_LTFU,status == "Follow up"),fill = "red", 
                 alpha = 0.4, binwidth = 5) +
  geom_histogram(data=subset(erasme_master,status == "LTFU"),fill = "blue", 
                 alpha = 0.4, binwidth = 5) + 
  scale_x_continuous(breaks=seq(0,90,5)) + 
  ggtitle("Erasme") +
  theme(plot.title = element_text(size = 20), axis.title.y=element_blank(),
        axis.title.x=element_blank()) +
  coord_cartesian(xlim=c(0,90))

ghent_LTFU_histo <- ggplot(ghent_not_LTFU, binwidth = 5, aes(x=age, stat="count")) + 
  geom_histogram(data=subset(ghent_not_LTFU,status == "Follow up"),fill = "red", 
                 alpha = 0.4, binwidth = 5) +
  geom_histogram(data=subset(ghent_px_master,status == "LTFU"),fill = "blue", 
                 alpha = 0.4, binwidth = 5) + 
  scale_x_continuous(breaks=seq(0,90,5)) + 
  ggtitle("Ghent") +
  theme(plot.title = element_text(size = 20), axis.title.y=element_blank(),
        axis.title.x=element_blank()) +
  coord_cartesian(xlim=c(0,90))

grid.arrange(pierre_LTFU_histo, liege_LTFU_histo, erasme_LTFU_histo, ghent_LTFU_histo)

####################### BMI Graphs ----------------------

#create boxplots - BMI by gender

#Liege
#with diamond at the mean
liege_bmi <- ggplot(liege_not_LTFU, aes(GENDER, BMI, fill=GENDER)) + geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=5, size=6) + 
  scale_fill_manual(values = c("seagreen3", "dodgerblue3")) + 
  scale_y_continuous(breaks=seq(0,50,5)) + 
  guides(fill=FALSE) + 
  ggtitle("BMI Distribution - Liège") + 
  xlab("Gender") +
  ylab("BMI")

#####pierre
#gender and bmi are in different databases

pierre_gen_bmi <- data.frame(comb_F(pierre_bmi_avg, pierre_not_LTFU))

pierre_bmi <- ggplot(pierre_gen_bmi, aes(GENDER, avg, fill=GENDER)) + geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=5, size=6) + 
  scale_fill_manual(values = c("seagreen3", "dodgerblue3")) + 
  scale_y_continuous(breaks=seq(0,50,5)) + 
  ggtitle("BMI Distribution - St. Pierre") + 
  xlab("Gender") +
  ylab("BMI") +
  guides(fill=FALSE)


grid.arrange(liege_bmi, pierre_bmi, ncol=2)

######################################################### age distros ----


#combine all the ages for summary statistics
ages_summary <- as.data.frame(c(liege_not_LTFU$age, ghent_not_LTFU$age, erasme_not_LTFU$age, 
                                pierre_not_LTFU$age))

summary(ages_summary[1])


#cut down on the size of the data frames, try just age first
#include only living patients

pierre_age_df <- data.frame(id = pierre_not_LTFU$PATIENT_ID, 
                            age = pierre_not_LTFU$age, 
                            age_binned = pierre_not_LTFU$age_binned, 
                            gender = pierre_not_LTFU$GENDER)
liege_age_df <- data.frame(id = liege_not_LTFU$PATIENT_ID, 
                           age = liege_not_LTFU$age, 
                           age_binned = liege_not_LTFU$age_binned, 
                           gender = liege_not_LTFU$GENDER)
erasme_age_df <- data.frame(id = erasme_not_LTFU$PATIENT_ID, 
                            age = erasme_not_LTFU$age, 
                            age_binned = erasme_not_LTFU$age_binned, 
                            gender = erasme_not_LTFU$GENDER)
ghent_age_df <- data.frame(id = ghent_not_LTFU$PATIENT_ID, 
                           age = ghent_not_LTFU$age, 
                           age_binned = ghent_not_LTFU$age_binned, 
                           gender = ghent_not_LTFU$GENDER)


#one combined DF
all_age <- rbind(pierre_age_df, liege_age_df, erasme_age_df, ghent_age_df)
all_age$gender <- as.character(all_age$gender)
all_age <- all_age[complete.cases(all_age[4]),]


#freq tables for histograms

pierre_age_freq <- data.frame(table(pierre_age_df$age_binned))
liege_age_freq <- data.frame(table(liege_age_df$age_binned))
erasme_age_freq <- data.frame(table(erasme_age_df$age_binned))
ghent_age_freq <- data.frame(table(ghent_age_df$age_binned))

#make freq a percentage of total
freq_perc <- function(df) {
  df$freq_percent <- as.vector(df$Freq/sum(df$Freq))
  return(df)
}

liege_age_freq_perc <- freq_perc(liege_age_freq)
pierre_age_freq_perc <- freq_perc(pierre_age_freq)
erasme_age_freq_perc <- freq_perc(erasme_age_freq)
ghent_age_freq_perc <- freq_perc(ghent_age_freq)

#all age
all_age_freq <- data.frame(table(all_age$age_binned))
all_age_freq_perc <- freq_perc(all_age_freq)

#histograms
liege_bar_age <- ggplot(data = liege_age_freq_perc, aes(Var1, freq_percent) ) +
  geom_bar(stat="identity", fill = "seagreen4") + 
  ggtitle("Liège") + theme(axis.text.x = element_blank(), 
                           axis.title.x=element_blank(),
                           axis.title.y=element_blank(),
                           axis.ticks.x=element_blank())

pierre_bar_age <- ggplot(data = pierre_age_freq_perc, aes(Var1, freq_percent) ) +
  geom_bar(stat="identity", fill = "seagreen4") + 
  ggtitle("St. Pierre") + theme(axis.text.x = element_blank(), 
                                axis.title.x=element_blank(),
                                axis.title.y=element_blank(),
                                axis.ticks.x=element_blank())

erasme_bar_age <- ggplot(data = erasme_age_freq_perc, aes(Var1, freq_percent) ) +
  geom_bar(stat="identity", fill = "seagreen4") +
  ggtitle("Erasme") + theme(axis.text.x = element_blank(), 
                            axis.title.x=element_blank(),
                            axis.title.y=element_blank(),
                            axis.ticks.x=element_blank())

ghent_bar_age <- ggplot(data = ghent_age_freq_perc, aes(Var1, freq_percent) ) +
  geom_bar(stat="identity", fill = "seagreen4") + 
  ggtitle("Ghent") + theme(axis.text.x = element_blank(), 
                           axis.title.x=element_blank(),
                           axis.title.y=element_blank(),
                           axis.ticks.x=element_blank())

all_bar_age <- ggplot(data = all_age_freq_perc, aes(Var1, freq_percent) ) +
  geom_bar(stat="identity", fill = "dodgerblue4") + xlab("Age groups") + ylab("Frequency (as % of total)") +
  ggtitle("Age distribution - All") + theme(plot.title = element_text(size = 20))

#theme(axis.text.x = element_text(angle = 60, hjust = 1))


grid.arrange(all_bar_age, arrangeGrob(liege_bar_age, pierre_bar_age, 
                                      erasme_bar_age, ghent_bar_age, ncol=4), heights=c(2.5/4, 1.5/4), ncol=1)


############ age boxplots --------------------

### remove NA from the distributions of liege and erasme 
liege_age_df$gender <- as.character(liege_age_df$gender)
erasme_age_df$gender <- as.character(erasme_age_df$gender)

liege_age_df <- liege_age_df[complete.cases(liege_age_df[4]),]
erasme_age_df <- erasme_age_df[complete.cases(erasme_age_df[4]),]

#boxplots, separate
#with diamond at the mean
liege_box_age <- ggplot(liege_age_df, aes(gender, age, fill=gender)) + geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=5, size=6) + 
  scale_fill_manual(values = c("seagreen3", "dodgerblue3")) + 
  scale_y_continuous(breaks=seq(0,95,5), limits = c(10,95)) + 
  theme(legend.position="none", axis.title.x=element_blank(),
        axis.title.y=element_blank(), plot.title = element_text(size=20)) + 
  ggtitle("Liège")

pierre_box_age <- ggplot(pierre_age_df, aes(gender, age, fill=gender)) + geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=5, size=6) + 
  scale_fill_manual(values = c("seagreen3", "dodgerblue3")) + 
  scale_y_continuous(breaks=seq(0,95,5), limits = c(10,95)) +
  theme(legend.position="none", axis.title.x=element_blank(),
        axis.title.y=element_blank(), plot.title = element_text(size=20)) + 
  ggtitle("St. Pierre")

erasme_box_age <- ggplot(erasme_age_df, aes(gender, age, fill=gender)) + geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=5, size=6) + 
  scale_fill_manual(values = c("seagreen3", "dodgerblue3")) + 
  scale_y_continuous(breaks=seq(0,95,5), limits = c(10,95)) + 
  theme(legend.position="none", axis.title.x=element_blank(),
        axis.title.y=element_blank(), plot.title = element_text(size=20)) + 
  ggtitle("Erasme")

ghent_box_age <- ggplot(ghent_age_df, aes(gender, age, fill=gender)) + geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=5, size=6) + 
  scale_fill_manual(values = c("seagreen3", "dodgerblue3")) + 
  scale_y_continuous(breaks=seq(0,95,5), limits = c(10,95)) + 
  theme(legend.position="none", axis.title.x=element_blank(),
        axis.title.y=element_blank(), plot.title = element_text(size=20)) +
  ggtitle("Ghent")

all_box_age <- ggplot(all_age, aes(gender, age, fill=gender)) + geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=5, size=6) + 
  scale_fill_manual(values = c("seagreen3", "dodgerblue3")) + 
  scale_y_continuous(breaks=seq(0,95,5), limits = c(10,95)) + 
  theme(legend.position="none", axis.title.x=element_blank(),
        axis.title.y=element_blank(), 
        plot.title = element_text(size = 30, face = "bold")) + 
  ggtitle("Age & Gender Distribution - All centers")

#to make a common legend for all
# go here: http://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots

grid.arrange(all_box_age, arrangeGrob(liege_box_age , pierre_box_age, erasme_box_age, 
                                      ghent_box_age, ncol=4), heights=c(2.5/4, 1.5/4), ncol=1)



###################### Cohort summaries --------------------

#Use the living patients not LTFU 

#combine into one vector to get average recent CD4
cd4_all <- list(ghent_not_LTFU$MOST_RECENT_CD4, erasme_not_LTFU$last.cd4_value, 
                   liege_not_LTFU$CD4_RECENT, pierre_not_LTFU$recent_cd4)
cd4_melt <- melt(cd4_all)
mean(cd4_melt$value, na.rm=TRUE)

#no nadir measurement for ghent
cd4_nadir_all <- list(erasme_not_LTFU$cd4_nadir_v, 
                liege_not_LTFU$CD4_NADIR, pierre_not_LTFU$cd4_nadir)
cd4_nadir_melt <- melt(cd4_nadir_all)

mean(cd4_nadir_melt$value, na.rm=TRUE)

#gender
gender_all <- list(ghent_not_LTFU$GENDER, erasme_not_LTFU$GENDER, 
                      liege_not_LTFU$GENDER, pierre_not_LTFU$GENDER)
gender_melt <- melt(gender_all)

### combined % started treatment

started_trt_all <- list(erasme_not_LTFU$started_trt, 
                      liege_not_LTFU$started_art, pierre_not_LTFU$started_trt_yes)
started_trt_all_melt <- melt(started_trt_all)

#combined years on ART

years_on_ART_all <- list(erasme_not_LTFU$art_duration_years, 
                        liege_not_LTFU$art_duration_years, ghent_not_LTFU$art_duration_years)
years_on_ART_all <- melt(years_on_ART_all)
#don't remove NA but make sure to do na.rm=TRUE in the mean calculation

#combined hypertension

#hypertension combined in a list DID NOT WORK
#BUT WHYYYYYYYYY


###### region of origin plots --------

#combined region
region_all <- list(ghent_not_LTFU$REGION_OF_ORIGIN, erasme_not_LTFU$REGION_OF_ORIGIN, 
                   liege_not_LTFU$REGION_OF_ORIGIN, pierre_not_LTFU$REGION_OF_ORIGIN)
region_melt <- melt(region_all)
table(region_melt$value)

colnames(region_melt) <- c("Region", "Site")
region_melt$Region <- gsub("Central America", "Latin America/Caribbean", region_melt$Region)
region_melt$Region <- gsub("South America", "Latin America/Caribbean", region_melt$Region)
region_melt$Region <- gsub("UNKNOWN", "Other/unknown", region_melt$Region)
region_melt$Region <- gsub("Eastern Europe", "Europe", region_melt$Region)
region_melt$Region <- gsub("Western Europe", "Europe", region_melt$Region)
region_melt$Region <- gsub("Oceania Aust. NZ", "Other/unknown", region_melt$Region)
region_melt$Region <- gsub("Unknown", "Other/unknown", region_melt$Region)

region_melt$Region[is.na(region_melt$Region)] <- "Other/unknown"

region_tab <- as.data.frame(table(region_melt$Region))
region_tab$Proportion <- (region_tab$Freq/(sum(region_tab$Freq)))*100
region_tab$Proportion <- round(region_tab$Proportion, 1)
names(region_tab)[1] <- "Region"

#relevel the factor orders by proportion
region_tab$Region <- factor(region_tab$Region, levels=region_tab[order(-region_tab$Proportion), "Region"])

region_bar <- ggplot(region_tab, aes(x=Region, y=Proportion)) +
  geom_bar(stat="identity", fill="seagreen3") +
  geom_text(aes(label=Proportion), position=position_dodge(width=0.9), vjust=-0.25) +
  ggtitle("Region of origin") + theme(plot.title = element_text(size=30, face="bold"), axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1, size = 12))

#########
#Prevalence of NICMs by age 

#liege
#cvd

# NICM_age <- function(df, x) {
#   df_1 <- subset(df, age <= x)
#   df_1
# }

liege_cvd4 <- subset(liege_cvd_not_LTFU_no_dups, age > 60)
liege_cvd3 <- subset(liege_cvd_not_LTFU_no_dups, age <= 60 & age > 50)
liege_cvd2 <- subset(liege_cvd_not_LTFU_no_dups, age <=50 & age > 40)
liege_cvd1 <- subset(liege_cvd_not_LTFU_no_dups, age <= 40)

liege_60 <- subset(liege_not_LTFU, age > 60)
liege_5160 <- subset(liege_not_LTFU, age <= 60 & age > 50)
liege_4150 <- subset(liege_not_LTFU, age <=50 & age > 40)
liege_40 <- subset(liege_not_LTFU, age <= 40)

pierre_cvd4 <- subset(pierre_cvd_not_LTFU, age > 60)
pierre_cvd3 <- subset(pierre_cvd_not_LTFU, age <= 60 & age > 50)
pierre_cvd2 <- subset(pierre_cvd_not_LTFU, age <=50 & age > 40)
pierre_cvd1 <- subset(pierre_cvd_not_LTFU, age <= 40)

pierre_60 <- subset(pierre_not_LTFU, age > 60)
pierre_5160 <- subset(pierre_not_LTFU, age <= 60 & age > 50)
pierre_4150 <- subset(pierre_not_LTFU, age <=50 & age > 40)
pierre_40 <- subset(pierre_not_LTFU, age <= 40)

erasme_cvd4 <- subset(erasme_cvd_total, AGE > 60)
erasme_cvd3 <- subset(erasme_cvd_total, AGE <= 60 & AGE > 50)
erasme_cvd2 <- subset(erasme_cvd_total, AGE <=50 & AGE > 40)
erasme_cvd1 <- subset(erasme_cvd_total, AGE <= 40)

erasme_60 <- subset(erasme_not_LTFU, AGE > 60)
erasme_5160 <- subset(erasme_not_LTFU, AGE <= 60 & AGE > 50)
erasme_4150 <- subset(erasme_not_LTFU, AGE <=50 & AGE > 40)
erasme_40 <- subset(erasme_not_LTFU, AGE <= 40)

ghent_cvd4 <- subset(ghent_CVD_not_LTFU, AGE > 60)
ghent_cvd3 <- subset(ghent_CVD_not_LTFU, AGE <= 60 & AGE > 50)
ghent_cvd2 <- subset(ghent_CVD_not_LTFU, AGE <=50 & AGE > 40)
ghent_cvd1 <- subset(ghent_CVD_not_LTFU, AGE <= 40)

ghent_60 <- subset(ghent_not_LTFU, AGE > 60)
ghent_5160 <- subset(ghent_not_LTFU, AGE <= 60 & AGE > 50)
ghent_4150 <- subset(ghent_not_LTFU, AGE <=50 & AGE > 40)
ghent_40 <- subset(ghent_not_LTFU, AGE <= 40)

pop_40 <-  (nrow(liege_40) + nrow(pierre_40) + nrow(erasme_40) + nrow(ghent_40))
pop_4150 <-  (nrow(liege_4150) + nrow(pierre_4150) + nrow(erasme_4150) + nrow(ghent_4150))
pop_5160 <-  (nrow(liege_5160) + nrow(pierre_5160) + nrow(erasme_5160) + nrow(ghent_5160))
pop_60 <-  (nrow(liege_60) + nrow(pierre_60) + nrow(erasme_60) + nrow(ghent_60))

pop_40_cvd <-  (nrow(liege_40) + nrow(pierre_40) + nrow(erasme_40))
pop_4150_cvd <-  (nrow(liege_4150) + nrow(pierre_4150) + nrow(erasme_4150))
pop_5160_cvd <-  (nrow(liege_5160) + nrow(pierre_5160) + nrow(erasme_5160))
pop_60_cvd <-  (nrow(liege_60) + nrow(pierre_60) + nrow(erasme_60))


NICM_age <- function(df1, df2, df3, df4, pop_df) {
  prev <- (nrow(df1) + nrow(df2) + nrow(df3) + nrow(df4))/pop_df
  prev
}

NICM_age_cvd <- function(df1, df2, df3, pop_df_cvd) {
  prev <- (nrow(df1) + nrow(df2) + nrow(df3))/pop_df_cvd
  prev
}

cvd_40 <- NICM_age_cvd(liege_cvd1, pierre_cvd1, erasme_cvd1, pop_40_cvd)
cvd_4150 <- NICM_age_cvd(liege_cvd2, pierre_cvd2, erasme_cvd2, pop_4150_cvd)
cvd_5160 <- NICM_age_cvd(liege_cvd3, pierre_cvd3, erasme_cvd3, pop_5160_cvd)
cvd_60 <- NICM_age_cvd(liege_cvd4, pierre_cvd4, erasme_cvd4, pop_60_cvd)

liege_renal4 <- subset(liege_esrd_ckd, age > 60)
liege_renal3 <- subset(liege_esrd_ckd, age <= 60 & age > 50)
liege_renal2 <- subset(liege_esrd_ckd, age <=50 & age > 40)
liege_renal1 <- subset(liege_esrd_ckd, age <= 40)

pierre_renal_not_LTFU <- subset(pierre_not_LTFU, RENAL_DISEASE == 1)

pierre_renal4 <- subset(pierre_renal_not_LTFU, age > 60)
pierre_renal3 <- subset(pierre_renal_not_LTFU, age <= 60 & age > 50)
pierre_renal2 <- subset(pierre_renal_not_LTFU, age <=50 & age > 40)
pierre_renal1 <- subset(pierre_renal_not_LTFU, age <= 40)

erasme_renal4 <- subset(erasme_renal_total, AGE > 60)
erasme_renal3 <- subset(erasme_renal_total, AGE <= 60 & AGE > 50)
erasme_renal2 <- subset(erasme_renal_total, AGE <=50 & AGE > 40)
erasme_renal1 <- subset(erasme_renal_total, AGE <= 40)

ghent_renal_not_LTFU <- subset(ghent_not_LTFU, RENAL_DISEASE == 1)

ghent_renal4 <- subset(ghent_renal_not_LTFU, AGE > 60)
ghent_renal3 <- subset(ghent_renal_not_LTFU, AGE <= 60 & AGE > 50)
ghent_renal2 <- subset(ghent_renal_not_LTFU, AGE <=50 & AGE > 40)
ghent_renal1 <- subset(ghent_renal_not_LTFU, AGE <= 40)

renal_40 <- NICM_age(liege_renal1, pierre_renal1, erasme_renal1, ghent_renal1, pop_40)
renal_4150 <- NICM_age(liege_renal2, pierre_renal2, erasme_renal2, ghent_renal2, pop_4150)
renal_5160 <- NICM_age(liege_renal3, pierre_renal3, erasme_renal3, ghent_renal3, pop_5160)
renal_60 <- NICM_age(liege_renal4, pierre_renal4, erasme_renal4, ghent_renal4, pop_60)


liege_hcv4 <- subset(liege_hcv_not_LTFU, age > 60)
liege_hcv3 <- subset(liege_hcv_not_LTFU, age <= 60 & age > 50)
liege_hcv2 <- subset(liege_hcv_not_LTFU, age <=50 & age > 40)
liege_hcv1 <- subset(liege_hcv_not_LTFU, age <= 40)


pierre_hcv4 <- subset(pierre_hcv_not_LTFU, age > 60)
pierre_hcv3 <- subset(pierre_hcv_not_LTFU, age <= 60 & age > 50)
pierre_hcv2 <- subset(pierre_hcv_not_LTFU, age <=50 & age > 40)
pierre_hcv1 <- subset(pierre_hcv_not_LTFU, age <= 40)

erasme_hcv_not_LTFU <- subset(erasme_liver_not_LTFU, hepc_yes == 1)

erasme_hcv4 <- subset(erasme_hcv_not_LTFU, AGE > 60)
erasme_hcv3 <- subset(erasme_hcv_not_LTFU, AGE <= 60 & AGE > 50)
erasme_hcv2 <- subset(erasme_hcv_not_LTFU, AGE <=50 & AGE > 40)
erasme_hcv1 <- subset(erasme_hcv_not_LTFU, AGE <= 40)

ghent_hcv4 <- subset(ghent_hepc_not_LTFU, AGE > 60)
ghent_hcv3 <- subset(ghent_hepc_not_LTFU, AGE <= 60 & AGE > 50)
ghent_hcv2 <- subset(ghent_hepc_not_LTFU, AGE <=50 & AGE > 40)
ghent_hcv1 <- subset(ghent_hepc_not_LTFU, AGE <= 40)

hcv_40 <- NICM_age(liege_hcv1, pierre_hcv1, erasme_hcv1, ghent_hcv1, pop_40)
hcv_4150 <- NICM_age(liege_hcv2, pierre_hcv2, erasme_hcv2, ghent_hcv2, pop_4150)
hcv_5160 <- NICM_age(liege_hcv3, pierre_hcv3, erasme_hcv3, ghent_hcv3, pop_5160)
hcv_60 <- NICM_age(liege_hcv4, pierre_hcv4, erasme_hcv4, ghent_hcv4, pop_60)

#diabetes

liege_dia4 <- subset(liege_dia_not_LTFU, age > 60)
liege_dia3 <- subset(liege_dia_not_LTFU, age <= 60 & age > 50)
liege_dia2 <- subset(liege_dia_not_LTFU, age <=50 & age > 40)
liege_dia1 <- subset(liege_dia_not_LTFU, age <= 40)


pierre_dia4 <- subset(pierre_dia_not_LTFU, age > 60)
pierre_dia3 <- subset(pierre_dia_not_LTFU, age <= 60 & age > 50)
pierre_dia2 <- subset(pierre_dia_not_LTFU, age <=50 & age > 40)
pierre_dia1 <- subset(pierre_dia_not_LTFU, age <= 40)

erasme_dia4 <- subset(erasme_diabetes_total, AGE > 60)
erasme_dia3 <- subset(erasme_diabetes_total, AGE <= 60 & AGE > 50)
erasme_dia2 <- subset(erasme_diabetes_total, AGE <=50 & AGE > 40)
erasme_dia1 <- subset(erasme_diabetes_total, AGE <= 40)

ghent_dia4 <- subset(ghent_dia_not_LTFU, AGE > 60)
ghent_dia3 <- subset(ghent_dia_not_LTFU, AGE <= 60 & AGE > 50)
ghent_dia2 <- subset(ghent_dia_not_LTFU, AGE <=50 & AGE > 40)
ghent_dia1 <- subset(ghent_dia_not_LTFU, AGE <= 40)

dia_40 <- NICM_age(liege_dia1, pierre_dia1, erasme_dia1, ghent_dia1, pop_40)
dia_4150 <- NICM_age(liege_dia2, pierre_dia2, erasme_dia2, ghent_dia2, pop_4150)
dia_5160 <- NICM_age(liege_dia3, pierre_dia3, erasme_dia3, ghent_dia3, pop_5160)
dia_60 <- NICM_age(liege_dia4, pierre_dia4, erasme_dia4, ghent_dia4, pop_60)

#anal cancer
liege_AC_not_LTFU <- subset(liege_nadm_not_LTFU, CLIN_EVENT_SPECIFICATION.x == "ANAL")

liege_AC4 <- subset(liege_AC_not_LTFU, age > 60)
liege_AC3 <- subset(liege_AC_not_LTFU, age <= 60 & age > 50)
liege_AC2 <- subset(liege_AC_not_LTFU, age <=50 & age > 40)
liege_AC1 <- subset(liege_AC_not_LTFU, age <= 40)

pierre_AC4 <- subset(pierre_AC, age > 60)
pierre_AC3 <- subset(pierre_AC, age <= 60 & age > 50)
pierre_AC2 <- subset(pierre_AC, age <=50 & age > 40)
pierre_AC1 <- subset(pierre_AC, age <= 40)

erasme_AC4 <- subset(erasme_AC, AGE > 60)
erasme_AC3 <- subset(erasme_AC, AGE <= 60 & AGE > 50)
erasme_AC2 <- subset(erasme_AC, AGE <=50 & AGE > 40)
erasme_AC1 <- subset(erasme_AC, AGE <= 40)

ghent_AC4 <- subset(ghent_AC, AGE > 60)
ghent_AC3 <- subset(ghent_AC, AGE <= 60 & AGE > 50)
ghent_AC2 <- subset(ghent_AC, AGE <=50 & AGE > 40)
ghent_AC1 <- subset(ghent_AC, AGE <= 40)

AC_40 <- NICM_age(liege_AC1, pierre_AC1, erasme_AC1, ghent_AC1, pop_40)
AC_4150 <- NICM_age(liege_AC2, pierre_AC2, erasme_AC2, ghent_AC2, pop_4150)
AC_5160 <- NICM_age(liege_AC3, pierre_AC3, erasme_AC3, ghent_AC3, pop_5160)
AC_60 <- NICM_age(liege_AC4, pierre_AC4, erasme_AC4, ghent_AC4, pop_60)

#HL
liege_HL_not_LTFU <- subset(liege_nadm_not_LTFU, CLIN_EVENT_SPECIFICATION.x == "HDL")

liege_HL4 <- subset(liege_HL_not_LTFU, age > 60)
liege_HL3 <- subset(liege_HL_not_LTFU, age <= 60 & age > 50)
liege_HL2 <- subset(liege_HL_not_LTFU, age <=50 & age > 40)
liege_HL1 <- subset(liege_HL_not_LTFU, age <= 40)

pierre_HL4 <- subset(pierre_HODG_LYMP, age > 60)
pierre_HL3 <- subset(pierre_HODG_LYMP, age <= 60 & age > 50)
pierre_HL2 <- subset(pierre_HODG_LYMP, age <=50 & age > 40)
pierre_HL1 <- subset(pierre_HODG_LYMP, age <= 40)

erasme_HL4 <- subset(erasme_HL, AGE > 60)
erasme_HL3 <- subset(erasme_HL, AGE <= 60 & AGE > 50)
erasme_HL2 <- subset(erasme_HL, AGE <=50 & AGE > 40)
erasme_HL1 <- subset(erasme_HL, AGE <= 40)

ghent_HL4 <- subset(ghent_HL, AGE > 60)
ghent_HL3 <- subset(ghent_HL, AGE <= 60 & AGE > 50)
ghent_HL2 <- subset(ghent_HL, AGE <=50 & AGE > 40)
ghent_HL1 <- subset(ghent_HL, AGE <= 40)

HL_40 <- NICM_age(liege_HL1, pierre_HL1, erasme_HL1, ghent_HL1, pop_40)
HL_4150 <- NICM_age(liege_HL2, pierre_HL2, erasme_HL2, ghent_HL2, pop_4150)
HL_5160 <- NICM_age(liege_HL3, pierre_HL3, erasme_HL3, ghent_HL3, pop_5160)
HL_60 <- NICM_age(liege_HL4, pierre_HL4, erasme_HL4, ghent_HL4, pop_60)


#recalc CVD
#exclude Ghent
nrow(liege_cvd_not_LTFU_no_dups) + nrow(pierre_cvd_not_LTFU) + 
  nrow(erasme_cvd_total) + nrow(ghent_CVD_not_LTFU)
142/(5787-907)
154/5787

nrow(liege_esrd_ckd) + nrow(erasme_renal_total) + nrow(pierre_renal_not_LTFU) + 
  nrow(ghent_renal_not_LTFU)
#make a graph that has the cases by age?

####CD4 nadir, exclude Ghent



#### population over 50
ghent_50 <- subset(ghent_not_LTFU, AGE >= 50)
pierre_50 <- subset(pierre_not_LTFU, age >= 50)
erasme_50 <- subset(erasme_not_LTFU, AGE >= 50)
liege_50 <- subset(liege_not_LTFU, age >= 50)

pop_50 <-  (nrow(liege_50) + nrow(pierre_50) + nrow(erasme_50) + nrow(ghent_50))

###do i need this?

########### Pierre death ----------

pierre_age_death <- as.data.table(pierre_death[c(1, 25)])
pierre_age_death[, id_age_death := 1:.N, by = PATIENT_ID]
pierre_age_death_cast <- dcast(pierre_age_death[,list(PATIENT_ID, id_age_death, age_binned)],
                               PATIENT_ID ~ id_age_death, value.var = 'age_binned', fill = 0)

names(pierre_age_death_cast)[2] <- "age_at_death"
