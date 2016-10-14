
######## Notes

###something to consider: When a patient dies, there is also an event for end of follow up.
#need to check if it's identical to death data

#also, a more nuanced look at what's happening at the events will be necessary.


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
liege_hcv_total <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/Ad hoc Projects/HIV/Data/Belgium/Université de Liège -Sart Tilman/csv/ULG_LAB_VIRO_3.csv', header = T, na.strings=c(""))


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
ghent_px2 <-read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/Ad hoc Projects/HIV/Data/Belgium/Ghent/Ghent_new_data2.csv', header = T, na.strings=c("UNK"))


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
liege_hcv_total <- id(liege_hcv_total)

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

##################### Overall functions ----------------------

comb <- function(df1, df2) {
  master <- as.data.table(merge(df1, df2, by = "PATIENT_ID", all = TRUE))
  master
}

comb_F <- function(df1, df2) {
  master <- as.data.table(merge(df1, df2, by = "PATIENT_ID", all = FALSE))
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

ethnic_freq <- function(df) {
  site_region <- data.frame(table(df$REGION_OF_ORIGIN))
  site_region$Freq <- (site_region$Freq/sum(site_region$Freq))*100
  site_region
}

#############################################################################################
#Liege
###################### Liege - changes to orig. data ------------------

#make dates into as.Date and to change the fake dates to NA

names(liege_BAS)[11] <- 'DATE.START.ART'
liege_BAS[, cols <- grep("DATE", names(liege_BAS))] <- lapply(liege_BAS[, cols <- grep("DATE", names(liege_BAS))], as.Date, format = "%m/%d/%Y")

#subtract birth date from today's date, make new column for age
liege_BAS$sys_date <- Sys.Date()

liege_BAS$age <- age_years(liege_BAS$BIRTH_DATE, liege_BAS$sys_date)

#do the same for the number of years on ART
liege_BAS$years_on_ART <- age_years(liege_BAS$DATE.START.ART, liege_BAS$sys_date)

#change the placeholder to NA
liege_BAS$years_on_ART[liege_BAS$years_on_ART == 104] <- NA

#make the fake placeholder date into NA
liege_BAS[liege_BAS == "1911-11-11"] <- NA

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

liege_comb <- comb(liege_BAS, liege_CEP)
liege_master <- comb(liege_comb, liege_death)

liege_master$GENDER <- gsub("F", "Female", liege_master$GENDER)
liege_master$GENDER <- gsub("M", "Male", liege_master$GENDER)

liege_master <- binning_ages(liege_master)

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
names(liege_master)[5] <- "REGION_OF_ORIGIN"


############## Liege BMI -------------

### find BMI for Liege patients
liege_master$HEIGHT[liege_master$HEIGHT == "999"] <- NA
liege_master$WEIGHT[liege_master$WEIGHT == "999"] <- NA

liege_master$HEIGHT <- liege_master$HEIGHT*liege_master$HEIGHT
liege_master$BMI <- (liege_master$WEIGHT/liege_master$HEIGHT)*10000

liege_master_no_dups <- liege_master[!duplicated(liege_master$PATIENT_ID)]

#################### Liege LTFU -----------------

#living, not LTFU px - Liege
liege_not_LTFU <- subset(liege_master_no_dups, STATUS == "Follow up")
liege_FU_all <- subset(liege_master_no_dups, STATUS != "Contact lost")
liege_FU_all <- subset(liege_FU_all, STATUS != "Transferred")

liege_not_LTFU <- binning_ages(liege_not_LTFU)
mean(liege_not_LTFU$age)
median(liege_not_LTFU$age)

#find patients that are LTFU
liege_LTFU_tmp <- subset(liege_master_no_dups, STATUS != "Follow up")
liege_LTFU_tmp2 <- subset(liege_LTFU_tmp, STATUS != "Death")
liege_LTFU <- subset(liege_LTFU_tmp2, STATUS != "Transferred")

mean(liege_LTFU$age)

ethnic_freq(liege_not_LTFU)

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
liege_CD4_all <- comb_F(liege_CD4_nadir, liege_CD4_avg)
liege_CD4_all <- comb_F(liege_CD4_all, liege_CD4_recent)

#find mean recent CD4 count for px not LTFU
liege_not_LTFU <- comb_F(liege_not_LTFU, liege_CD4_all)
mean(liege_not_LTFU$CD4_RECENT)


####################### Liege NICMs --------------------------

#subset liege hcv to just rna positive

liege_hcv_rna_tmp <- subset(liege_hcv_total, VIROSERO_ID == "HCVR")

liege_hcv_rna <- subset(liege_hcv_rna_tmp, VIROSERO_RESULT != "Non detect", na.rm=FALSE)
liege_hcv_rna <- liege_hcv_rna[!duplicated(liege_hcv_rna$PATIENT_ID),]
liege_hcv_not_LTFU <- comb_F(liege_hcv_rna, liege_not_LTFU)

15/1006


#Subset NICMs to ensure that there are no duplicates

liege_dia <- subset(liege_CEP, CLIN_EVENT_ID == "DIA")
liege_esrd <- subset(liege_CEP, CLIN_EVENT_ID == "ESRD")
liege_fra <- subset(liege_CEP, CLIN_EVENT_ID == "FRA")
liege_nadm <- subset(liege_CEP, CLIN_EVENT_ID == "NADM")

#CVD is a combo of stroke, 
liege_cvd <- subset(liege_CEP, CLIN_EVENT_ID == "STR" | CLIN_EVENT_ID == "ACS" | CLIN_EVENT_ID == "ICP")

liege_cvd_not_LTFU <- comb_F(liege_cvd, liege_not_LTFU)
#eliminate duplicates from liege cvd
liege_cvd_not_LTFU_no_dups <- liege_cvd_not_LTFU[!duplicated(liege_cvd_not_LTFU$PATIENT_ID)]

liege_dia_not_LTFU <- comb_F(liege_dia, liege_not_LTFU)
liege_nadm_not_LTFU <- comb_F(liege_nadm, liege_not_LTFU)

nrow(liege_cvd_not_LTFU_no_dups)/1006
nrow(liege_dia_not_LTFU)/1006
nrow(liege_nadm_not_LTFU)/1006


##### Liege smoking -------

table(liege_not_LTFU$SMOKING)
(251+112)/1006

##### Liege hyp -------

table(liege_not_LTFU$DRUG_HTA)
229/1006


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

liege_egfr_diff <- data.table(merge(liege_egfr_base, liege_egfr_recent, by="PATIENT_ID"))

#VALUE.x and VALUE.y have to be numeric
liege_egfr_diff[VALUE.x == "> 60", VALUE.x := "60"]
liege_egfr_diff[VALUE.y == "> 60", VALUE.y := "60"]

liege_egfr_diff$VALUE.x <- as.numeric(as.character(liege_egfr_diff$VALUE.x))
liege_egfr_diff$VALUE.y <- as.numeric(as.character(liege_egfr_diff$VALUE.y))

liege_egfr_dec <- liege_egfr_diff[, CKD_calc := (VALUE.y <= 0.25*VALUE.x) + 0]
liege_egfr_ckd <- subset(liege_egfr_dec, CKD_calc == 1)
liege_egfr_ckd2 <- subset(liege_egfr_dec, VALUE.x == 60)
liege_egfr_ckd3 <- subset(liege_egfr_ckd2, VALUE.y < 60)

#these are the patients with ckd
liege_ckd <- comb_F(liege_egfr_ckd2, liege_egfr_ckd3)

#merge with not LTFU
liege_ckd_not_LTFU <- comb_F(liege_ckd, liege_not_LTFU)

liege_esrd <- subset(liege_CEP, CLIN_EVENT_ID == "ESRD")
liege_esrd_ckd <- comb(liege_ckd_not_LTFU, liege_esrd)
(nrow(liege_esrd_ckd))/1006

######### Liege drug data ----------------------

names(liege_art)[2] <- "drug_code"

#see how many people are on treatment
liege_started_trt <- comb_F(liege_art, liege_not_LTFU)
liege_started_trt <- liege_started_trt[!duplicated(liege_started_trt$PATIENT_ID),]
nrow(liege_started_trt)/nrow(liege_not_LTFU)


### Liege drug graphs ---------

#subset to just ART drugs that have no end date (i.e. are presumably still being taken)
liege_art2015 <- subset(liege_art, is.na(ART_END_DATE))

liege_art_merge <- merge(liege_artcodes, liege_art2015, by = "drug_code", all.x=TRUE, all.y=TRUE)
liege_art_merge2 <- liege_art_merge %>% count(art_name, wt = NULL)
liege_art_merge2 <- merge(liege_art_merge2, liege_artcodes, by = "art_name", all.x=TRUE, all.y=TRUE)

liege_art_count <- subset(liege_art_merge2, drug_class != "Integrase_inhib")
liege_art_count <- subset(liege_art_count, drug_class != "Entry_inhib")
liege_art_count <- subset(liege_art_count, drug_class != "Other")
liege_art_count <- subset(liege_art_count, drug_class != "Fusion_inhib")


liege_art_count <- liege_art_count[!duplicated(liege_art_count$art_name),]
## Create separate palette for each drug class
liege_art_count$drug_class <- factor(liege_art_count$drug_class)

dropme <- subset(liege_art_count, drug_class != 'Entry_inhib')
dropme2 <- subset(dropme, drug_class != 'Integrase_inhib')
dropme3 <- subset(dropme2, drug_class != 'Fusion_inhib')

liege_art_count <- droplevels(dropme3)

## Add a column for positioning drug labels on graph
liege_art_count = liege_art_count %>% group_by(drug_class) %>%
  mutate(cum.freq = cumsum(n) - 0.5*n)

ncol = table(liege_art_count$drug_class)
repeating.pal = mapply(function(x,y) brewer.pal(x,y), ncol, 
                       c("Set3","Set3","Set3","Set3"))

#repeating.pal[[2]] = repeating.pal[[2]][1:2]  # We only need 2 colors but brewer.pal creates 3 minimum

repeating.pal = unname(unlist(repeating.pal))

liege_art_count_sorted <- liege_art_count[order(liege_art_count$drug_class),]
liege_art_count_sorted$labOrder <- liege_art_count$art_name
liege_art_count_sorted$colours<-repeating.pal

liege_art_names <- ggplot(data = liege_art_count_sorted, aes(x=drug_class, y=n, fill=labOrder) ) + 
  geom_bar(stat="identity", colour="black", lwd=0.2) +
  geom_text(aes(label=paste0(art_name,": ", n), y=cum.freq, size=n), colour="grey20", fontface="bold") +
  xlab("Drug class") + ylab("Number of patients") + 
  ggtitle("Liege ART drug counts") +
  scale_fill_manual(values=liege_art_count_sorted$colours) +
  guides(fill=FALSE) + theme(plot.title = element_text(size = 20))



#end of Liege cleaning

###############################################################################################
#Pierre
####################### Pierre - changes to orig. data -----------------

pierre_px$DOB <- as.Date(pierre_px$DOB, format = "%d/%m/%Y")

pierre_px$DOB[pierre_px$DOB == "1911-11-11"] <- NA

pierre_px$sys_date <- as.Date(format(today, format = "%Y-%m-%d"))

#use previously defined age_years function
pierre_px$age <- age_years(pierre_px$DOB, pierre_px$sys_date)

#limit the database to only patients who were seen after 2011
pierre_px$DTE_END_STUDY <- as.Date(pierre_px$DTE_END_STUDY, format = "%d/%m/%Y")
pierre_px_recent <- subset(pierre_px, pierre_px$DTE_END_STUDY > "2011-12-31")

pierre_px_recent_id <- data.frame("PATIENT_ID" = pierre_px_recent$PATIENT_ID)

#make pierre_events only include the patients from pierre_px_recent
pierre_events_recent <- comb_F(pierre_px_recent_id, pierre_events)

######################### Pierre LTFU -----------

## Create dataframes of the not_LTFU px that should be used for 2015 snapshot

#first subset living patients
pierre_px_recent <- binning_ages(pierre_px_recent)
pierre_alive <- subset(pierre_px_recent, DEATH == "0")


#subset to take a closer look at LTFU

pierre_LTFU <- subset(pierre_alive, pierre_alive$DTE_END_STUDY < "2014-05-31")
pierre_not_LTFU <- subset(pierre_alive, pierre_alive$DTE_END_STUDY > "2014-05-31")
pierre_FU_all <- subset(pierre_px_recent, pierre_px_recent$DTE_END_STUDY > "2014-05-31")

mean(pierre_LTFU$age, na.rm=TRUE)
mean(pierre_not_LTFU$age, na.rm = TRUE)
median(pierre_not_LTFU$age)

pierre_not_LTFU <- binning_ages(pierre_not_LTFU)

########################## Pierre Hepatitis -------------------------------
pierre_events_recent <- as.data.frame(lapply(pierre_events_recent, FUN = function(foo) recode(foo, "c('No', 'N')= 0; 
                                                                                 c('Yes', 'Y')= 1; 'UNK'= NA")))

#old code stopped working ~mystery headache~~
table(pierre_events_recent$HCV_AT_EVENT)
pierre_hcv <- subset(pierre_events_recent, HCV_AT_EVENT == 1)
pierre_hcv <- pierre_hcv[!duplicated(pierre_hcv$PATIENT_ID),]
pierre_hcv_not_LTFU <- comb_F(pierre_hcv, pierre_not_LTFU)

nrow(pierre_hcv_not_LTFU)

################### Pierre CD4 ----------------

pierre_events_recent <- as.data.table(pierre_events_recent)
#for each id, keep the later DTE_EVENT
pierre_events_recent$DTE_EVENT <- as.Date(pierre_events_recent$DTE_EVENT, format = "%d/%m/%Y")
pierre_events_recent_event <- pierre_events_recent[pierre_events_recent[, .I[DTE_EVENT == max(DTE_EVENT)], by=PATIENT_ID]$V1]

#delete duplicates
pierre_events_recent_event <- pierre_events_recent_event[!duplicated(pierre_events_recent_event$PATIENT_ID),]

pierre_cd4_nadir <- pierre_events_recent[!duplicated(pierre_events_recent$PATIENT_ID),]

#combine into one cd4 dataframe
pierre_cd4_all <- data.frame(PATIENT_ID=pierre_cd4_nadir$PATIENT_ID, cd4_recent = pierre_events_recent_event$CD4_AT_EVENT, cd4_nadir = pierre_cd4_nadir$CD4_NADIR_EVENT)
pierre_not_LTFU <- comb_F(pierre_cd4_all, pierre_not_LTFU)

mean(pierre_not_LTFU$cd4_recent, na.rm=TRUE)

table(pierre_not_LTFU$GENDER)

########### Pierre hyp ------------------
##treated for hypertension

pierre_hyp <- as.data.table(pierre_events_recent)
pierre_hyp[, id_hyp := 1:.N, by = PATIENT_ID]
pierre_hyp_cast <- dcast(pierre_hyp[,list(PATIENT_ID, id_hyp, TRT_HYPERTENSION_AT_EVENT)], PATIENT_ID ~ id_hyp, value.var = 'TRT_HYPERTENSION_AT_EVENT', fill = 0)

pierre_hyp_cast <- as.data.table(pierre_hyp_cast)
pierre_hyp_cast <- pierre_hyp_cast[, lapply(.SD, as.numeric), by = PATIENT_ID]

pierre_hyp_cast <- dichotomous(pierre_hyp_cast)
colnames(pierre_hyp_cast)[9] <- "hyp_yes"
pierre_hyp_cast <- pierre_hyp_cast[ , paste0(c("1", "2", "3", "4", "5", "6", "sum_col")) := NULL]

pierre_hyp_not_LTFU <- comb_F(pierre_hyp_cast, pierre_not_LTFU)
540/(nrow(pierre_not_LTFU))


###### Pierre smoking -------------------

pierre_smoke <- as.data.table(pierre_events_recent)
pierre_smoke[, id_smoke := 1:.N, by = PATIENT_ID]
pierre_smoke_cast <- dcast(pierre_smoke[,list(PATIENT_ID, id_smoke, EVER_SMOKED_AT_EVENT)], PATIENT_ID ~ id_smoke, value.var = 'EVER_SMOKED_AT_EVENT', fill = 0)

pierre_smoke_cast <- as.data.table(pierre_smoke_cast)
pierre_smoke_cast <- pierre_smoke_cast[, lapply(.SD, as.numeric), by = PATIENT_ID]

pierre_smoke_cast <- dichotomous(pierre_smoke_cast)
colnames(pierre_smoke_cast)[9] <- "smoke_yes"
pierre_smoke_cast <- pierre_smoke_cast[ , paste0(c("1", "2", "3", "4", "5", "6", "sum_col")) := NULL]

pierre_smoke_not_LTFU <- comb_F(pierre_smoke_cast, pierre_not_LTFU)
table(pierre_smoke_not_LTFU$var_yes)
1212/(nrow(pierre_not_LTFU))

########################## Pierre BMI ----------------------

pierre_bmi <- as.data.table(pierre_events_recent)
pierre_bmi[, id_bmi := 1:.N, by = PATIENT_ID]
pierre_bmi_cast <- dcast(pierre_bmi[,list(PATIENT_ID, id_bmi, BMI_AT_EVENT)], PATIENT_ID ~ id_bmi, value.var = 'BMI_AT_EVENT', fill = 0)

bmi_avg <- data.frame(round(Reduce(`+`, pierre_bmi_cast[-1]) / rowSums(pierre_bmi_cast[-1] != 0), 2))
names(bmi_avg)[1] <- "avg"

pierre_bmi_avg <- as.data.frame(cbind(pierre_bmi_cast$PATIENT_ID, bmi_avg$avg))
colnames(pierre_bmi_avg)[c(1:2)] <- c("PATIENT_ID", "avg")

################### Pierre treatment ------------------
#Started treatment at event

pierre_trt <- as.data.table(pierre_events_recent)
pierre_trt[, id_trt := 1:.N, by = PATIENT_ID]
pierre_trt_cast <- dcast(pierre_trt[,list(PATIENT_ID, id_trt, STARTED_TREATMENT_AT_EVENT)], PATIENT_ID ~ id_trt, value.var = 'STARTED_TREATMENT_AT_EVENT', fill = 0)

pierre_trt_cast <- as.data.table(pierre_trt_cast)
pierre_trt_cast <- pierre_trt_cast[, lapply(.SD, as.numeric), by = PATIENT_ID]

pierre_trt_cast <- dichotomous(pierre_trt_cast)
colnames(pierre_trt_cast)[9] <- "trt_yes"
pierre_trt_cast <- pierre_trt_cast[ , paste0(c("1", "2", "3", "4", "5", "6", "sum_col")) := NULL]

pierre_trt_not_LTFU <- comb_F(pierre_trt_cast, pierre_not_LTFU)
table(pierre_trt_not_LTFU$var_yes)
2835/(nrow(pierre_not_LTFU))

########### Pierre death ----------

pierre_age_death <- as.data.table(pierre_death[c(1, 25)])
pierre_age_death[, id_age_death := 1:.N, by = PATIENT_ID]
pierre_age_death_cast <- dcast(pierre_age_death[,list(PATIENT_ID, id_age_death, age_binned)],
                               PATIENT_ID ~ id_age_death, value.var = 'age_binned', fill = 0)

names(pierre_age_death_cast)[2] <- "age_at_death"

################ Create Pierre master ----------

pierre_not_LTFU$REGION_OF_ORIGIN <- gsub("North Africa", "North Africa/Middle East", pierre_not_LTFU$REGION_OF_ORIGIN)

pierre_not_LTFU <- comb_F(pierre_not_LTFU, pierre_cd4_all)
pierre_not_LTFU <- comb_F(pierre_not_LTFU, pierre_hepc_cast)
pierre_not_LTFU <- comb_F(pierre_not_LTFU, pierre_hepb_cast)
pierre_not_LTFU <- comb_F(pierre_not_LTFU, pierre_bmi_cast)
pierre_not_LTFU <- comb_F(pierre_not_LTFU, pierre_hyp_cast)
pierre_not_LTFU <- comb_F(pierre_not_LTFU, pierre_trt_cast)
pierre_not_LTFU <- comb_F(pierre_not_LTFU, pierre_smoke_cast)
pierre_not_LTFU <- comb_F(pierre_not_LTFU, pierre_age_death_cast)

ethnic_freq(pierre_not_LTFU)

#################### Pierre NICMs -----------------------

pierre_cvd <- pierre_events_recent[grep("2 - CVD", pierre_events_recent$OUTCOME), ]
pierre_cvd <- pierre_cvd[(pierre_cvd$OUTCOME != "2 - CVD - HYPCHOL"),]
pierre_cvd <- pierre_cvd[!duplicated(pierre_cvd$PATIENT_ID)]

pierre_not_LTFU <- pierre_not_LTFU[ , paste0(c("1", "2", "3", "4", "5", "6", "7.x", "7.y", "var_yes.x", "var_yes.y")) := NULL]
pierre_not_LTFU <- pierre_not_LTFU[ , paste0(c("7.x", "7.y")) := NULL]

pierre_cvd_not_LTFU <- comb_F(pierre_cvd, pierre_not_LTFU)

table(duplicated(pierre_cvd_not_LTFU$PATIENT_ID))
nrow(pierre_cvd_not_LTFU) 70
(nrow(pierre_cvd_not_LTFU))/(nrow(pierre_not_LTFU))

pierre_dia <- pierre_events_recent[grep("4 - DIABETE", pierre_events_recent$OUTCOME), ]
pierre_dia_not_LTFU <- comb_F(pierre_dia, pierre_not_LTFU)

nrow(pierre_dia_not_LTFU) 126
(nrow(pierre_dia_not_LTFU))/(nrow(pierre_not_LTFU))


#create a new column that is Non-AIDS defining malignancies to match Liege data
pierre_not_LTFU$NADM <- pierre_not_LTFU$LUNG_CANCER + 
  pierre_not_LTFU$ANAL_CANCER + pierre_not_LTFU$HODG_LYMP + pierre_not_LTFU$LIVER_CANCER
23/(nrow(pierre_not_LTFU))

#do table to do manual calcs for the individual types of cancers
#subset AC
pierre_AC <- subset(pierre_not_LTFU, ANAL_CANCER == 1)
pierre_HODG_LYMP <- subset(pierre_not_LTFU, HODG_LYMP == 1)

table(pierre_not_LTFU$RENAL_DISEASE)
301/(nrow(pierre_not_LTFU))

################## Pierre drug data --------------------------------

#find out how many patients not LTFU have started treatment
pierre_events_not_LTFU <- comb_F(pierre_not_LTFU, pierre_events)

#library("plyr")
pierre_events_started_trt <-ddply(pierre_events_not_LTFU,.(PATIENT_ID),summarize, 
                                  started_trt=max(STARTED_TREATMENT_AT_EVENT))

pierre_renal <- subset(pierre_events_recent, OUTCOME == "1 - RENAL DISEASE")
pierre_cvd <- subset(pierre_events_recent, OUTCOME == "2 - CVD")


p_renal1 <- ggplot(data=pierre_renal, aes(x=DURATION_PI_MONTHS_AT_EVENT, y=AGE_AT_EVENT)) +
  geom_point() +
  xlab("Duration of PI exposure at event (months)") +
  ylab("Age at event") +
  ggtitle("St. Pierre - Renal")

p_renal2 <- ggplot(data=pierre_renal, aes(x=DURATION_NRTI_MONTHS_AT_EVENT, y=AGE_AT_EVENT)) +
  geom_point() +
  xlab("Duration of NRTI exposure at event (months)") +
  ylab("Age at event") 

p_renal3 <- ggplot(data=pierre_renal, aes(x=DURATION_NNRTI_MONTHS_AT_EVENT, y=AGE_AT_EVENT)) +
  geom_point() + 
  xlab("Duration of NRTI exposure at event (months)") +
  ylab("Age at event")

grid.arrange(p_renal1, p_renal2, p_renal3)

p_cvd1 <- ggplot(data=pierre_cvd, aes(x=DURATION_PI_MONTHS_AT_EVENT, y=AGE_AT_EVENT)) +
  geom_point() +
  xlab("Duration of PI exposure at event (months)") +
  ylab("Age at event") +
  ggtitle("St. Pierre - CVD")

p_cvd2 <- ggplot(data=pierre_cvd, aes(x=DURATION_NRTI_MONTHS_AT_EVENT, y=AGE_AT_EVENT)) +
  geom_point() + 
  xlab("Duration of NRTI exposure at event (months)") +
  ylab("Age at event")

p_cvd3 <- ggplot(data=pierre_cvd, aes(x=DURATION_NNRTI_MONTHS_AT_EVENT, y=AGE_AT_EVENT)) +
  geom_point() + 
  xlab("Duration of NNRTI exposure at event (months)") +
  ylab("Age at event")

grid.arrange(p_cvd1, p_cvd2, p_cvd3)


                                                                                                                                                                                   
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

#limit it to px who have been followed up with since 2012
erasme_master <- erasme_master[erasme_master$LAST_VISIT > as.Date("2011-12-31")]

erasme_master$status[erasme_master$LAST_VISIT > as.Date("2014-05-31")] <- "Follow up"
erasme_master$status[erasme_master$LAST_VISIT < as.Date("2014-05-31")] <- "LTFU"
erasme_master$status[erasme_master$DEATH_Date > as.Date("2000-07-01")] <- "Death"


############# Erasme nadir  ---------------

erasme_nadir$cd4_nadir_v <- as.numeric(erasme_nadir$cd4_nadir_v)
mean(erasme_nadir$cd4_nadir_v, na.rm=TRUE)
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

erasme_master <- comb_F(erasme_master, erasme_bmi_recent)


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

###### Erasme LTFU -----

erasme_LTFU <- subset(erasme_master, status == "LTFU")
erasme_not_LTFU <- subset(erasme_master, status == "Follow up")
erasme_FU_all <- subset(erasme_master, status != "LTFU")

erasme_not_LTFU_cd4 <- comb_F(erasme_not_LTFU, erasme_cd4)
mean(erasme_not_LTFU_cd4$last.cd4_value, na.rm=TRUE)

##### Erasme NICMs --------

erasme_cvd <- erasme_cvd[!duplicated(erasme_cvd$PATIENT_ID),]
erasme_cvd_total <- comb_F(erasme_cvd, erasme_not_LTFU)
(nrow(erasme_cvd_total))/(nrow(erasme_not_LTFU))

erasme_renal <- erasme_renal[!duplicated(erasme_renal$PATIENT_ID),]
erasme_renal_total <- comb_F(erasme_renal, erasme_not_LTFU)
(nrow(erasme_renal_total))/(nrow(erasme_not_LTFU))

erasme_diabetes <- erasme_diabetes[!duplicated(erasme_diabetes$PATIENT_ID),]
erasme_diabetes_total <- comb_F(erasme_diabetes, erasme_not_LTFU)
(nrow(erasme_diabetes_total))/(nrow(erasme_not_LTFU))

#only includes HL and anal cancer
erasme_cancer <- erasme_cancer[!duplicated(erasme_cancer$PATIENT_ID),]
erasme_cancer_total <- comb_F(erasme_cancer, erasme_not_LTFU)
(nrow(erasme_cancer_total))/(nrow(erasme_not_LTFU))

#find the prevalence of anal cancer and HL separately
erasme_HL <- subset(erasme_cancer_total, specCode == 1)
erasme_AC <- subset(erasme_cancer_total, specCode == 0)
(nrow(erasme_HL))/(nrow(erasme_not_LTFU))
(nrow(erasme_AC))/(nrow(erasme_not_LTFU))

#find AC in males
AC_male <- merge(erasme_AC, erasme_not_LTFU, by = "PATIENT_ID")
#they are all male

######## erasme smoking -------------

table(erasme_not_LTFU$HAS_SMOKED)

###### erasme hyp ----------

erasme_hyp_not_LTFU <- comb_F(erasme_hta, erasme_not_LTFU)
110/(nrow(erasme_not_LTFU))

######################erasme liver -------------------

#make new columns for HCV and HBV status

erasme_liver$hepb_yes <- ifelse(erasme_liver$Medical.history.report == "Hepatite B chronique", 1, 0)
erasme_liver$hepc_yes <- ifelse(erasme_liver$Medical.history.report == "Hepatite C chronique", 1, 0)

#merge with erasme_master
erasme_liver_not_LTFU <- comb(erasme_not_LTFU, erasme_liver)

#make the NAs 0 for hepb and hepc

erasme_liver_not_LTFU$hepb_yes[is.na(erasme_master$hepb_yes)] <- 0
erasme_liver_not_LTFU$hepc_yes[is.na(erasme_master$hepc_yes)] <- 0

table(erasme_liver_not_LTFU$hepc_yes)


########################### erasme drug data -----------------


#find the maximum value of years on any ART for each patient
erasme_years_ART <- aggregate(art_duration ~ PATIENT_ID, data = erasme_ART, max)
erasme_years_ART_not_LTFU <- comb_F(erasme_years_ART, erasme_not_LTFU)
mean(erasme_years_ART_not_LTFU$art_duration)

erasme_art_tmp <- subset(erasme_ART, !is.na(art_start.date))

erasme_started_trt <- comb_F(erasme_not_LTFU, erasme_art_tmp)
erasme_started_trt <- erasme_started_trt[!duplicated(erasme_started_trt$PATIENT_ID)]

####combine cd4 with it 

erasme_not_LTFU <- comb_F(erasme_not_LTFU, erasme_cd4)

###Notes

###when comining liver data with base dataset, 0 = no liver disease
#mortality data: need to divide the cause specific mortality rate by the number of
# px from each age group in the cohort, graph next to the normal background mortality
#of Belgium, saved in Excel
#table on p 60 of CDC document could be a good model for summary statistics table

#case rates by region of origin? (cases per 100,000 population)

####### erasme drug graphs ----------

erasme_ART <- as.data.table(erasme_ART)
erasme_ART[, id := 1:.N, by = PATIENT_ID]
erasme_ART_cast <- dcast(erasme_ART[,list(PATIENT_ID, id, art_name)], PATIENT_ID ~ id, value.var = 'art_name', fill = 0)

erasme_ART_num <- subset(erasme_ART, art_name != is.na(art_name))


erasme_ART$art_start.date <- as.Date(erasme_ART$art_start.date, 
                                     format = "%d-%m-%Y")
erasme_ART$art_end.date <- as.Date(erasme_ART$art_end.date, 
                                   format = "%d-%m-%Y")

#subset to just the drugs that patients are still on (no end date)
erasme_ART2015 <- subset(erasme_ART, is.na(erasme_ART$art_end.date))

erasme_ART2015 <- as.data.table(erasme_ART2015)
erasme_ART2015[, id := 1:.N, by = PATIENT_ID]
erasme_ART2015_cast <- dcast(erasme_ART2015[,list(PATIENT_ID, id, art_name)], PATIENT_ID ~ id, value.var = 'art_name', fill = 0)

#make the ID column a factor instead of integer
erasme_ART2015$PATIENT_ID <- factor(erasme_ART2015$PATIENT_ID)    

#add 1 to every row in column art_duration
#be careful to only do this once
erasme_ART2015$art_duration <- erasme_ART2015$art_duration + 1

#exclude rows where data is not available for both the art duration and the drug name
erasme_ART2015 <- as.data.frame(erasme_ART2015)
erasme_ART2015 <- erasme_ART2015[!is.na(erasme_ART2015["art_duration"]),]

erasme_ART2015$art_name <- as.character(erasme_ART2015$art_name)

erasme_art_count <- count(erasme_ART2015, vars = 'art_name')
#add a column that specifies the drug class
erasme_art_count$drug_class <- vector(mode='character', length=nrow(erasme_art_count))

#only include drugs which appear in erasme_art_count
erasme_art_count$drug_class[erasme_art_count$art_name %in% c('Invirase', 'Norvir', 'Prezista', 'Reyataz', 
                                                             'Rezolsta', 'Telzir', 'Tivicay')] <- 'PI'

erasme_art_count$drug_class[erasme_art_count$art_name %in% c('Edurant' , 'Intelence', 'Stocrin',
                                                             'Viramune')] <- 'NNRTI'


erasme_art_count$drug_class[erasme_art_count$art_name %in% c('Emtriva', 'Epivir', 'Kivexa', 'Retrovir', 'Videx',
                                                             'Videz', 'Viread', 'Zerit', 'Ziagen')] <- 'NRTI'


erasme_art_count$drug_class[erasme_art_count$art_name %in% c('Atripla', 'Combivir', 'Eviplera',
                                                             'Kaletra', 'Stribild', 'Triumeq',
                                                             'Trizivir', 'Truvada')] <- 'comb'

erasme_art_count$drug_class[erasme_art_count$art_name %in% c('Celsentri')] <- 'Entry_inhib'
#erasme_art_count$drug_class[erasme_art_count$art_name %in% c('Fuzeon')] <- 'Fusion_inhib'
erasme_art_count$drug_class[erasme_art_count$art_name %in% c('Isentress')] <- 'Integrase_inhib'
erasme_art_count$drug_class[erasme_art_count$art_name %in% c('Other test drug')] <- 'Other_test_drug'

erasme_art_count$drug_class <- factor(erasme_art_count$drug_class)

dropme <- subset(erasme_art_count, drug_class != 'Entry_inhib')
dropme2 <- subset(dropme, drug_class != 'Integrase_inhib')
dropme3 <- subset(dropme2, drug_class != 'Other_test_drug')

erasme_art_count <- droplevels(dropme3)

#second solution from stackoverflow

#library(dplyr) # for the chaining (%>%) operator

## Add a column for positioning drug labels on graph
erasme_art_count = erasme_art_count %>% group_by(drug_class) %>%
  mutate(cum.freq = cumsum(freq) - 0.5*freq)

ncol = table(erasme_art_count$drug_class)
repeating.pal = mapply(function(x,y) brewer.pal(x,y), ncol, 
                       c("Set3","Set3","Set3","Set3"))

#repeating.pal[[2]] = repeating.pal[[2]][1:2]  # We only need 2 colors but brewer.pal creates 3 minimum

repeating.pal = unname(unlist(repeating.pal))

erasme_art_count_sorted <- erasme_art_count[order(erasme_art_count$drug_class),]
erasme_art_count_sorted$labOrder <- erasme_art_count$art_name
erasme_art_count_sorted$colours<-repeating.pal

erasme_art_names <- ggplot(data = erasme_art_count_sorted, aes(x=drug_class, y=freq, fill=labOrder) ) + 
  geom_bar(stat="identity", colour="black", lwd=0.2) +
  geom_text(aes(label=paste0(art_name,": ", freq), y=cum.freq, size=freq), colour="grey20", fontface="bold") +
  xlab("Drug class") + ylab("Number of patients") + 
  ggtitle("Erasme ART drug counts") +
  scale_fill_manual(values=erasme_art_count_sorted$colours) +
  guides(fill=FALSE) + theme(plot.title = element_text(size = 20))


#find average duration on ART by patient
erasme_ART$art_duration <- erasme_ART$art_duration + 1


#make dataframe that is ART patients who are alive
erasme_alive_art <- merge(erasme_alive, erasme_ART, by = "PATIENT_ID", all.x=FALSE)
#find the max value in art_duration for each px
erasme_alive_art <- as.data.table(erasme_alive_art)
erasme_alive_art[, id := 1:.N, by = PATIENT_ID]
erasme_alive_art <- dcast(erasme_alive_art[,list(PATIENT_ID, id, art_duration)], PATIENT_ID ~ id, value.var = 'art_duration', fill = 0)

erasme_alive_art$max <- apply(erasme_alive_art[, 2:37], 1, max)
mean(erasme_alive_art$max)

mean(liege_not_LTFU$years_on_ART, na.rm=TRUE)

#end of Erasme cleaning
##############################################################################################
###### Ghent master ---------

#easiest may be to subset just the date of last visit and merge it with ghent_new

ghent_last_vis <- data.frame(PATIENT_ID = ghent_px1$PATIENT_ID, LAST_VISIT_DATE = as.Date(ghent_px1$LAST_VISIT_DATE))
ghent_last_vis <- ghent_last_vis[!duplicated(ghent_last_vis),]

ghent_px_master <- as.data.table(merge(ghent_px2, ghent_last_vis, by="PATIENT_ID"))

#limit to 2012 and on

ghent_px_master <- ghent_px_master[ghent_px_master$LAST_VISIT_DATE > as.Date("2011-12-31")]

ghent_px_master$status[ghent_px_master$LAST_VISIT_DATE > as.Date("2014-05-31")] <- "Follow up"
ghent_px_master$status[ghent_px_master$LAST_VISIT_DATE < as.Date("2014-05-31")] <- "LTFU"

ghent_px_master$DATE_DEATH <- as.Date(ghent_px_master$DATE_DEATH, format = "%Y-%m-%d")
ghent_px_master$status[ghent_px_master$DATE_DEATH > as.Date("2000-07-01")] <- "Death"

ghent_px_master$CURRENT.LEGAL.SEX <- gsub("M", "Male", ghent_px_master$CURRENT.LEGAL.SEX)
ghent_px_master$CURRENT.LEGAL.SEX <- gsub("F", "Female", ghent_px_master$CURRENT.LEGAL.SEX)


################## Ghent ethnic --------------

table(ghent_px_master$REGION.OF.ORIGIN..10.White..20.Black..21.Black.African.22.Black.Carribean..30.Hispanic..40.Asian.50.American.60.Indigenous.97.other.99.unknown.)
names(ghent_px_master)[5] <- "REGION_OF_ORIGIN"

ghent_px_master$REGION_OF_ORIGIN[ghent_px_master$REGION_OF_ORIGIN == 10] <- "Western Europe"
ghent_px_master$REGION_OF_ORIGIN[ghent_px_master$REGION_OF_ORIGIN == 20] <- "Western Europe"
ghent_px_master$REGION_OF_ORIGIN[ghent_px_master$REGION_OF_ORIGIN == 21] <- "Sub-Saharan Africa"
ghent_px_master$REGION_OF_ORIGIN[ghent_px_master$REGION_OF_ORIGIN == 22] <- "Latin America/Caribbean"
ghent_px_master$REGION_OF_ORIGIN[ghent_px_master$REGION_OF_ORIGIN == 30] <- "Latin America/Caribbean"
ghent_px_master$REGION_OF_ORIGIN[ghent_px_master$REGION_OF_ORIGIN == 40] <- "Asia"
ghent_px_master$REGION_OF_ORIGIN[ghent_px_master$REGION_OF_ORIGIN == 50] <- "North America"
ghent_px_master$REGION_OF_ORIGIN[ghent_px_master$REGION_OF_ORIGIN == 97] <- "Other/unknown"
ghent_px_master$REGION_OF_ORIGIN[ghent_px_master$REGION_OF_ORIGIN == 99] <- "Other/unknown"

################## Ghent LTFU -----------

ghent_LTFU <- subset(ghent_px_master, status == "LTFU")
ghent_not_LTFU <- subset(ghent_px_master, status == "Follow up")
ghent_FU_all <- subset(ghent_px_master, status != "LTFU")

mean(ghent_LTFU$AGE, na.rm=TRUE)
mean(ghent_not_LTFU$AGE, na.rm=TRUE)

ghent_not_LTFU$MOST_RECENT_CD4 <- as.character(ghent_not_LTFU$MOST_RECENT_CD4)
ghent_not_LTFU$MOST_RECENT_CD4 <- as.numeric(ghent_not_LTFU$MOST_RECENT_CD4)

mean(ghent_not_LTFU$MOST_RECENT_CD4, na.rm=TRUE)
table(ghent_not_LTFU$CURRENT.LEGAL.SEX)
names(ghent_not_LTFU)[4] <- "GENDER"

################### Ghent NICM ------------

ghent_not_LTFU$nadm <- ghent_not_LTFU$HL + ghent_not_LTFU$AC + ghent_not_LTFU$LC +
  ghent_not_LTFU$CC 

#subset AC and HL
ghent_AC <- subset(ghent_not_LTFU, AC == 1)
ghent_HL <- subset(ghent_not_LTFU, HL == 1)

ghent_hepc <- subset(ghent_px1, HCVRNA_AT.EVENT == "POS")
ghent_hepc_not_LTFU <- comb_F(ghent_hepc, ghent_not_LTFU)


ghent_not_LTFU$CVD <- ghent_not_LTFU$MI + ghent_not_LTFU$STROKE + ghent_not_LTFU$ICP
ghent_CVD_not_LTFU <- subset(ghent_not_LTFU, CVD == 1)

ghent_dia_not_LTFU <- subset(ghent_not_LTFU, DIABETES == 1)
########## Ghent hyp -------
names(ghent_events)[2] <- "OUTCOME"
table(ghent_events$OUTCOME)
ghent_events_not_LTFU <- comb_F(ghent_events, ghent_not_LTFU)

ghent_hyp_not_LTFU <- subset(ghent_events_not_LTFU, OUTCOME == 6)
105/907

######### Ghent years on ART ---------
ghent_date_diff <-read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/HIV/Data/Belgium/Ghent/Ghent_date_calcs.csv', header = T, stringsAsFactors = FALSE)
ghent_date_diff$date_diff_y <- as.numeric(ghent_date_diff$date_diff_y)

ghent_date_diff_not_LTFU <- comb_F(ghent_date_diff, ghent_not_LTFU)
mean(ghent_date_diff_not_LTFU$date_diff_y, na.rm=TRUE)

####### all sites hyp -------

#liege
252/1006

#pierre
540/2992

#erasme
110/882

#ghent
105/907

(252+540+110+105)/(1006+2992+882+907)


################# all sites mortality ----------------------

#liege_master all are post 2012
#pierre_dead is only recent
#erasme_dead
#ghent_px_master

#change the age col name in ghent so the function can be used
names(ghent_px_master)[3] <- "age"
ghent_px_master <- binning_ages(ghent_px_master)
names(erasme_master)[5] <- "age"
erasme_master <- binning_ages(erasme_master)

erasme_dead <- subset(erasme_master, status == "Death")
ghent_dead <- subset(ghent_px_master, status == "Death")
liege_dead <- subset(liege_master, STATUS == "Death")
pierre_dead <- subset(pierre_px_recent, DEATH == "1")

pierre_dead <- binning_ages(pierre_dead)
erasme_dead <- binning_ages(erasme_dead)
ghent_dead <- binning_ages(ghent_dead)

###calculate person years
#erasme
erasme_dead$Seroconversion.date.or.first.contact.with.AIDS.center.if.seroconversion.date.unknown <- as.Date(erasme_dead$Seroconversion.date.or.first.contact.with.AIDS.center.if.seroconversion.date.unknown, format = "%d-%m-%Y")
names(erasme_dead)[2] <- "DTE_START_STUDY"
erasme_dead_2014 <- subset(erasme_dead, DEATH_Date > "2014-05-31")

liege_dead$DEATH_DATE <- as.Date(liege_dead$DEATH_DATE, format = "%m/%d/%Y")
liege_dead_2014 <- subset(liege_dead, DEATH_DATE > "2014-05-31")

pierre_dead$DTE_START_STUDY <- as.Date(pierre_dead$DTE_START_STUDY, format = "%m/%d/%Y")
pierre_dead_2014 <- subset(pierre_dead, DTE_END_STUDY > "2014-05-31")

names(ghent_dead)[7] <- "DTE_START_STUDY"
ghent_dead$DTE_START_STUDY <- as.Date(as.character(ghent_dead$DTE_START_STUDY, format = "%m/%d/%Y"))
ghent_dead_2014 <- subset(ghent_dead, DATE_DEATH > "2014-05-31")
ghent_dead_2014 <- as.data.frame(ghent_dead_2014)


#ghent_px_master
#make new df that combined dead with not LTFU
names(ghent_FU_all)[3] <- "age"
erasme_FU_all <- binning_ages(erasme_FU_all)
ghent_FU_all <- binning_ages(ghent_FU_all)
liege_FU_all <- binning_ages(liege_FU_all)
pierre_FU_all <- binning_ages(pierre_FU_all)

#stack overflow
liege_dead_2014$person_months <- round(ifelse(liege_dead_2014$FIRST_VIS_DATE<as.Date("2014-05-31"), 
                                              liege_dead_2014$DEATH_DATE-as.Date("2014-05-31"), liege_dead_2014$DEATH_DATE-liege_dead_2014$FIRST_VIS_DATE)/30, 2)
incidence_death_liege <- (nrow(liege_dead_2014)/sum(liege_dead_2014$person_months))*100

pierre_dead_2014$person_months <- round(ifelse(pierre_dead_2014$DTE_START_STUDY<as.Date("2014-05-31"), 
                                               pierre_dead_2014$DTE_END_STUDY-as.Date("2014-05-31"), pierre_dead_2014$DTE_END_STUDY-pierre_dead_2014$DTE_START_STUDY)/30, 2)
incidence_death_pierre <- (nrow(pierre_dead_2014)/sum(pierre_dead_2014$person_months, na.rm=TRUE))*100


erasme_dead_2014$person_months <- round(ifelse(erasme_dead_2014$DTE_START_STUDY<as.Date("2014-05-31"), 
                                               erasme_dead_2014$DEATH_Date-as.Date("2014-05-31"), erasme_dead_2014$DEATH_Date-erasme_dead_2014$DTE_START_STUDY)/30, 2)
incidence_death_erasme <- (nrow(erasme_dead_2014)/sum(erasme_dead_2014$person_months, na.rm=TRUE))*100


ghent_dead_2014$person_months <- round(ifelse(ghent_dead_2014$DTE_START_STUDY<as.Date("2014-05-31"), 
                                              ghent_dead_2014$DATE_DEATH-as.Date("2014-05-31"), ghent_dead_2014$DATE_DEATH-ghent_dead_2014$DTE_START_STUDY)/30, 2)
incidence_death_ghent <- (nrow(ghent_dead_2014)/sum(ghent_dead_2014$person_months, na.rm=TRUE))*100

#########


tab <- function(df) {
  base <- as.data.frame(table(age_binned = df$age_binned))
  base
}

#erasme needs age_binned
erasme_FU_all$AGE <- as.numeric(erasme_FU_all$AGE)
names(erasme_FU_all)[5] <- "age"
erasme_FU_all <- binning_ages(erasme_FU_all)

pierre_tab <- tab(pierre_FU_all)
pierre_tab_death <- tab(pierre_dead_2014)
liege_tab <- tab(liege_FU_all)
liege_tab_death <- tab(liege_dead_2014)
erasme_tab <- tab(erasme_FU_all)
erasme_tab_death <- tab(erasme_dead_2014)
ghent_tab <- tab(ghent_FU_all)
ghent_tab_death <- tab(ghent_dead_2014)

# pierre_pm <- aggregate(person_months~age_binned, pierre_dead_2014, sum)
# pierre_tab_death_pm <- merge(pierre_pm, pierre_tab_death, by="age_binned", all=TRUE)
# 
# liege_pm <- aggregate(person_months~age_binned, liege_dead_2014, sum)
# liege_tab_death_pm <- merge(liege_pm, liege_tab_death, by="age_binned", all=TRUE)
# 
# erasme_pm <- aggregate(person_months~age_binned, erasme_dead_2014, sum)
# erasme_tab_death_pm <- merge(erasme_pm, erasme_tab_death, by="age_binned", all=TRUE)
# 
# ghent_pm <- aggregate(person_months~age_binned, ghent_dead_2014, sum)
# ghent_tab_death_pm <- merge(ghent_pm, ghent_tab_death, by="age_binned", all=TRUE)
# 

# for (i in liege_tab_death) {
#   liege_tab_death[3] <- i/sum(liege_tab_death$Freq)
#   liege_tab_death
# }
# 
# for (i in pierre_tab_death) {
#   pierre_tab_death[3] <- i/sum(pierre_tab_death$Freq)
#   pierre_tab_death
# }
# 
# for (i in erasme_tab_death) {
#   erasme_tab_death[3] <- i/(sum(erasme_tab_death$Freq))
#   erasme_tab_death
# }
# 
# for (i in ghent_tab_death) {
#   ghent_tab_death[3] <- i/(sum(ghent_tab_death$Freq))
#   ghent_tab_death
# }
# 
# for (i in liege_tab) {
#   liege_tab[3] <- i/sum(liege_tab$Freq)
#   liege_tab
# }
# 
# for (i in pierre_tab) {
#   pierre_tab[3] <- i/sum(pierre_tab$Freq)
#   pierre_tab
# }
# 
# for (i in erasme_tab) {
#   erasme_tab[3] <- i/(sum(erasme_tab$Freq))
#   erasme_tab
# }
# 
# for (i in ghent_tab) {
#   ghent_tab[3] <- i/(sum(ghent_tab$Freq))
#   ghent_tab
# }

avg_pops_df <- as.data.frame(cbind(liege_tab[1], liege_tab$Freq, pierre_tab$Freq, 
                                   erasme_tab$Freq, ghent_tab$Freq))

avg_pm_df <- as.data.frame(cbind(liege_tab[1], liege_tab_death_pm$person_months, pierre_tab_death_pm$person_months,
                                 erasme_tab_death_pm$person_months, ghent_tab_death_pm$person_months))

total_deaths_df <- as.data.frame(cbind(liege_tab[1], liege_tab_death$Freq, pierre_tab_death$Freq, 
                                     erasme_tab_death$Freq, ghent_tab_death$Freq))


colnames(avg_pops_df)[c(1:5)] <- (c("Age_groups", "Liege", "St. Pierre", "Erasme", "Ghent"))
colnames(total_deaths_df)[c(1:5)] <- (c("Age_groups", "Liege", "Pierre", "Erasme", "Ghent"))
colnames(avg_pm_df)[c(1:5)] <- (c("Age_groups", "Liege", "St. Pierre", "Erasme", "Ghent"))


avg_pops_df$avg <- (rowMeans(avg_pops_df[,-1]))
total_deaths_df$avg <- (rowMeans(total_deaths_df[2:5]))
total_deaths_df$sum <- rowSums(total_deaths_df[2:5])
avg_pops_df$sum <- rowSums(avg_pops_df[2:5])
avg_pm_df$sum <- rowSums(avg_pm_df[2:5], na.rm=TRUE)

#add a percentage after the sum column out of total in total_deaths_df
total_deaths_df$proportion <- (total_deaths_df$sum/(sum(total_deaths_df$sum)))*100
total_deaths_df$proportion_liege <- (total_deaths_df$Liege/(sum(total_deaths_df$Liege)))*100
total_deaths_df$proportion_pierre <- (total_deaths_df$Pierre/(sum(total_deaths_df$Pierre)))*100
total_deaths_df$proportion_ghent <- (total_deaths_df$Ghent/(sum(total_deaths_df$Ghent)))*100
total_deaths_df$proportion_erasme <- (total_deaths_df$Erasme/(sum(total_deaths_df$Erasme)))*100

#copied code from below

# prop_tabs <- as.data.frame(cbind(total_deaths_df[1], total_deaths_df$proportion_liege,
#                                        total_deaths_df$proportion_pierre, 
#                                        total_deaths_df$proportion_erasme, 
#                                        total_deaths_df$proportion_ghent))
#                            
# colnames(prop_tabs)[c(1:5)] <- c("Age", "Liege", "St. Pierre", "Erasme", "Ghent")
# 
# prop_tabs_long <- melt(prop_tabs, id="Age")  # convert to long format
# names(prop_tabs_long)[2] <- "Cohort"

# prop_mortality <- ggplot(data=prop_tabs_long,
#                                  aes(x=Age, y=value, group = Cohort, colour=Cohort)) +
#   geom_line(size=1.5) + ylab("Percentage") + ggtitle("Proportional mortality rates by cohort")



######### crudge rates
#make crude and adjusted rates
liege_tab$crude <- (liege_tab_death$Freq/liege_tab$Freq)*1000
pierre_tab$crude <- (pierre_tab_death$Freq/pierre_tab$Freq)*1000
erasme_tab$crude <- (erasme_tab_death$Freq/erasme_tab$Freq)*1000
ghent_tab$crude <- (ghent_tab_death$Freq/ghent_tab$Freq)*1000

liege_tab$adj <- (liege_tab_death$Freq/avg_pops_df$avg)*1000
pierre_tab$adj <- (pierre_tab_death$Freq/avg_pops_df$avg)*1000
erasme_tab$adj <- (erasme_tab_death$Freq/avg_pops_df$avg)*1000
ghent_tab$adj <- (ghent_tab_death$Freq/avg_pops_df$avg)*1000

total_deaths_df$total <- (total_deaths_df$avg/avg_pops_df$avg)*1000

#71-75 is an outlier. It's one person who was followed for less than a month
#remove it
#avg_pm_df$pm <- (total_deaths_df$sum/avg_pm_df$sum)*100
#avg_pm_df[15, 7] = 0



#mortal_tabs_crude <- as.data.frame(cbind(liege_tab[1], liege_tab$crude, 
                                         pierre_tab$crude, erasme_tab$crude, ghent_tab$crude))
#colnames(mortal_tabs_crude)[c(1:5)] <- c("Age", "Liege", "St. Pierre", "Erasme", "Ghent")

# 
# mortal_tabs_crude_long <- melt(mortal_tabs_crude, id="Age")  # convert to long format
# names(mortal_tabs_crude_long)[2] <- "Cohort"
# 
# crude_mortality <- ggplot(data=mortal_tabs_crude_long,
#                           aes(x=Age, y=value, group = Cohort, colour=Cohort)) +
#   geom_line(size=1.5) + ylab("Mortality rate") + ggtitle("Age-adjusted crude mortality rates by cohort")


mortal_tabs <- as.data.frame(cbind(liege_tab[1], liege_tab$adj, pierre_tab$adj, erasme_tab$adj, ghent_tab$adj, total_deaths_df$total))
colnames(mortal_tabs)[c(1:6)] <- c("Age", "Liege", "St. Pierre", "Erasme", "Ghent", "All")

mortal_tabs_long <- melt(mortal_tabs, id="Age")  # convert to long format
names(mortal_tabs_long)[2] <- "Cohort"

standardized_mortality <- ggplot(data=mortal_tabs_long,
       aes(x=Age, y=value, group = Cohort, colour=Cohort)) +
  geom_line(size=1.5) + ylab("Mortality rate per 1000") + ggtitle("Age-adjusted standardized mortality rates by cohort")

###

# pm_mortality <- ggplot(data=avg_pm_df,
#                                  aes(x=Age_groups, y=pm, group=1)) +
#   geom_line(size=1.5) + ylab("Mortality rate per 100 person-months") + ggtitle("Mortality by age group")

#  
# combined_mortality <- ggplot(data=total_deaths_df,
#                                  aes(x=Age_groups, y=total, group = 1)) +
#   geom_line(size=1.5) + ylab("Mortality rate per 1000") + ggtitle("Age-adjusted standardized mortality rates by cohort")



################ gendered mortality ----------------

### subset the dead_2014 data frames to m or f
#need to make this look better later

pierre_FU_female <- subset(pierre_FU_all, GENDER == "Female")
pierre_FU_male <- subset(pierre_FU_all, GENDER == "Male")
pierre_dead_2014_female <- subset(pierre_dead_2014, GENDER == "Female")
pierre_dead_2014_male <- subset(pierre_dead_2014, GENDER == "Male")


liege_FU_female <- subset(liege_FU_all, GENDER == "Female")
liege_FU_male <- subset(liege_FU_all, GENDER == "Male")
liege_dead_2014_female <- subset(liege_dead_2014, GENDER == "Female")
liege_dead_2014_male <- subset(liege_dead_2014, GENDER == "Male")

erasme_FU_female <- subset(erasme_FU_all, GENDER == "Female")
erasme_FU_male <- subset(erasme_FU_all, GENDER == "Male")
erasme_dead_2014_female <- subset(erasme_dead_2014, GENDER == "Female")
erasme_dead_2014_male <- subset(erasme_dead_2014, GENDER == "Male")

ghent_FU_female <- subset(ghent_FU_all, CURRENT.LEGAL.SEX == "Female")
ghent_FU_male <- subset(ghent_FU_all, CURRENT.LEGAL.SEX == "Male")
ghent_dead_2014_female <- subset(ghent_dead_2014, CURRENT.LEGAL.SEX == "Female")
ghent_dead_2014_male <- subset(ghent_dead_2014, CURRENT.LEGAL.SEX == "Male")

pierre_tab_female <- tab(pierre_FU_female)
pierre_tab_male <- tab(pierre_FU_male)
pierre_tab_death_female <- tab(pierre_dead_2014_female)
pierre_tab_death_male <- tab(pierre_dead_2014_male)

liege_tab_female <- tab(liege_FU_female)
liege_tab_male <- tab(liege_FU_male)
liege_tab_death_female <- tab(liege_dead_2014_female)
liege_tab_death_male <- tab(liege_dead_2014_male)

erasme_tab_female <- tab(erasme_FU_female)
erasme_tab_male <- tab(erasme_FU_male)
erasme_tab_death_female <- tab(erasme_dead_2014_female)
erasme_tab_death_male <- tab(erasme_dead_2014_male)

ghent_tab_female <- tab(ghent_FU_female)
ghent_tab_male <- tab(ghent_FU_male)
ghent_tab_death_female <- tab(ghent_dead_2014_female)
ghent_tab_death_male <- tab(ghent_dead_2014_male)


avg_pops_df_female <- as.data.frame(cbind(liege_tab[1], liege_tab_female$Freq, pierre_tab_female$Freq, 
                                   erasme_tab_female$Freq, ghent_tab_female$Freq))

avg_pops_df_male <- as.data.frame(cbind(liege_tab[1], liege_tab_male$Freq, pierre_tab_male$Freq, 
                                          erasme_tab_male$Freq, ghent_tab_male$Freq))

total_deaths_df_female <- as.data.frame(cbind(liege_tab[1], liege_tab_death_female$Freq, pierre_tab_death_female$Freq, 
                                       erasme_tab_death_female$Freq, ghent_tab_death_female$Freq))

total_deaths_df_male <- as.data.frame(cbind(liege_tab[1], liege_tab_death_male$Freq, pierre_tab_death_male$Freq, 
                                              erasme_tab_death_male$Freq, ghent_tab_death_male$Freq))

colnames(avg_pops_df_female)[c(1:5)] <- (c("Age_groups", "Liege", "St. Pierre", "Erasme", "Ghent"))
colnames(avg_pops_df_male)[c(1:5)] <- (c("Age_groups", "Liege", "St. Pierre", "Erasme", "Ghent"))

colnames(total_deaths_df_female)[c(1:5)] <- (c("Age_groups", "Liege", "Pierre", "Erasme", "Ghent"))
colnames(total_deaths_df_male)[c(1:5)] <- (c("Age_groups", "Liege", "Pierre", "Erasme", "Ghent"))


avg_pops_df_female$avg <- (rowMeans(avg_pops_df_female[,-1]))
avg_pops_df_male$avg <- (rowMeans(avg_pops_df_male[,-1]))

avg_pops_df_female$sum <- (rowSums(avg_pops_df_female[2:5]))
avg_pops_df_male$sum <- (rowSums(avg_pops_df_male[2:5]))

# total_deaths_df_female$avg <- (rowMeans(total_deaths_df_female[2:5]))
# total_deaths_df_male$avg <- (rowMeans(total_deaths_df_male[2:5]))

total_deaths_df_female$sum <- (rowSums(total_deaths_df_female[2:5]))
total_deaths_df_male$sum <- (rowSums(total_deaths_df_male[2:5]))

# total_deaths_df_female$total <- (total_deaths_df_female$avg/avg_pops_df_female$avg)*1000
# total_deaths_df_male$total <- (total_deaths_df_male$avg/avg_pops_df_male$avg)*1000

total_deaths_df_female$crude <- (total_deaths_df_female$sum/avg_pops_df_female$sum)*1000
total_deaths_df_male$crude <- (total_deaths_df_male$sum/avg_pops_df_male$sum)*1000

total_deaths_df$crude <- (total_deaths_df$sum/avg_pops_df$sum)*1000

#combine the datasets

# total_deaths_df_female <- total_deaths_df_female[, -c(7)]
# total_deaths_df_male <- total_deaths_df_male[, -c(7)]

names(total_deaths_df_female)[7] <- "Female"
names(total_deaths_df_male)[7] <- "Male"
names(total_deaths_df)[14] <- "All"

combined_MF <- merge(total_deaths_df_female, total_deaths_df_male, by = "Age_groups")
combined_MF <- combined_MF[, -c(2:6)]
combined_MF <- combined_MF[, -c(3:7)]

combined_MF <- merge(combined_MF, total_deaths_df, by= "Age_groups")
combined_MF <- combined_MF[, -c(4:15)]

#find rowsums for >65, combine
above_65 <- rowSums(total_deaths_df[, c(14:19)])


# drop_ages <- subset(combined_MF, Age_groups != "66-70" & Age_groups != "71-75" 
#                     & Age_groups != "76-80" & Age_groups != "81-85" 
#                     & Age_groups != "86-90" & Age_groups != "90-95" 
#                     & Age_groups != "0-5" & Age_groups != "6-10")

#combined_MF_new <- droplevels(drop_ages)
comb_tabs_long <- melt(combined_MF, id="Age_groups")  # convert to long format
names(comb_tabs_long)[2] <- "Gender"

comb_tabs_long <- melt(combined_MF_new, id="Age_groups")  # convert to long format
names(comb_tabs_long)[2] <- "Gender"

gender_mortality <- ggplot(data=comb_tabs_long, aes(x=Age_groups, y=value, group = Gender, colour=Gender)) +
  geom_line(size=1.5, alpha = 0.4) + ylab("Mortality rate per 1000") + 
  xlab("Age groups") +
  ggtitle("Age-adjusted crude mortality rates by gender") + 
  theme(legend.position = "bottom", plot.title = element_text(size = 30, face="bold"))


total_deaths_df$proportion <- (total_deaths_df$sum/(sum(total_deaths_df$sum)))*100

#make a graph of proportions of deaths
proportion_mortality <- ggplot(data=total_deaths_df,aes(x=Age_groups, y=proportion, group=1)) +
  geom_line(size=1.5) + xlab("Age groups") +
  ylab("Percentage of total deaths") +
  ggtitle("Proportion of total deaths by age group") +
  theme(plot.title = element_text(size = 30, face = "bold"))

#using this graph for the poster
grid.arrange(gender_mortality, proportion_mortality)



total_deaths_df$sum <- rowSums(total_deaths_df[2:5])
avg_pops_df$sum <- rowSums(avg_pops_df[2:5])



############# mortality by under and over 55 ------


young_func <- function(df) {
  young_df <- subset(df, age <= 55)
  young_df
}

old_func <- function(df) {
  old_df <- subset(df, age > 55)
  old_df
}

pierre_FU_young <- young_func(pierre_FU_all)
pierre_dead_young <- young_func(pierre_dead_2014)
liege_FU_young <- young_func(liege_FU_all)
liege_dead_young <- young_func(liege_dead_2014)
erasme_FU_young <- young_func(erasme_FU_all)
erasme_dead_young <- young_func(erasme_dead_2014)
ghent_FU_young <- young_func(ghent_FU_all)
ghent_dead_young <- young_func(ghent_dead_2014)

pierre_FU_old <- old_func(pierre_FU_all)
pierre_dead_old <- old_func(pierre_dead_2014)
liege_FU_old <- old_func(liege_FU_all)
liege_dead_old <- old_func(liege_dead_2014)
erasme_FU_old <- old_func(erasme_FU_all)
erasme_dead_old <- old_func(erasme_dead_2014)
ghent_FU_old <- old_func(ghent_FU_all)
ghent_dead_old <- old_func(ghent_dead_2014)

pierre_tab_young <- tab(pierre_FU_young)
pierre_tab_death_young <- tab(pierre_dead_young)

liege_tab_young <- tab(liege_FU_young)
liege_tab_death_young <- tab(liege_dead_young)

erasme_tab_young <- tab(erasme_FU_young)
erasme_tab_death_young <- tab(erasme_dead_young)

ghent_tab_young <- tab(ghent_FU_young)
ghent_tab_death_young <- tab(ghent_dead_young)

#old

pierre_tab_old <- tab(pierre_FU_old)
pierre_tab_death_old <- tab(pierre_dead_old)

liege_tab_old <- tab(liege_FU_old)
liege_tab_death_old <- tab(liege_dead_old)

erasme_tab_old <- tab(erasme_FU_old)
erasme_tab_death_old <- tab(erasme_dead_old)

ghent_tab_old <- tab(ghent_FU_old)
ghent_tab_death_old <- tab(ghent_dead_old)

avg_pops_df_young <- as.data.frame(cbind(liege_tab[1], liege_tab_young$Freq, pierre_tab_young$Freq, 
                                          erasme_tab_young$Freq, ghent_tab_young$Freq))

avg_pops_df_old <- as.data.frame(cbind(liege_tab[1], liege_tab_old$Freq, pierre_tab_old$Freq, 
                                        erasme_tab_old$Freq, ghent_tab_old$Freq))

total_deaths_df_young <- as.data.frame(cbind(liege_tab[1], liege_tab_death_young$Freq, pierre_tab_death_young$Freq, 
                                              erasme_tab_death_young$Freq, ghent_tab_death_young$Freq))

total_deaths_df_old <- as.data.frame(cbind(liege_tab[1], liege_tab_death_old$Freq, pierre_tab_death_old$Freq, 
                                            erasme_tab_death_old$Freq, ghent_tab_death_old$Freq))

colnames(avg_pops_df_young)[c(1:5)] <- (c("Age_groups", "Liege", "St. Pierre", "Erasme", "Ghent"))
colnames(avg_pops_df_old)[c(1:5)] <- (c("Age_groups", "Liege", "St. Pierre", "Erasme", "Ghent"))

colnames(total_deaths_df_young)[c(1:5)] <- (c("Age_groups", "Liege", "Pierre", "Erasme", "Ghent"))
colnames(total_deaths_df_old)[c(1:5)] <- (c("Age_groups", "Liege", "Pierre", "Erasme", "Ghent"))

        
avg_pops_df_young$avg <- (rowMeans(avg_pops_df_young[,-1]))
avg_pops_df_old$avg <- (rowMeans(avg_pops_df_old[,-1]))

avg_pops_df_young$sum <- (rowSums(avg_pops_df_young[2:5]))
avg_pops_df_old$sum <- (rowSums(avg_pops_df_old[2:5]))

total_deaths_df_young$sum <- (rowSums(total_deaths_df_young[2:5]))
total_deaths_df_old$sum <- (rowSums(total_deaths_df_old[2:5]))

total_deaths_df_young$crude <- (total_deaths_df_young$sum/avg_pops_df_young$sum)*1000
total_deaths_df_old$crude <- (total_deaths_df_old$sum/avg_pops_df_old$sum)*1000

names(total_deaths_df_young)[7] <- "young"
names(total_deaths_df_old)[7] <- "old"

combined_YO <- merge(total_deaths_df_young, total_deaths_df_old, by = "Age_groups")
combined_YO <- combined_YO[, -c(2:6)]
combined_YO <- combined_YO[, -c(3:7)]

comb_YO_tabs_long <- melt(combined_YO, id="Age_groups")  # convert to long format
names(comb_tabs_long)[2] <- "Gender"

#sum
total_deaths_young_sum <- sum(total_deaths_df_young$sum, na.rm=TRUE)
total_pop_young_sum <- sum(avg_pops_df_young$sum, na.rm=TRUE)

total_deaths_old_sum <- sum(total_deaths_df_old$sum, na.rm=TRUE)
total_pop_old_sum <- sum(avg_pops_df_old$sum, na.rm=TRUE)

total_young_crude <- (total_deaths_young_sum/total_pop_young_sum)
total_old_crude <- (total_deaths_old_sum/total_pop_old_sum)

#####
#try it with age as a continuous variable

tab_cont <- function(df) {
  base <- as.data.frame(table(age = df$age))
  base
}

liege_tab_cont <- tab_cont(liege_FU_all)
pierre_tab_cont <- tab_cont(pierre_FU_all)
erasme_tab_cont <- tab_cont(erasme_FU_all)
ghent_tab_cont <- tab_cont(ghent_FU_all)

liege_tab_death_cont <- tab_cont(liege_dead_2014)
pierre_tab_death_cont <- tab_cont(pierre_dead_2014)
erasme_tab_death_cont <- tab_cont(erasme_dead_2014)
ghent_tab_death_cont <- tab_cont(ghent_dead_2014)


rbind(pierre_tab_death_cont$age, new_row)

old_comb <- function(site_tab_cont) {
  site_tab_cont$age <- as.character(site_tab_cont$age)
  site_tab_cont$age <- as.numeric(site_tab_cont$age)
  tab_65 <- subset(site_tab_cont, age >= 66)
  tab_65_sum <- sum(tab_65$Freq)
  df_new <- site_tab_cont
  df_new$Freq[df_new$age == 66] <- tab_65_sum
  df_new
}



pierre_comb_65 <- old_comb(pierre_tab_cont)
liege_comb_65 <- old_comb(liege_tab_cont)
erasme_comb_65 <- old_comb(erasme_tab_cont)
ghent_comb_65 <- old_comb(ghent_tab_cont)

pierre_comb_death_65 <- old_comb(pierre_tab_death_cont)
liege_comb_death_65 <- old_comb(liege_tab_death_cont)
erasme_comb_death_65 <- old_comb(erasme_tab_death_cont)
ghent_comb_death_65 <- old_comb(ghent_tab_death_cont)

pierre_comb_65 <- pierre_comb_65[!(pierre_comb_65$age >66),]
liege_comb_65 <- liege_comb_65[!(liege_comb_65$age >66),]
erasme_comb_65 <- erasme_comb_65[!(erasme_comb_65$age >66),]
ghent_comb_65 <- ghent_comb_65[!(ghent_comb_65$age >66),]

comb_comb_65_tmp <- merge(liege_comb_65, pierre_comb_65, by="age", all=TRUE)
comb_comb_65_tmp2 <- merge(comb_comb_65_tmp, erasme_comb_65, by="age", all=TRUE)
colnames(comb_comb_65_tmp2)[c(2:3)] <- c("liege", "pierre")
comb_comb_65 <- merge(comb_comb_65_tmp2, ghent_comb_65, by="age", all=TRUE)
colnames(comb_comb_65)[c(4:5)] <- c("erasme", "ghent")
comb_comb_65$sum <- rowSums(comb_comb_65[2:5], na.rm=TRUE)

#now deaths

comb_comb_65_death_tmp <- merge(liege_comb_death_65, pierre_comb_death_65, by="age", all=TRUE)
comb_comb_65_death_tmp2 <- merge(comb_comb_65_death_tmp, erasme_comb_death_65, by="age", all=TRUE)
colnames(comb_comb_65_death_tmp2)[c(2:3)] <- c("liege", "pierre")
comb_comb_65_death <- merge(comb_comb_65_death_tmp2, ghent_comb_death_65, by="age", all=TRUE)
colnames(comb_comb_65_death)[c(4:5)] <- c("erasme", "ghent")
comb_comb_65_death$sum <- rowSums(comb_comb_65_death[2:5], na.rm=TRUE)

deaths_65 <- subset(comb_comb_65_death, age >= 66)
deaths_65_sum <- sum(deaths_65$sum)

#rename 69 in the age col in comb_comb_65_death to be 66
comb_comb_65_death[26, 1] = 66
comb_comb_65_death[26, 6] = 2
comb_comb_65_death <- comb_comb_65_death[!(comb_comb_65_death$age > 66),]

drops <- c("liege", "pierre", "erasme", "ghent")
comb_comb_65 <- comb_comb_65[, !names(comb_comb_65) %in% drops]
comb_comb_65_death <- comb_comb_65_death[, !names(comb_comb_65_death) %in% drops]

new_death_df <- merge(comb_comb_65, comb_comb_65_death, by="age", all=TRUE)
new_death_df[is.na(new_death_df)] <- 0
colnames(new_death_df)[c(2:3)] <- c("pop", "deaths")
new_death_df$crude <- (new_death_df$deaths/new_death_df$pop)*1000

#need to bin age groups 
#just going to export to excel right now
library(xlsx)
write.xlsx(new_death_df, "/Users/cda/Desktop/new_death_df.xlsx")
#graph it 
#bring it back

new_death_df_fixed <- read.csv('/Users/cda/Desktop/new_death_fixed.csv', header = T, na.strings=c(""))

new_graph_over_66 <- ggplot(data=new_death_df_fixed, aes(x=Age, y=Crude, group = 1)) +
  geom_line(size=1.5) + ylab("Mortality rate per 1000") + 
  xlab("Age groups") +
  ggtitle("Age-adjusted crude mortality rates") + 
  theme(legend.position = "bottom", plot.title = element_text(size = 30, face="bold"))

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
sum(all_tabs_death$sum)/sum(all_tabs$sum)


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

names(erasme_not_LTFU)[5] <- "age"
names(erasme_LTFU)[5] <- "age"

names(ghent_not_LTFU)[3] <- "age"
names(ghent_LTFU)[3] <- "age"

liege_LTFU <- subset(liege_LTFU, STATUS == "Contact lost")
mean(liege_LTFU$age)

erasme_LTFU <- subset(erasme_master,status == "LTFU")
mean(erasme_LTFU$age)

ghent_LTFU <- subset(ghent_px_master,status == "LTFU")
mean(ghent_LTFU$age)

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

######## LTFU or transferred? -------------
#post to Stack Overflow and come back to this
#could merge by multiple variables

#rename all the DOB columns to be able to merge
names(ghent_px_master)[2] <- "DOB"
names(erasme_master)[4] <- "DOB"
names(liege_master2)[2] <- "DOB"

#make sure they are all in date format
erasme_LTFU$DOB <- as.Date(erasme_LTFU$DOB, format = "%d-%m-%Y")
ghent_LTFU$DOB <- as.Date(ghent_LTFU$DOB, format = "%d-%m-%Y")

trans <- c(pierre_LTFU$DOB, liege_not_LTFU$DOB)
trans2 <- trans[duplicated(trans)]
trans2 <- trans2[order(trans2)]
trans_df <- data.frame(DOB = trans2)

liege_not_LTFU_order <- liege_not_LTFU[order(liege_not_LTFU$DOB)]
pierre_LTFU_order <- pierre_LTFU[order(pierre_LTFU$DOB),]

transferred_pierre <- merge(liege_not_LTFU_order, trans_df, by = "DOB", all.y=FALSE)



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

erasme_not_LTFU <- binning_ages(erasme_not_LTFU)
ghent_not_LTFU <- binning_ages(ghent_not_LTFU)
pierre_not_LTFU <- binning_ages(pierre_not_LTFU)

#combine all the ages for summary statistics
ages_summary <- as.data.frame(c(liege_not_LTFU$age, ghent_not_LTFU$AGE, erasme_not_LTFU$AGE, 
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
cd4_all <- c(ghent_not_LTFU$MOST_RECENT_CD4, erasme_not_LTFU$last.cd4_value,
             liege_not_LTFU$CD4_RECENT, pierre_not_LTFU$cd4_recent)

cd4_nadir_all <- c(erasme_not_LTFU$cd4_nadir_v,
             liege_not_LTFU$CD4_NADIR, pierre_not_LTFU$cd4_nadir.x)

mean(cd4_all, na.rm=TRUE)
mean(cd4_nadir_all, na.rm=TRUE)

#change factors to characters
pierre_not_LTFU$GENDER <- as.character(pierre_not_LTFU$GENDER)
erasme_not_LTFU$GENDER <- as.character(erasme_not_LTFU$GENDER)

sex_all <- data.frame(c(ghent_not_LTFU$GENDER, erasme_not_LTFU$GENDER, 
             liege_not_LTFU$GENDER, pierre_not_LTFU$GENDER))
table(sex_all$c.ghent_not_LTFU.GENDER..erasme_not_LTFU.GENDER..liege_not_LTFU.GENDER..)
### combined % started treatment

nrow(erasme_started_trt)
nrow(liege_started_trt)
table(pierre_started_trt$started_trt)

(2842+972+829)/(1006+3003+882)

#combined region
pierre_not_LTFU$REGION_OF_ORIGIN <- as.character(pierre_not_LTFU$REGION_OF_ORIGIN)

region_all <- data.frame(c(ghent_not_LTFU$REGION_OF_ORIGIN, erasme_not_LTFU$REGION_OF_ORIGIN, 
                        liege_not_LTFU$REGION_OF_ORIGIN, pierre_not_LTFU$REGION_OF_ORIGIN), na.rm=TRUE)

names(region_all)[1] <- "REGION_OF_ORIGIN"

###### ethnic plots --------

region_all$REGION_OF_ORIGIN <- gsub("Central America", "Latin America/Caribbean", region_all$REGION_OF_ORIGIN)
region_all$REGION_OF_ORIGIN <- gsub("South America", "Latin America/Caribbean", region_all$REGION_OF_ORIGIN)
region_all$REGION_OF_ORIGIN <- gsub("UNKNOWN", "Other/unknown", region_all$REGION_OF_ORIGIN)
region_all$REGION_OF_ORIGIN <- gsub("Eastern Europe", "Europe", region_all$REGION_OF_ORIGIN)
region_all$REGION_OF_ORIGIN <- gsub("Western Europe", "Europe", region_all$REGION_OF_ORIGIN)
region_all$REGION_OF_ORIGIN <- gsub("Oceania Aust. NZ", "Other/unknown", region_all$REGION_OF_ORIGIN)
region_all$REGION_OF_ORIGIN <- gsub("Unknown", "Other/unknown", region_all$REGION_OF_ORIGIN)

region_all <- ethnic_freq(region_all)
region_all$Freq <- round(region_all$Freq, digits = 1)
names(region_all)[1] <- "Region"

region_all


region_bar <- ggplot(region_all, aes(x=1, y=Freq, fill=Region)) +
  geom_bar(stat="identity") +
  ggtitle("Region of origin")

y.breaks <- cumsum(region_all$Freq) - region_all$Freq/2

region_pie <- ggplot(region_all, aes(x=1, y=Freq, fill=Region)) +
  # black border around pie slices
  geom_bar(stat="identity", color='black') +
  # remove black diagonal line from legend
  guides(fill=guide_legend(override.aes=list(colour=NA))) +
  # polar coordinates
  coord_polar(theta='y') +
  # label aesthetics
  theme(plot.title = element_text(size=30, face="bold"),
        legend.text=element_text(size=15),
        legend.title = element_blank(),
        axis.ticks=element_blank(),  # the axis ticks
        axis.title=element_blank(),  # the axis labels
        axis.text.y=element_blank(), # the 0.75, 1.00, 1.25 labels
        axis.text.x=element_text(color='black', size = 11)) + 
  ggtitle("Region of origin") + 
  scale_y_continuous(
    breaks=y.breaks,
    labels=region_all$Freq)


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
