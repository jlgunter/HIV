
######## Random notes

#REDO GHENT BC OF DEATH ISSUE

# MUST DECIDE whether to use living not LTFU or just not LTFU
#Use living not LTFU
#liege_not_LTFU, pierre_not_LTFU, erasme_not_LTFU are only living px

###something to consider: When a patient dies, there is also an event for end of follow up.
#need to check if it's identical to death data

#also, a more nuanced look at what's happening at the events will be necessary.
#for now, for example for HTA, I'm just looking at if the patient was ever treated

### Add in new Erasme data

### Add Ghent data

###Should Pierre BMI and CD4 be an average, or the most recent?


#figure out when they other cohorts considered it LTFU
#pierre was no follow up after June 2014
#Liege was no follow up after Jan 2015
#need to change Liege to match Pierre
#then make Erasme match

##################### Packages -----------
library(data.table)
library(ltm)
#library(stringr)
#library(aod)
#library(brglm)
library(Hmisc)
library(reshape2)
library(tidyr)
library(plyr)
library(RColorBrewer)
require(gridExtra)
library(car)

####################### Read in files ----------------------
liege_CD4 <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/HIV/Data/Belgium/Université de Liège -Sart Tilman/csv/ULG_CD4_2.csv', header = T, na.strings=c(""))
liege_CD4_percent <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/HIV/Data/Belgium/Université de Liège -Sart Tilman/csv/ULG_CD4_PERCENT_2.csv', header = T, na.strings=c(""))
liege_death <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/HIV/Data/Belgium/Université de Liège -Sart Tilman/csv/ULG_DEATH_2.csv', header = T, na.strings=c(""))
liege_BAS <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/HIV/Data/Belgium/Université de Liège -Sart Tilman/csv/ULG_BAS_2.csv', header = T, na.strings=c(""), stringsAsFactors = FALSE)
liege_CEP <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/HIV/Data/Belgium/Université de Liège -Sart Tilman/csv/ULG_CEP_2.csv', header = T, na.strings=c(""))
liege_artcodes <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/HIV/Data/Belgium/ART_standardization.csv', header = T, na.strings=c(""))
liege_art <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/HIV/Data/Belgium/Université de Liège -Sart Tilman/csv/ULG_ART_2.csv', header = T, na.strings=c(""))

pierre_px <- read.csv2('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/HIV/Data/Belgium/St Pierre/PATIENTS.csv',
                          header = TRUE, quote = "\"", dec = ",", na.strings=c(""))

pierre_events <- read.csv2('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/HIV/Data/Belgium/St Pierre/EVENTS.csv',
                              header = TRUE, quote = "\"", dec = ",", na.strings=c(""))

erasme_base <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/HIV/Data/Belgium/Erasme/Erasme_base.csv', header = T, na.strings=c(""))
erasme_ART <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/HIV/Data/Belgium/Erasme/Erasme_ART.csv', header = T, na.strings=c(""))

erasme_cd4 <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/HIV/Data/Belgium/Erasme/Erasme_CD4.csv', header = T, na.strings=c(""))

#aggregate the different CM dataframes
erasme_cancer <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/HIV/Data/Belgium/Erasme/Erasme_cancer.csv', header = T, na.strings=c(""))
erasme_cvd <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/HIV/Data/Belgium/Erasme/Erasme_cvd.csv', header = T, na.strings=c(""))
erasme_diabetes <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/HIV/Data/Belgium/Erasme/Erasme_diabetes.csv', header = T, na.strings=c(""))
erasme_hta <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/HIV/Data/Belgium/Erasme/Erasme_hta.csv', header = T, na.strings=c(""))
erasme_liver <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/HIV/Data/Belgium/Erasme/Erasme_liver.csv', header = T, na.strings=c(""))
erasme_renal <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/HIV/Data/Belgium/Erasme/Erasme_renal.csv', header = T, na.strings=c(""))
erasme_bmi <- read.csv('/Users/cda/Desktop/Erasme_bmi.csv', header = T, na.strings=c(""), sep=',')
erasme_new <- read.csv('/Users/cda/Desktop/Erasme_new_data.csv', header = T, na.strings=c(""), sep=',')
erasme_nadir <- read.csv('/Users/cda/Desktop/Erasme_nadircd4.csv', header = T, na.strings=c(""), sep=',')

ghent_px1 <-read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/HIV/Data/Belgium/Ghent/Ghent_new_data.csv', header = T, na.strings=c(""))
ghent_px2 <-read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/HIV/Data/Belgium/Ghent/Ghent_new_data2.csv', header = T, na.strings=c(""))


##################### Overall functions ----------------------

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

### age function

#from http://r.789695.n4.nabble.com/Calculate-difference-between-dates-in-years-td835196.html
age_years <- function(first, second) 
{ 
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

#make the fake placeholder date into NA
liege_BAS[liege_BAS == "1911-11-11"] <- NA

#subtract birth date from today's date, make new column for age
liege_BAS$sys_date <- Sys.Date()
#as.numeric(liege_BAS$sys_date, "%Y")

liege_BAS$age <- age_years(liege_BAS$BIRTH_DATE, liege_BAS$sys_date)

#do the same for the number of years on ART
liege_BAS$years_on_ART <- age_years(liege_BAS$DATE.START.ART, liege_BAS$sys_date)

#standardize spelling in ethnic
liege_BAS$ETHNIC <- gsub("africain", "Africain", liege_BAS$ETHNIC)
liege_BAS$ETHNIC <- gsub("caucasien", "Caucasien", liege_BAS$ETHNIC)

liege_CEP$CLIN_EVENT_DATE <- as.Date(liege_CEP$CLIN_EVENT_DATE, format = "%m/%d/%Y")
liege_CEP$CLIN_EVENT_DATE[liege_CEP$CLIN_EVENT_DATE == "1911-11-11"]<- NA


########## Liege CD4 -----------

#combine CD4_comb with this database
liege_BAS_CD4 <- merge(liege_BAS, CD4_comb, by = "IDENT_NR", all=FALSE)

#find nadir CD4 count for each subject
liege_CD4_nadir <- aggregate(liege_CD4$CD4_VALUE ~ liege_CD4$IDENT_NR, liege_CD4, min)
colnames(liege_CD4_nadir)[1:2] <- c('IDENT_NR', 'CD4_NADIR')

#find mean CD4 count for each subject
liege_CD4_avg <- aggregate(liege_CD4$CD4_VALUE ~ liege_CD4$IDENT_NR, liege_CD4, mean)
colnames(liege_CD4_avg)[1:2] <- c('IDENT_NR', 'CD4_AVG')

#find most recent CD4 count
#convert dates from a factor to a date
liege_CD4$CD4_DATE <- as.Date(liege_CD4$CD4_DATE, format = "%m/%d/%Y")
liege_CD4_recent <- aggregate(liege_CD4$CD4_DATE ~ liege_CD4$IDENT_NR, liege_CD4, max)
colnames(liege_CD4_recent)[1:2] <- c('IDENT_NR', 'CD4_DATE')
liege_CD4_rec <- merge(liege_CD4, liege_CD4_recent, by = c("CD4_DATE", "IDENT_NR"), all=FALSE)
names(liege_CD4_rec)[3] <- 'CD4_RECENT'
liege_CD4_rec_ord <- liege_CD4_rec[order(liege_CD4_rec[,2]),]

#merge into one dataframe by ident
liege_CD4_tmp <- merge(liege_CD4_nadir, liege_CD4_avg, by = "IDENT_NR", all = FALSE)
liege_CD4_comb <- merge(liege_CD4_tmp, liege_CD4_rec, by = "IDENT_NR", all = FALSE)
names(liege_CD4_comb)[4] <- 'RECENT_DATE'

#find mean recent CD4 count for px not LTFU
liege_cd4_not_ltfu <- merge(liege_not_LTFU, liege_CD4_comb, by="IDENT_NR", all=FALSE)
mean(liege_cd4_not_ltfu$CD4_RECENT)

########### Liege master DF -------

#liege_master2 will include dead patients for NICM analysis
#For current living patients under follow up, use liege_not_LTFU

#add it on to the master database

comb <- function(df1, df2) {
  master <- as.data.table(merge(df1, df2, by = "IDENT_NR", all = TRUE))
  master
}

liege_comb <- comb(liege_BAS, liege_CEP)
liege_master <- comb(liege_comb, liege_death)

liege_master$GENDER <- gsub("F", "Female", liege_master$GENDER)
liege_master$GENDER <- gsub("M", "Male", liege_master$GENDER)
liege_master$GENDER <- gsub("O", "Other", liege_master$GENDER)

liege_master <- binning_ages(liege_master)

############################### Liege Num CMs ------------------

#make a new column that is number of co-morbidities each patient experienced
liege_master[, num_CMs := .N, by=IDENT_NR][is.na(CLIN_EVENT_ID), num_CMs := 0]
liege_master[, num_CMs := as.numeric(num_CMs)]

#dcast the data
liege_master[, id := 1:.N, by = IDENT_NR]
liege_master_dcast <- dcast(liege_master[,list(IDENT_NR, id, CLIN_EVENT_ID)], IDENT_NR ~ id, value.var = 'CLIN_EVENT_ID', fill = 0)

liege_master2 <- merge(liege_master, liege_master_dcast, by="IDENT_NR", all.x=FALSE)
liege_master2 <- liege_master2[!duplicated(liege_master2$IDENT_NR)]

colnames(liege_master2)[c(29:33)] <- (c("CLIN_EVENT_ID1", "CLIN_EVENT_ID2", 
                                        "CLIN_EVENT_ID3", "CLIN_EVENT_ID4", 
                                        "CLIN_EVENT_ID5"))

#change the LTFU status for those in the second half of 2014 as in Pierre

liege_master2$STATUS[liege_master2$LAST_VIS_DATE > as.Date("2014-07-01")] <- "Follow up"

#change back the dead patients to death status

liege_master2$STATUS[liege_master2$DEATH_DATE > as.Date("2000-07-01")] <- "Death"

######################## Liege ethnic var -------------------
#rename the country variable in Liege_not_LTFU to match pierre_not_LTFU's region of origin

liege_master2 <- within(liege_master2, COUNTRY[COUNTRY == 'Belgique' | COUNTRY == 'Italie'|
                                                   COUNTRY == 'France'| COUNTRY == 'Espagne'| 
                                                   COUNTRY == 'Grece' | COUNTRY == 'Royaume Uni' |
                                                   COUNTRY == 'Portugal' | COUNTRY == 'Albanie'] <- 'Western Europe')

liege_master2 <- within(liege_master2, COUNTRY[COUNTRY == 'Turquie' | COUNTRY == 'Inde'|
                                                   COUNTRY == 'Armenie'| COUNTRY == 'Indonesie'| 
                                                   COUNTRY == 'Japon' | COUNTRY == 'Thailande' |
                                                   COUNTRY == 'Liban' | COUNTRY == 'Ouzbekistan'] <- 'Asia')

liege_master2 <- within(liege_master2, COUNTRY[COUNTRY == 'Slovaquie' | COUNTRY == 'Russie'|
                                                   COUNTRY == 'Roumanie' | COUNTRY == 'Pologne' |
                                                 COUNTRY == 'Lituanie'] <- 'Eastern Europe')

liege_master2 <- within(liege_master2, COUNTRY[COUNTRY == 'Mali' | COUNTRY == 'Rwanda'|
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
                                                   COUNTRY == 'Guinee'] <- 'Sub-Saharan Africa')

liege_master2 <- within(liege_master2, COUNTRY[COUNTRY == 'Cuba' | COUNTRY == 'Haiti'] <- 'Central America')

liege_master2 <- within(liege_master2, COUNTRY[COUNTRY == 'Bresil' | COUNTRY == 'Perou' |
                                                   COUNTRY == 'Equateur' | COUNTRY == 'Uruguay'] <- 'South America')

liege_master2 <- within(liege_master2, COUNTRY[COUNTRY == 'Maroc' | COUNTRY == 'Algerie'] <- 'North Africa')


liege_master2 <- within(liege_master2, COUNTRY[COUNTRY == 'Canada'] <- 'North America')

liege_master2$COUNTRY<- gsub("Cote d'Ivoire", "Sub-Saharan Africa", liege_master2$COUNTRY)
liege_master2$COUNTRY<- gsub("Inconnu", "Unknown", liege_master2$COUNTRY)

#find out frequency of region of origin
names(liege_master2)[5] <- "REGION_OF_ORIGIN"
ethnic_freq <- function(df) {
  site_region <- data.frame(table(df$REGION_OF_ORIGIN))
  site_region$Freq <- (site_region$Freq/sum(site_region$Freq))*100
  site_region
}

############## Liege BMI -------------

### find BMI for Liege patients
liege_master2$HEIGHT[liege_master2$HEIGHT == "999"] <- NA
liege_master2$WEIGHT[liege_master2$WEIGHT == "999"] <- NA

#the BMI of 54 must be an error
liege_outlier <- subset(liege_master2, BMI > 40)
#change to 105
liege_master2$WEIGHT[liege_master2$WEIGHT == 150] <- 105

#liege_master2$HEIGHT <- liege_master2$HEIGHT^2
liege_master2$HEIGHT <- liege_master2$HEIGHT*liege_master2$HEIGHT
liege_master2$BMI <- (liege_master2$WEIGHT/liege_master2$HEIGHT)*10000


#################### Liege LTFU -----------------

#living, not LTFU px - Liege
liege_not_LTFU <- subset(liege_master2, STATUS == "Follow up")
liege_not_LTFU <- binning_ages(liege_not_LTFU)
mean(liege_not_LTFU$age)

liege_LTFU_tmp <- subset(liege_master2, STATUS != "Follow up")
liege_LTFU_tmp2 <- subset(liege_LTFU_tmp, STATUS != "Death")
liege_LTFU <- subset(liege_LTFU_tmp2, STATUS != "Transferred")

mean(liege_LTFU$age)

ethnic_freq(liege_not_LTFU)

####################### Liege NICMs --------------------------
#look at liege_all CMs
#Create a new column that is binary instead of count for num_CMs

#liege_master2[, CM_yes := num_CMs]
#liege_master2$CM_yes <- ifelse(liege_master2$num_CMs>=1, 1, 0)

table(liege_CEP$CLIN_EVENT_ID)
#just subset it to ensure that there are no repeat individuals
liege_acs <- subset(liege_CEP, CLIN_EVENT_ID == "ACS")
liege_dia <- subset(liege_CEP, CLIN_EVENT_ID == "DIA")
liege_esrd <- subset(liege_CEP, CLIN_EVENT_ID == "ESRD")
liege_fra <- subset(liege_CEP, CLIN_EVENT_ID == "FRA")
liege_hepc <- subset(liege_CEP, CLIN_EVENT_ID == "HEPC")
liege_nadm <- subset(liege_CEP, CLIN_EVENT_ID == "NADM")
liege_str <- subset(liege_CEP, CLIN_EVENT_ID == "STR")

liege_cvd <- merge(liege_acs, liege_str, by="IDENT_NR", all=TRUE)
liege_cvd <- liege_cvd[!duplicated(liege_cvd$IDENT_NR),]
table(duplicated(liege_cvd$IDENT_NR))
(nrow(liege_cvd))/1244

liege_cvd <- merge(liege_acs, liege_str, by="IDENT_NR", all=TRUE)
liege_cvd <- liege_cvd[!duplicated(liege_cvd$IDENT_NR),]
table(duplicated(liege_cvd$IDENT_NR))
(nrow(liege_cvd))/1244

liege_fra <- liege_fra[!duplicated(liege_fra$IDENT_NR),]
(nrow(liege_fra))/1244

liege_esrd <- liege_esrd[!duplicated(liege_esrd$IDENT_NR),]
(nrow(liege_esrd))/1244

liege_dia <- liege_dia[!duplicated(liege_dia$IDENT_NR),]
(nrow(liege_dia))/1244

liege_nadm <- liege_nadm[!duplicated(liege_nadm$IDENT_NR),]
(nrow(liege_nadm))/1244

liege_hepc <- liege_hepc[!duplicated(liege_hepc$IDENT_NR),]
(nrow(liege_hepc))/1244

liege_cvd <- merge(liege_acs, liege_str, by="IDENT_NR", all=TRUE)
liege_cvd <- liege_cvd[!duplicated(liege_cvd$IDENT_NR),]
table(duplicated(liege_cvd$IDENT_NR))
(nrow(liege_cvd))/1244
#take out Hep C for now
liege_events <- liege_events[-c(6),]

#add in the second, third, and fourth CMs
liege_events$Freq[liege_events$Freq == 44] <- 54
liege_events$Freq[liege_events$Freq == 14] <- 20
liege_events$Freq[liege_events$Freq == 63] <- 72
liege_events$Freq[liege_events$Freq == 1] <- 3
liege_events$Freq[liege_events$Freq == 18] <- 23

bp <- ggplot(data=liege_events, aes(x=1, y=Freq, fill = CM)) + 
  geom_bar(stat="identity", color="black") + 
  ggtitle("Comorbidities (non-AIDS defining) - Liège") + 
  coord_polar(theta='y') + 
  guides(fill=guide_legend(override.aes=list(colour=NA))) + 
  theme(axis.ticks=element_blank(), axis.title=element_blank(), axis.text.y=element_blank()) 

y.breaks <- cumsum(liege_events$Freq) - liege_events$Freq/2

liege_pie <- bp + theme(axis.text.x=element_text(color='black')) +
  scale_y_continuous(
    breaks=y.breaks,   # where to place the labels
    labels=liege_events$CM # the labels
  ) + scale_fill_discrete(name="Comorbidity Legend",
                          breaks=c("ACS", "ASCI", "DIA", "ESRD", "FRA", "HEPC", "NADM", "STR"),
                          labels=c("Acute coronary syndrome", "Ascites", "Diabetes mellitus", "End stage renal disease", "Bone fracture", "Acute Hepatitis C", "Non-AIDS defining malignancies", "Stroke"))
####
liege_bar <- ggplot(data=liege_events, aes(x=CM, y=Freq, fill=CM)) + geom_bar(stat="identity") + ggtitle("Comorbidities (non-AIDS defining) - Liège") + scale_fill_discrete(name="Comorbidity Legend",
                                                                                                                                                                            breaks=c("ACS", "ASCI", "DIA", "ESRD", "FRA", "NADM", "STR"),
                                                                                                                                                                  labels=c("Acute coronary syndrome", "Ascites", "Diabetes mellitus", "End stage renal disease", "Bone fracture", "Non-AIDS defining malignancies", "Stroke"))

grid.arrange(liege_pie, liege_bar, ncol=2)


######### Liege drug data ----------------------

names(liege_art)[2] <- "drug_code"

#subset to just ART drugs that have no end date (i.e. are presumably still being taken)
liege_art2015 <- subset(liege_art, is.na(ART_END_DATE))

liege_art_merge <- merge(liege_artcodes, liege_art2015, by = "drug_code", all.x=TRUE, all.y=TRUE)
liege_art_merge2 <- liege_art_merge %>% count(art_name, wt = NULL)
liege_art_merge2 <- merge(liege_art_merge2, liege_artcodes, by = "art_name", all.x=TRUE, all.y=TRUE)

liege_art_count <- subset(liege_art_merge2, drug_class != "Integrase_inhib")
liege_art_count <- subset(liege_art_count, drug_class != "Entry_inhib")
liege_art_count <- subset(liege_art_count, drug_class != "Other")
liege_art_count <- subset(liege_art_count, drug_class != "Fusion_inhib")

#library("dplyr")


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


########################## Pierre Hepatitis -------------------------------

#recode the HepB and C columns to be numeric

pierre_events <- as.data.frame(lapply(pierre_events, FUN = function(foo) recode(foo, "c('No', 'N')= 0; 
                                                                                 c('Yes', 'Y')= 1; 'UNK'= NA")))

#write a function to create a new column that is conditional on whether the sum column => 1

dichotomous <- function(DT) {
  DT$sum_col <- rowSums(DT[,2:7, with=FALSE])
  DT$var_yes <- ifelse(DT$sum_col>=1, 1, 0)
  DT
}


pierre_hepb <- as.data.table(pierre_events[c(1, 20)])
pierre_hepb[, id_hepb := 1:.N, by = ID_PATIENT]
pierre_hepb_cast <- dcast(pierre_hepb[,list(ID_PATIENT, id_hepb, HBV_AT_EVENT)], 
                          ID_PATIENT ~ id_hepb, 
                          value.var = 'HBV_AT_EVENT', fill = 0)

pierre_hepc <- as.data.table(pierre_events[c(1, 21)])
pierre_hepc[, id_hepc := 1:.N, by = ID_PATIENT]
pierre_hepc_cast <- dcast(pierre_hepc[,list(ID_PATIENT, id_hepc, HCV_AT_EVENT)], 
                          ID_PATIENT ~ id_hepc, 
                          value.var = 'HCV_AT_EVENT', fill = 0)

pierre_hepb_cast <- as.data.table(pierre_hepb_cast)
pierre_hepb_cast <- pierre_hepb_cast[, lapply(.SD, as.numeric), by = ID_PATIENT]
pierre_hepc_cast <- as.data.table(pierre_hepc_cast)
pierre_hepc_cast <- pierre_hepc_cast[, lapply(.SD, as.numeric), by = ID_PATIENT]

pierre_hepb_cast <- dichotomous(pierre_hepb_cast)
colnames(pierre_hepb_cast)[9] <- "hepb_yes"
pierre_hepb_cast <- pierre_hepb_cast[ , paste0(c("1", "2", "3", "4", "5", "6", "sum_col")) := NULL]

pierre_hepc_cast <- dichotomous(pierre_hepc_cast)
colnames(pierre_hepc_cast)[9] <- "hepc_yes"
pierre_hepc_cast <- pierre_hepc_cast[ , paste0(c("1", "2", "3", "4", "5", "6", "sum_col")) := NULL]

################### Pierre CD4 ----------------

pierre_cd4 <- as.data.table(pierre_events[c(1, 8)])
pierre_cd4[, id_cd4 := 1:.N, by = ID_PATIENT]
pierre_cd4_cast <- dcast(pierre_cd4[,list(ID_PATIENT, id_cd4, CD4_AT_EVENT)], ID_PATIENT ~ id_cd4, value.var = 'CD4_AT_EVENT', fill = 0)

pierre_cd4_avg <- data.frame(round(Reduce(`+`, pierre_cd4_cast[-1]) / rowSums(pierre_cd4_cast[-1] != 0), 2))
names(pierre_cd4_avg)[1] <- "avg"
pierre_cd4_avg$avg <- as.numeric(pierre_cd4_avg$avg)

pierre_cd4_avg_df <- as.data.frame(cbind(ID_PATIENT = pierre_cd4_cast$ID_PATIENT, avg = pierre_cd4_avg$avg))
mean(pierre_cd4_avg_df$avg, na.rm=TRUE)

###########################################

pierre_cd4_nadir <- as.data.table(pierre_events[c(1, 9)])
pierre_cd4_nadir[, id_cd4_nadir := 1:.N, by = ID_PATIENT]
pierre_cd4_nadir_cast <- dcast(pierre_cd4_nadir[,list(ID_PATIENT, id_cd4_nadir, CD4_NADIR_EVENT)], ID_PATIENT ~ id_cd4_nadir, value.var = 'CD4_NADIR_EVENT', fill = 0)

pierre_cd4_nadir_cast <- as.data.table(pierre_cd4_nadir_cast)
pierre_cd4_nadir_cast <- pierre_cd4_nadir_cast[, lapply(.SD, as.numeric), by = ID_PATIENT]

pierre_cd4_all <- data.frame(ID_PATIENT=pierre_cd4_nadir_cast$ID_PATIENT, cd4_avg = pierre_cd4_cast$avg, cd4_nadir = pierre_cd4_nadir_cast[,2, with = FALSE])
names(pierre_cd4_all)[3] <- "cd4_nadir"

########### Pierre hyp ------------------
##treated for hypertension

pierre_hyp <- as.data.table(pierre_events)
pierre_hyp[, id_hyp := 1:.N, by = ID_PATIENT]
pierre_hyp_cast <- dcast(pierre_hyp[,list(ID_PATIENT, id_hyp, TRT_HYPERTENSION_AT_EVENT)], ID_PATIENT ~ id_hyp, value.var = 'TRT_HYPERTENSION_AT_EVENT', fill = 0)

pierre_hyp_cast <- as.data.table(pierre_hyp_cast)
pierre_hyp_cast <- pierre_hyp_cast[, lapply(.SD, as.numeric), by = ID_PATIENT]

pierre_hyp_cast <- dichotomous(pierre_hyp_cast)
colnames(pierre_hyp_cast)[9] <- "hyp_yes"
pierre_hyp_cast <- pierre_hyp_cast[ , paste0(c("1", "2", "3", "4", "5", "6", "sum_col")) := NULL]

###### Pierre smoking -------------------

pierre_smoke <- as.data.table(pierre_events[c(1, 18)])
pierre_smoke[, id_smoke := 1:.N, by = ID_PATIENT]
pierre_smoke_cast <- dcast(pierre_smoke[,list(ID_PATIENT, id_smoke, EVER_SMOKED_AT_EVENT)], ID_PATIENT ~ id_smoke, value.var = 'EVER_SMOKED_AT_EVENT', fill = 0)

pierre_smoke_cast <- as.data.table(pierre_smoke_cast)
pierre_smoke_cast <- pierre_smoke_cast[, lapply(.SD, as.numeric), by = ID_PATIENT]

pierre_smoke_cast <- dichotomous(pierre_smoke_cast)
colnames(pierre_smoke_cast)[9] <- "smoke_yes"
pierre_smoke_cast <- pierre_smoke_cast[ , paste0(c("1", "2", "3", "4", "5", "6", "sum_col")) := NULL]

########################## Pierre BMI ----------------------

pierre_bmi <- as.data.table(pierre_events[c(1, 19)])
pierre_bmi[, id_bmi := 1:.N, by = ID_PATIENT]
pierre_bmi_cast <- dcast(pierre_bmi[,list(ID_PATIENT, id_bmi, BMI_AT_EVENT)], ID_PATIENT ~ id_bmi, value.var = 'BMI_AT_EVENT', fill = 0)

bmi_avg <- data.frame(round(Reduce(`+`, pierre_bmi_cast[-1]) / rowSums(pierre_bmi_cast[-1] != 0), 2))
names(bmi_avg)[1] <- "avg"

pierre_bmi_avg <- as.data.frame(cbind(pierre_bmi_cast$ID_PATIENT, bmi_avg$avg))
colnames(pierre_bmi_avg)[c(1:2)] <- c("ID_PATIENT", "avg")

################### Pierre treatment ------------------
#Started treatment at event

pierre_trt <- as.data.table(pierre_events[c(1, 12)])
pierre_trt[, id_trt := 1:.N, by = ID_PATIENT]
pierre_trt_cast <- dcast(pierre_trt[,list(ID_PATIENT, id_trt, STARTED_TREATMENT_AT_EVENT)], ID_PATIENT ~ id_trt, value.var = 'STARTED_TREATMENT_AT_EVENT', fill = 0)

pierre_trt_cast <- as.data.table(pierre_trt_cast)
pierre_trt_cast <- pierre_trt_cast[, lapply(.SD, as.numeric), by = ID_PATIENT]

pierre_trt_cast <- dichotomous(pierre_trt_cast)
colnames(pierre_trt_cast)[9] <- "trt_yes"
pierre_trt_cast <- pierre_trt_cast[ , paste0(c("1", "2", "3", "4", "5", "6", "sum_col")) := NULL]


########### Pierre death ----------

pierre_age_death <- as.data.table(pierre_death[c(1, 25)])
pierre_age_death[, id_age_death := 1:.N, by = ID_PATIENT]
pierre_age_death_cast <- dcast(pierre_age_death[,list(ID_PATIENT, id_age_death, age_binned)],
                               ID_PATIENT ~ id_age_death, value.var = 'age_binned', fill = 0)

names(pierre_age_death_cast)[2] <- "age_at_death"

################ Create Pierre master ----------

comb2 <- function(df1, df2) {
  master <- as.data.table(merge(df1, df2, by = "ID_PATIENT", all.x = TRUE))
  master
}

pierre_px_master <- comb2(pierre_px, pierre_cd4_all)
pierre_px_master <- comb2(pierre_px_master, pierre_hepc_cast)
pierre_px_master <- comb2(pierre_px_master, pierre_hepb_cast)
pierre_px_master <- comb2(pierre_px_master, pierre_bmi_cast)
pierre_px_master <- comb2(pierre_px_master, pierre_hyp_cast)
pierre_px_master <- comb2(pierre_px_master, pierre_trt_cast)
pierre_px_master <- comb2(pierre_px_master, pierre_smoke_cast)
pierre_px_master <- comb2(pierre_px_master, pierre_age_death_cast)


pierre_px_master$DTE_END_STUDY <- as.Date(pierre_px_master$DTE_END_STUDY, format="%d/%m/%Y")
subset_pierre <- subset(pierre_px_master, LTFU == 1)

subset_pierre2 <- subset(subset_pierre, DTE_END_STUDY > as.Date("2014-01-01"))

######################### Pierre LTFU -----------

## Create dataframes of the not_LTFU px that should be used for 2015 snapshot

#first subset living patients

pierre_alive <- subset(pierre_px, DEATH == "0")

#subset to take a closer look at LTFU
pierre_LTFU <- subset(pierre_alive, LTFU == "1")
pierre_not_LTFU <- subset(pierre_alive, LTFU == "0")

mean(pierre_LTFU$age, na.rm=TRUE)
mean(pierre_not_LTFU$age, na.rm = TRUE)

pierre_not_LTFU <- binning_ages(pierre_not_LTFU)

#################### Pierre NICMs -----------------------

pierre_px_master$new_counts <- pierre_px_master$RENAL_DISEASE + pierre_px_master$CVD + 
  pierre_px_master$DIABETE + pierre_px_master$HEPATIC_DECOMP + pierre_px_master$LIVER_CANCER + 
  pierre_px_master$HODG_LYMP + pierre_px_master$LUNG_CANCER + pierre_px_master$ANAL_CANCER

pierre_px_master$CM_yes <- ifelse(pierre_px_master$new_counts>=1, 1, 0)

#create a new column that is Non-AIDS defining malignancies to match Liege data
pierre_px_master$NADM <- pierre_px_master$LUNG_CANCER + 
  pierre_px_master$ANAL_CANCER + pierre_px_master$HODG_LYMP + pierre_px_master$LIVER_CANCER



pierre_events_plot <- as.data.frame(c("RD", "DIA", "CVD", "NADM"))
pierre_events_plot$Freq <- (c(402, 160, 79, 43))
names(pierre_events_plot)[1] <- "CM"

pierre_plot <- ggplot(data=pierre_events, aes(x=1, y=Freq, fill = CM)) + 
  geom_bar(stat="identity", color="black") + 
  ggtitle("Comorbidities (non-AIDS defining) - St. Pierre") + 
  coord_polar(theta='y') + geom_bar(stat="identity",) + 
  guides(fill=guide_legend(override.aes=list(colour=NA))) + 
  theme(axis.ticks=element_blank(), axis.title=element_blank(), axis.text.y=element_blank()) 

y.breaks <- cumsum(pierre_events$Freq) - pierre_events$Freq/2

pierre_pie <- pierre_bar + theme(axis.text.x=element_text(color='black')) +
  scale_y_continuous(
    breaks=y.breaks,   # where to place the labels
    labels=pierre_events$CM # the labels
  ) + scale_fill_discrete(name="Comorbidity Legend",
                          breaks=c("DIA", "CVD", "NADM", "RD"),
                          labels=c("Diabetes mellitus", "Cardiovascular disease", "Non-AIDS defining malignancies", "Renal disease"))
####
pierre_bar <- ggplot(data=pierre_events, aes(x=CM, y=Freq, fill=CM)) + geom_bar(stat="identity") + ggtitle("Comorbidities (non-AIDS defining) - St. Pierre") + scale_fill_discrete(name="Comorbidity Legend", breaks=c("DIA", "CVD", "NADM", "RD")

                                                                                                                                                                                   
#######


pierre_events_alive <- subset(pierre_events, EVENT == "EFU")
names(pierre_events_alive)[6] <- "age"
pierre_events_alive <- binning_ages(pierre_events_alive)

pierre_age_event <- as.data.table(pierre_events_alive[c(1, 25)])
pierre_age_event[, id_age_event := 1:.N, by = ID_PATIENT]
pierre_age_event_cast <- dcast(pierre_age_event[,list(ID_PATIENT, id_age_event, age_binned)],
                               ID_PATIENT ~ id_age_event, value.var = 'age_binned', fill = 0)

names(pierre_age_event_cast)[2] <- "age_at_EFU"


pierre_px_master_new <- Reduce(function(x, y) merge(x, y, by= "ID_PATIENT", all=TRUE), 
                               list(pierre_px, pierre_bmi_cast, 
                                    pierre_smoke_cast, pierre_hyp_cast, 
                                    pierre_trt_cast, pierre_hepb_cast,
                                    pierre_hepc_cast, pierre_cd4_all,
                                    pierre_age_death_cast, pierre_age_event_cast))


################## Pierre drug data --------------------------------
View(pierre_events)
pierre_renal <- subset(pierre_events, OUTCOME == "1 - RENAL DISEASE")
pierre_cvd <- subset(pierre_events, OUTCOME == "2 - CVD")


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

comb3 <- function(df1, df2) {
  master <- as.data.table(merge(df1, df2, by = "patient_id", all.x = TRUE))
  master
}

a <- as.Date(erasme_new$LAST_VISIT,format="%d/%m/%Y")
b <- as.Date(erasme_new$LAST_VISIT,format="%d-%m-%Y") 
a[is.na(a)] <- b[!is.na(b)] # Combine both while keeping their ranks
erasme_new$LAST_VISIT <- a # Put it back in your dataframe

erasme_master <- comb3(erasme_base, erasme_new)
erasme_master$DEATH_Date <- as.Date(erasme_master$DEATH_Date,format="%d-%m-%Y") 

erasme_master$status[erasme_master$LAST_VISIT > as.Date("2014-07-01")] <- "Follow up"
erasme_master$status[erasme_master$LAST_VISIT < as.Date("2014-07-01")] <- "LTFU"
erasme_master$status[erasme_master$DEATH_Date > as.Date("2000-07-01")] <- "Death"


############# Erasme nadir  ---------------

erasme_nadir$cd4_nadir_v <- as.numeric(erasme_nadir$cd4_nadir_v)
mean(erasme_nadir$cd4_nadir_v, na.rm=TRUE)
erasme_master <- comb3(erasme_master, erasme_nadir)

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

erasme_bmi_recent <- setDT(erasme_bmi)[,.SD[which.max(vis_d)],keyby=patient_id]

erasme_master <- merge(erasme_master, erasme_bmi_recent, by= "patient_id", all.y=FALSE)

######## erasme smoking -------------

table(erasme_not_LTFU$HAS_SMOKED)

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
                                                     Country_of_origin == 'Dominicaine, Republique'] <- 'Central America/Caribbean')

erasme_master <- within(erasme_master, Country_of_origin[Country_of_origin == 'Bresil' | Country_of_origin == 'Perou' |
                                                     Country_of_origin == 'equateur' | Country_of_origin == 'Venezuela' |
                                                     Country_of_origin == 'Chili' | Country_of_origin == 'Colombie'] <- 'South America')

erasme_master <- within(erasme_master, Country_of_origin[Country_of_origin == 'Maroc' | Country_of_origin == 'Algerie'
                                                   | Country_of_origin == 'Isra\xebl'] <- 'North Africa/Middle East')


erasme_master <- within(erasme_master, Country_of_origin[Country_of_origin == 'Canada' | Country_of_origin == 'etats-Unis'] <- 'North America')

names(erasme_master)[8] <- "REGION_OF_ORIGIN"
names(erasme_alive_not_LTFU)[8] <- "REGION_OF_ORIGIN"
ethnic_freq(erasme_alive_not_LTFU)

######################erasme liver -------------------

#make new columns for HCV and HBV status

erasme_liver$hepb_yes <- ifelse(erasme_liver$Medical.history.report == "Hepatite B chronique", 1, 0)
erasme_liver$hepc_yes <- ifelse(erasme_liver$Medical.history.report == "Hepatite C chronique", 1, 0)

#merge with erasme_master
erasme_master <- comb3(erasme_master, erasme_liver)

#make the NAs 0 for hepb and hepc
erasme_master$hepb_yes[is.na(erasme_master$hepb_yes)] <- 0
erasme_master$hepc_yes[is.na(erasme_master$hepc_yes)] <- 0

####### Erasme NICMs -------
erasme_master_hepc <- subset(erasme_master, hepc_yes == 1)
table(duplicated(erasme_master_hepc$patient_id))
#20
20/875

###### Erasme LTFU -----

erasme_LTFU <- subset(erasme_master, status == "LTFU")
erasme_not_LTFU <- subset(erasme_master, status == "Follow up")


mean(erasme_alive_not_LTFU$AGE)
mean(erasme_LTFU$AGE)

erasme_cvd <- erasme_cvd[!duplicated(erasme_cvd$patient_id),]
erasme_cvd_total <- merge(erasme_cvd, erasme_not_LTFU, by="patient_id", all=FALSE)
32/875

erasme_renal <- erasme_renal[!duplicated(erasme_renal$patient_id),]
erasme_renal_total <- merge(erasme_renal, erasme_not_LTFU, by="patient_id", all=FALSE)
25/875

erasme_diabetes <- erasme_diabetes[!duplicated(erasme_diabetes$patient_id),]
erasme_diabetes_total <- merge(erasme_diabetes, erasme_not_LTFU, by="patient_id", all=FALSE)
43/875

erasme_cancer <- erasme_cancer[!duplicated(erasme_cancer$patient_id),]
erasme_cancer_total <- merge(erasme_cancer, erasme_not_LTFU, by="patient_id", all=FALSE)
14/875

########################### erasme drug data -----------------


#find the maximum value of years on any ART for each patient
erasme_years_ART <- aggregate(art_duration ~ patient_id, data = erasme_ART, max)

#this is most recent cd4 count, still waiting on nadir

###Notes

###when comining liver data with base dataset, 0 = no liver disease
#mortality data: need to divide the cause specific mortality rate by the number of
# px from each age group in the cohort, graph next to the normal background mortality
#of Belgium, saved in Excel
#table on p 60 of CDC document could be a good model for summary statistics table

#case rates by region of origin? (cases per 100,000 population)

#exposure category for Pierre and Liege? would need to ask for it

#dcast data

erasme_ART <- as.data.table(erasme_ART)
erasme_ART[, id := 1:.N, by = patient_id]
erasme_ART_cast <- dcast(erasme_ART[,list(patient_id, id, art_name)], patient_id ~ id, value.var = 'art_name', fill = 0)

erasme_ART_num <- subset(erasme_ART, art_name != is.na(art_name))


erasme_ART$art_start.date <- as.Date(erasme_ART$art_start.date, 
                                     format = "%d-%m-%Y")
erasme_ART$art_end.date <- as.Date(erasme_ART$art_end.date, 
                                   format = "%d-%m-%Y")

#subset to just the drugs that patients are still on (no end date)
erasme_ART2015 <- subset(erasme_ART, is.na(erasme_ART$art_end.date))

erasme_ART2015 <- as.data.table(erasme_ART2015)
erasme_ART2015[, id := 1:.N, by = patient_id]
erasme_ART2015_cast <- dcast(erasme_ART2015[,list(patient_id, id, art_name)], patient_id ~ id, value.var = 'art_name', fill = 0)

#make the ID column a factor instead of integer
erasme_ART2015$patient_id <- factor(erasme_ART2015$patient_id)    

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

library(dplyr) # for the chaining (%>%) operator

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
erasme_alive_art <- merge(erasme_alive, erasme_ART, by = "patient_id", all.x=FALSE)
#find the max value in art_duration for each px
erasme_alive_art <- as.data.table(erasme_alive_art)
erasme_alive_art[, id := 1:.N, by = patient_id]
erasme_alive_art <- dcast(erasme_alive_art[,list(patient_id, id, art_duration)], patient_id ~ id, value.var = 'art_duration', fill = 0)

erasme_alive_art$max <- apply(erasme_alive_art[, 2:37], 1, max)
mean(erasme_alive_art$max)

mean(liege_not_LTFU$years_on_ART, na.rm=TRUE)

#end of Erasme cleaning
##############################################################################################
###### Ghent LTFU ---------

#easiest may be to subset just the date of last visit and merge it with ghent_new

ghent_last_vis <- data.frame(PATIENT_ID = ghent_px1$PATIENT_ID, LAST_VISIT_DATE = as.Date(ghent_px1$LAST_VISIT_DATE))
ghent_last_vis <- ghent_last_vis[!duplicated(ghent_last_vis),]

ghent_px_master <- merge(ghent_px2, ghent_last_vis, by="PATIENT_ID")

ghent_px_master$status[ghent_px_master$LAST_VISIT_DATE > as.Date("2014-07-01")] <- "Follow up"
ghent_px_master$status[ghent_px_master$LAST_VISIT_DATE < as.Date("2014-07-01")] <- "LTFU"

ghent_px_master$DATE_DEATH <- as.Date(ghent_px_master$DATE_DEATH, format = "%Y-%m-%d")
ghent_px_master$status[ghent_px_master$DATE_DEATH > as.Date("2000-07-01")] <- "Death"

ghent_px_master$CURRENT.LEGAL.SEX <- gsub("M", "Male", ghent_px_master$CURRENT.LEGAL.SEX)
ghent_px_master$CURRENT.LEGAL.SEX <- gsub("F", "Female", ghent_px_master$CURRENT.LEGAL.SEX)

#inclusion criteria - visit between 2005-2015
ghent_px_remove <- subset(ghent_px_master, LAST_VISIT_DATE < as.Date("2005-01-01"))
ghent_px_master <- ghent_px_master[!(ghent_px_master$LAST_VISIT_DATE < as.Date("2005-01-01")),]

ghent_LTFU <- subset(ghent_px_master, status == "LTFU")
ghent_not_LTFU <- subset(ghent_px_master, status == "Follow up")

mean(ghent_LTFU$AGE, na.rm=TRUE)
mean(ghent_not_LTFU$AGE, na.rm=TRUE)


################## Ghent ethnic --------------

table(ghent_living_not_LTFU$REGION.OF.ORIGIN..10.White..20.Black..21.Black.African.22.Black.Carribean..30.Hispanic..40.Asian.50.American.60.Indigenous.97.other.99.unknown.)


################### Ghent NICM ------------

#subset them all
ghent_dia <- subset(ghent_px_master, DIABETES == 1)
ghent_mi <- subset(ghent_px_master, MI == 1)
ghent_str <- subset(ghent_px_master, STROKE == 1)
ghent_cvd <- merge(ghent_mi, ghent_str, by="PATIENT_ID", all=TRUE)
ghent_fra <- subset(ghent_px_master, FRACTURES == 1)
ghent_renal <- subset(ghent_px_master, RENAL_DISEASE == 1)
ghent_px_master$nadm <- ghent_px_master$NHL + ghent_px_master$AC + ghent_px_master$LC +
  ghent_px_master$CC 
ghent_nadm <- subset(ghent_px_master, nadm > 0)

ghent_hepc <- subset(ghent_px1, HCVRNA_AT.EVENT == "POS")

# not LTFU
ghent_dia2 <- subset(ghent_not_LTFU, DIABETES == 1)
ghent_mi2 <- subset(ghent_not_LTFU, MI == 1)
ghent_str2 <- subset(ghent_not_LTFU, STROKE == 1)
ghent_cvd2 <- merge(ghent_mi, ghent_str, by="PATIENT_ID", all=TRUE)
ghent_cvd3 <- merge(ghent_cvd2, ghent_not_LTFU, by="PATIENT_ID", all=FALSE)
ghent_fra2 <- subset(ghent_not_LTFU, FRACTURES == 1)
ghent_renal2 <- subset(ghent_not_LTFU, RENAL_DISEASE == 1)
ghent_not_LTFU$nadm <- ghent_not_LTFU$NHL + ghent_not_LTFU$AC + ghent_not_LTFU$LC +
  ghent_not_LTFU$CC 
ghent_nadm <- subset(ghent_not_LTFU, nadm > 0)

ghent_hepc <- subset(ghent_px1, HCVRNA_AT.EVENT == "POS")
ghent_hepc_FU <- merge(ghent_hepc, ghent_not_LTFU, by="PATIENT_ID", all=FALSE)

################# all sites mortality ----------------------

names(erasme_base)[5] <- "age"
erasme_base$age <- as.numeric(erasme_base$age)
erasme_base <- binning_ages(erasme_base)
pierre_px_master <- binning_ages(pierre_px_master)

pierre_age_death <- subset(pierre_events, OUTCOME == "12 - DEATH")
names(pierre_age_death)[6] <- "age"
pierre_age_death$age <- as.numeric(pierre_age_death$age)
pierre_age_death <- binning_ages(pierre_age_death)
pierre_death <- merge(pierre_age_death, pierre_px_master, by = "ID_PATIENT")
names(pierre_death)[25] <- "age_binned"

liege_death <- subset(liege_master2, STATUS == "Death")
erasme_death <- subset(erasme_base, DEATH_Date != is.na(erasme_base$DEATH_Date))
ghent_death <- subset(ghent_px_master, status == "Death")

#problems. ghent is not prepared
names(ghent_death)[3] <- "age"
ghent_death$age <- as.numeric(ghent_death$age) 
ghent_death <- binning_ages(ghent_death)

names(ghent_px_master)[3] <- "age"
ghent_px_master$age <- as.numeric(ghent_px_master$age) 
ghent_px_master <- binning_ages(ghent_px_master)

tab <- function(df) {
  base <- as.data.frame(table(df$age_binned))
  base
}

pierre_tab <- tab(pierre_px_master)
pierre_tab_death <- tab(pierre_death)
liege_tab <- tab(liege_master2)
liege_tab_death <- tab(liege_death)
erasme_tab <- tab(erasme_base)
erasme_tab_death <- tab(erasme_death)
ghent_tab <- tab(ghent_px_master)
ghent_tab_death <- tab(ghent_death)

#make a new column that is a proportion of deaths by age group

for (i in liege_tab_death) {
  liege_tab_death[3] <- i/sum(liege_tab_death$Freq)
  liege_tab_death
}

for (i in pierre_tab_death) {
  pierre_tab_death[3] <- i/sum(pierre_tab_death[2])
  pierre_tab_death
}

for (i in erasme_tab_death) {
  erasme_tab_death[3] <- i/(sum(erasme_tab_death$Freq))
  erasme_tab_death
}

for (i in ghent_tab_death) {
  ghent_tab_death[3] <- i/(sum(ghent_tab_death$Freq))
  ghent_tab_death
}

for (i in liege_tab) {
  liege_tab[3] <- i/sum(liege_tab$Freq)
  liege_tab
}

for (i in pierre_tab) {
  pierre_tab[3] <- i/sum(pierre_tab[2])
  pierre_tab
}

for (i in erasme_tab) {
  erasme_tab[3] <- i/(sum(erasme_tab$Freq))
  erasme_tab
}

for (i in ghent_tab) {
  ghent_tab[3] <- i/(sum(ghent_tab$Freq))
  ghent_tab
}

#make one data frame that combines the proportion of deaths in each age group
#include Ghent

avg_pops_df <- as.data.frame(cbind(liege_tab[1], liege_tab$V3, pierre_tab$V3, 
                                   erasme_tab$V3, ghent_tab$V3))

avg_deaths_df <- as.data.frame(cbind(liege_tab[1], liege_tab_death$V3, pierre_tab_death$V3, 
                                   erasme_tab_death$V3, ghent_tab_death$V3))

colnames(avg_pops_df)[c(1:5)] <- (c("Age_groups", "Liege", "St. Pierre", "Erasme", "Ghent"))
colnames(avg_deaths_df)[c(1:5)] <- (c("Age_groups", "Liege", "St. Pierre", "Erasme", "Ghent"))

avg_pops_df$avg <- (rowMeans(avg_pops_df[,-1]))
avg_deaths_df$avg <- (rowMeans(avg_deaths_df[,-1]))

#make a graph of proportions of deaths
proportion_mortality <- ggplot(data=avg_deaths_df,aes(x=Age_groups, y=avg, group=1)) +
  geom_line(size=1.5) + xlab("Age groups") +
  ylab("Percentage of total deaths by age group") +
  ggtitle("Proportion of deaths by age group - all cohorts") +
  theme(plot.title = element_text(size = 20))


liege_tab$crude <- (liege_tab$V3*liege_tab_death$Freq)
pierre_tab$crude <- (pierre_tab$V3*pierre_tab_death$Freq)
erasme_tab$crude <- (erasme_tab$V3*erasme_tab_death$Freq)
ghent_tab$crude <- (ghent_tab$V3*ghent_tab_death$Freq)

liege_tab$adj <- (avg_pops_df$avg*liege_tab_death$Freq)
pierre_tab$adj <- (avg_pops_df$avg*pierre_tab_death$Freq)
erasme_tab$adj <- (avg_pops_df$avg*erasme_tab_death$Freq)
ghent_tab$adj <- (avg_pops_df$avg*ghent_tab_death$Freq)



mortal_tabs_crude <- as.data.frame(cbind(liege_tab[1], liege_tab$crude, 
                                         pierre_tab$crude, erasme_tab$crude, ghent_tab$crude))
colnames(mortal_tabs_crude)[c(1:5)] <- c("Age", "Liege", "St. Pierre", "Erasme", "Ghent")


mortal_tabs_crude_long <- melt(mortal_tabs_crude, id="Age")  # convert to long format
names(mortal_tabs_crude_long)[2] <- "Cohort"

crude_mortality <- ggplot(data=mortal_tabs_crude_long,
                          aes(x=Age, y=value, group = Cohort, colour=Cohort)) +
  geom_line(size=1.5) + ylab("Mortality rate") + ggtitle("Age-adjusted crude mortality rates by cohort")


mortal_tabs <- as.data.frame(cbind(liege_tab[1], liege_tab$adj, pierre_tab$adj, erasme_tab$adj, ghent_tab$adj))
colnames(mortal_tabs)[c(1:5)] <- c("Age", "Liege", "St. Pierre", "Erasme", "Ghent")

mortal_tabs_long <- melt(mortal_tabs, id="Age")  # convert to long format
names(mortal_tabs_long)[2] <- "Cohort"

standardized_mortality <- ggplot(data=mortal_tabs_long,
       aes(x=Age, y=value, group = Cohort, colour=Cohort)) +
  geom_line(size=1.5) + ylab("Mortality rate") + ggtitle("Age-adjusted standardized mortality rates by cohort")


grid.arrange(crude_mortality, standardized_mortality)


################ combined mortality graph--------------

all_tabs <- as.data.frame(cbind(liege_tab$Freq, pierre_tab$Freq, erasme_tab$Freq, ghent_tab$Freq))
all_tabs$sum <- rowSums(all_tabs)

all_tabs_death <- as.data.frame(cbind(liege_tab_death$Freq,
                                      pierre_tab_death$Freq, erasme_tab_death$Freq,
                                      ghent_tab_death$Freq))
all_tabs_death$sum <- rowSums(all_tabs_death)
sum(all_tabs_death$sum)/sum(all_tabs$sum)

all_tabs$mortality <- (all_tabs_death$sum)/(all_tabs$sum)
sum(all_tabs$mortality, na.rm=TRUE)
all_tabs$mortality <- all_tabs$mortality*100
all_tabs <- as.data.frame(cbind(liege_tab[1], all_tabs))

mort_total_plot <- ggplot(data=all_tabs,
                          aes(x=Var1, y=mortality, group=1)) +
  geom_line(size=1.5) + xlab("Age groups") + 
  ylab("Mortality rate (per 100)") + 
  ggtitle("Age-adjusted crude mortality rate, all cohorts") +
  theme(plot.title = element_text(size = 20)) + 
  theme(axis.title = element_text(size = 15))

#get the overall crude mortality rates for each cohort

sum(all_tabs_death$V1)/sum(all_tabs$V1)
sum(all_tabs_death$V2)/sum(all_tabs$V2)
sum(all_tabs_death$V3)/sum(all_tabs$V3)
sum(all_tabs_death$V4)/sum(all_tabs$V4)

#total pop mortality rate
sum(all_tabs_death$sum)/sum(all_tabs$sum)



########################## subsetting by age groups pierre ----------
##### change the color pal and remove the labels on the plot

age_subset <- subset(pierre_px_master_new, age_at_EFU == "46-50")
death_age_subset <- subset(age_subset, DEATH == 1)
living_age_subset <- subset(age_subset, DEATH == 0)

#make pie charts with region of origin for living and dead
death_age_subset_tab <- as.data.frame(table(death_age_subset$REGION_OF_ORIGIN))

pierre_death_age_plot <- ggplot(data=death_age_subset_tab, aes(x=1, y=Freq, fill = Var1)) + 
  geom_bar(stat="identity", color="black", position = "fill") + 
  ggtitle("Region of origin - St. Pierre deaths ages 46-50") + 
  coord_polar(theta='y') +
  guides(fill=FALSE) +
  scale_fill_brewer(palette="Paired") +
  theme(axis.ticks=element_blank(), axis.title=element_blank(), 
        axis.text=element_blank(), plot.title = element_text(size = 20)) 

#living

living_age_subset_tab <- as.data.frame(table(living_age_subset$REGION_OF_ORIGIN))

pierre_living_age_plot <- ggplot(data=living_age_subset_tab, aes(x=1, y=Freq, fill = Var1)) + 
  geom_bar(stat="identity", color= "black", position="fill") + 
  ggtitle("Region of origin - St. Pierre living ages 46-50") + 
  coord_polar(theta='y') +
  scale_fill_brewer(palette ="Paired") + 
  guides(fill=guide_legend("Region of origin")) + 
  theme(axis.ticks=element_blank(), 
        axis.title=element_blank(), axis.text=element_blank(),
        plot.title = element_text(size = 20)) 


grid.arrange(pierre_death_age_plot, pierre_living_age_plot, ncol=2)

table(death_age_subset$hepc_yes)
table(living_age_subset$hepc_yes)

table(death_age_subset$RENAL_DISEASE)
table(living_age_subset$RENAL_DISEASE)


table(death_age_subset$CVD)
table(living_age_subset$CVD)
#chi square
#death rate over all

hepb_mat <- matrix(c(660, 31, 33, 2), ncol = 2)
dimnames(hepb_mat) <- list(hepb = c("no","yes"),
                           death= c("no", "yes"))

hepc_mat <- matrix(c(630, 61, 24, 11), ncol = 2)
dimnames(hepc_mat) <- list(hepc = c("no","yes"),
                           death= c("no", "yes"))

renal_mat <- matrix(c(651, 40, 23, 12), ncol = 2)
dimnames(renal_mat) <- list(renal = c("no","yes"),
                            death= c("no", "yes"))

cvd_mat <- matrix(c(681, 10, 33, 2), ncol = 2)
dimnames(cvd_mat) <- list(cvd = c("no","yes"),
                          death= c("no", "yes"))

smoke_mat <- matrix(c(405, 286, 14, 21), ncol = 2)
dimnames(smoke_mat) <- list(smoke = c("no","yes"),
                            death= c("no", "yes"))

hyp_mat <- matrix(c(372, 319, 31, 4), ncol = 2)
dimnames(hyp_mat) <- list(hyp = c("no","yes"),
                          death= c("no", "yes"))

(660/(660+33))/(31/(31+2))
(630/(630+24))/(61/(61+11))
(651/(651+23))/(40/(40+12))

calcRelativeRisk <- function(mymatrix,alpha=0.05,referencerow=2)
{
  numrow <- nrow(mymatrix)
  myrownames <- rownames(mymatrix)
  for (i in 1:numrow)
  {
    rowname <- myrownames[i]
    DiseaseUnexposed <- mymatrix[referencerow,1]
    ControlUnexposed <- mymatrix[referencerow,2]
    if (i != referencerow)
    {
      DiseaseExposed <- mymatrix[i,1]
      ControlExposed <- mymatrix[i,2]
      totExposed <- DiseaseExposed + ControlExposed
      totUnexposed <- DiseaseUnexposed + ControlUnexposed
      probDiseaseGivenExposed <- DiseaseExposed/totExposed
      probDiseaseGivenUnexposed <- DiseaseUnexposed/totUnexposed
      
      # calculate the relative risk
      relativeRisk <- probDiseaseGivenExposed/probDiseaseGivenUnexposed
      print(paste("category =", rowname, ", relative risk = ",relativeRisk))
      
      # calculate a confidence interval
      confidenceLevel <- (1 - alpha)*100
      sigma <- sqrt((1/DiseaseExposed) - (1/totExposed) +
                      (1/DiseaseUnexposed) - (1/totUnexposed))
      # sigma is the standard error of estimate of log of relative risk
      z <- qnorm(1-(alpha/2))
      lowervalue <- relativeRisk * exp(-z * sigma)
      uppervalue <- relativeRisk * exp( z * sigma)
      print(paste("category =", rowname, ", ", confidenceLevel,
                  "% confidence interval = [",lowervalue,",",uppervalue,"]"))
    }
  }
}

calcRelativeRisk(hepc_mat)
calcRelativeRisk(hepb_mat)
calcRelativeRisk(renal_mat)
calcRelativeRisk(cvd_mat)
calcRelativeRisk(smoke_mat)
calcRelativeRisk(hyp_mat)

#relative risk
library(epitools)
chisq.test(hepc_matrix)
epitab(hepb_mat, method="riskratio")


###################### LTFU graphs -------------------------------

#LTFU graphs

#histograms

pierre_LTFU_histo <- ggplot(pierre_alive, binwidth = 5, aes(x=age, stat="count")) + 
  geom_histogram(data=subset(pierre_not_LTFU,LTFU == 0),fill = "red", 
                 alpha = 0.4, binwidth = 5) +
  geom_histogram(data=subset(pierre_LTFU,LTFU == 1),fill = "blue", 
                 alpha = 0.4, binwidth = 5) + 
  scale_x_continuous(breaks=seq(0,90,5)) + 
  ggtitle("St. Pierre") +
  theme(plot.title = element_text(size = 20), axis.title.y=element_blank(),
        axis.title.x=element_blank()) +
  coord_cartesian(xlim=c(0,90))

#liege_alive <- subset(liege_master2, STATUS != "Death")

liege_LTFU_histo <- ggplot(liege_alive, binwidth = 5, aes(x=age, stat="count")) + 
  geom_histogram(data=subset(liege_alive,STATUS == "Follow up"),fill = "red", 
                 alpha = 0.4, binwidth = 5) +
  geom_histogram(data=subset(liege_alive,STATUS == "Contact lost"),fill = "blue", 
                 alpha = 0.4, binwidth = 5) + 
  scale_x_continuous(breaks=seq(0,90,5)) + 
  ggtitle("Liege") +
  theme(plot.title = element_text(size = 20), axis.title.y=element_blank(),
        axis.title.x=element_blank()) +
  coord_cartesian(xlim=c(0,90))

erasme_LTFU_histo <- ggplot(erasme_master, binwidth = 5, aes(x=AGE, stat="count")) + 
  geom_histogram(data=subset(erasme_master,status == "Follow up"),fill = "red", 
                 alpha = 0.4, binwidth = 5) +
  geom_histogram(data=subset(erasme_master,status == "LTFU"),fill = "blue", 
                 alpha = 0.4, binwidth = 5) + 
  scale_x_continuous(breaks=seq(0,90,5)) + 
  ggtitle("Erasme") +
  theme(plot.title = element_text(size = 20), axis.title.y=element_blank(),
        axis.title.x=element_blank()) +
  coord_cartesian(xlim=c(0,90))

ghent_LTFU_histo <- ggplot(ghent_px_master, binwidth = 5, aes(x=age, stat="count")) + 
  geom_histogram(data=subset(ghent_px_master,status == "Follow up"),fill = "red", 
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
  scale_fill_manual(values = c("seagreen3", "dodgerblue3", "salmon2")) + 
  scale_y_continuous(breaks=seq(0,50,5)) + 
  guides(fill=FALSE) + 
  ggtitle("BMI Distribution - Liège") + 
  xlab("Gender") +
  ylab("BMI")

#####pierre
#gender and bmi are in different databases

pierre_gen_bmi <- data.frame(merge(pierre_bmi_avg, pierre_not_LTFU,
                                   by = "ID_PATIENT", all=FALSE))

pierre_bmi <- ggplot(pierre_gen_bmi, aes(GENDER, avg, fill=GENDER)) + geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=5, size=6) + 
  scale_fill_manual(values = c("seagreen3", "dodgerblue3")) + 
  scale_y_continuous(breaks=seq(0,50,5)) + 
  ggtitle("BMI Distribution - St. Pierre") + 
  xlab("Gender") +
  ylab("BMI") +
  guides(fill=FALSE)

#Erasme

erasme_bmi_plot <- ggplot(erasme_not_LTFU, aes(GENDER, bmi, fill=GENDER)) + geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=5, size=6) + 
  scale_fill_manual(values = c("seagreen3", "dodgerblue3")) + 
  scale_y_continuous(breaks=seq(0,50,5)) + 
  ggtitle("BMI Distribution - Erasme") + 
  xlab("Gender") +
  ylab("BMI") +
  guides(fill=FALSE)

grid.arrange(liege_bmi, pierre_bmi, ncol=2)


###### ethnic plots --------
liege_ethnic_plot <- ggplot(data=liege_ethnic, aes(x=Var1, y=Freq, fill=Var1)) + geom_bar(stat="identity") 

LP <- ggplot(liege_country_tab, aes(x=1, y=Freq, fill=Var1)) +
  geom_bar(stat="identity")

LP <- LP + coord_polar(theta='y') + geom_bar(stat="identity", color="black") + 
  guides(fill=guide_legend(override.aes=list(colour=NA))) + 
  theme(axis.ticks=element_blank(), axis.title=element_blank(), axis.text.y=element_blank()) 

y.breaks <- cumsum(liege_country_tab$Freq) - liege_country_tab$Freq/2

liege_country_pie <- LP + theme(axis.text.x=element_text(color='black')) +
  scale_y_continuous(
    breaks=y.breaks,   
    labels=liege_country_tab$Var1 # the labels
  ) + scale_fill_discrete(name="Region of origin",
                          labels=c("Asia", "Central America", "Eastern Europe", "North America", "North Africa", 
                                   "North America", "South America", "Sub-Saharan Africa", "Unknown",
                                   "Western Europe"))
####
                                                                                                                                                                            labels=c("Acute coronary syndrome", "Ascites", "Diabetes mellitus", "End stage renal disease", "Bone fracture", "Acute Hepatitis C", "Non-AIDS defining malignancies", "Stroke"))


######################################################### age distros ----
names(erasme_not_LTFU)[5] <- "age"
names(ghent_not_LTFU)[3] <- "age"

erasme_not_LTFU <- binning_ages(erasme_not_LTFU)
ghent_not_LTFU <- binning_ages(ghent_not_LTFU)
pierre_not_LTFU <- binning_ages(pierre_not_LTFU)

#combine all the ages for summary statistics
ages_summary <- as.data.frame(c(liege_not_LTFU$age, ghent_not_LTFU$AGE, erasme_not_LTFU$AGE, 
                                pierre_not_LTFU$age))

summary(ages_summary[1])


#cut down on the size of the data frames, try just age first
#include only living patients

pierre_age_df <- data.frame(id = pierre_not_LTFU$ID_PATIENT, 
                            age = pierre_not_LTFU$age, 
                            age_binned = pierre_not_LTFU$age_binned, 
                            gender = pierre_not_LTFU$GENDER)
liege_age_df <- data.frame(id = liege_not_LTFU$IDENT_NR, 
                           age = liege_not_LTFU$age, 
                           age_binned = liege_not_LTFU$age_binned, 
                           gender = liege_not_LTFU$GENDER)
erasme_age_df <- data.frame(id = erasme_not_LTFU$patient_id, 
                            age = erasme_not_LTFU$age, 
                            age_binned = erasme_not_LTFU$age_binned, 
                            gender = erasme_not_LTFU$GENDER)
ghent_age_df <- data.frame(id = ghent_not_LTFU$PATIENT_ID, 
                            age = ghent_not_LTFU$age, 
                            age_binned = ghent_not_LTFU$age_binned, 
                            gender = ghent_not_LTFU$CURRENT.LEGAL.SEX)


#one combined DF
all_age <- rbind(pierre_age_df, liege_age_df, erasme_age_df, ghent_age_df)

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

### REMOVE AXIS LABELS

#boxplots, separate
#with diamond at the mean
liege_box_age <- ggplot(liege_age_df, aes(gender, age, fill=gender)) + geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=5, size=6) + 
  scale_fill_manual(values = c("seagreen3", "dodgerblue3", "salmon2")) + 
  scale_y_continuous(breaks=seq(0,95,5)) + 
  theme(legend.position="none", axis.title.x=element_blank(),
        axis.title.y=element_blank()) + 
  ggtitle("Liège")

pierre_box_age <- ggplot(pierre_age_df, aes(gender, age, fill=gender)) + geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=5, size=6) + 
  scale_fill_manual(values = c("seagreen3", "dodgerblue3")) + 
  scale_y_continuous(breaks=seq(0,95,5)) +
  theme(legend.position="none", axis.title.x=element_blank(),
        axis.title.y=element_blank()) + 
  ggtitle("St. Pierre")

erasme_box_age <- ggplot(erasme_age_df, aes(gender, age, fill=gender)) + geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=5, size=6) + 
  scale_fill_manual(values = c("seagreen3", "dodgerblue3")) + 
  scale_y_continuous(breaks=seq(0,95,5)) + 
  theme(legend.position="none", axis.title.x=element_blank(),
        axis.title.y=element_blank()) + 
  ggtitle("Erasme")

ghent_box_age <- ggplot(ghent_age_df, aes(gender, age, fill=gender)) + geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=5, size=6) + 
  scale_fill_manual(values = c("seagreen3", "dodgerblue3")) + 
  scale_y_continuous(breaks=seq(0,95,5)) + 
  theme(legend.position="none", axis.title.x=element_blank(),
        axis.title.y=element_blank()) + 
  ggtitle("Ghent")

all_box_age <- ggplot(all_age, aes(gender, age, fill=gender)) + geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=5, size=6) + 
  scale_fill_manual(values = c("seagreen3", "dodgerblue3", "salmon2")) + 
  scale_y_continuous(breaks=seq(0,95,5)) + 
  theme(legend.position="none", axis.title.x=element_blank(),
        axis.title.y=element_blank()) + 
  ggtitle("Age & Gender Distribution - All")

#to make a common legend for all
# go here: http://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots

grid.arrange(all_box_age, arrangeGrob(liege_box_age , pierre_box_age, erasme_box_age, 
                                      ghent_box_age, ncol=4), heights=c(2.5/4, 1.5/4), ncol=1)



###################### Cohort summaries --------------------
#Use the living patients not LTFU 


#check to make sure there is only one row per id
#use duplicated, table, summary commands

uniq_art <- data.frame(patient_id = unique(erasme_ART$patient_id))
uniq_art_merge <- merge(erasme_not_LTFU, uniq_art, by = "patient_id", all=FALSE)


#bring in Pierre CD4
names(pierre_cd4_cast)[1] <- "patient_id"
pierre_cd4_not_LTFU <- merge(pierre_cd4_cast, pierre_not_LTFU, by="patient_id")
names(pierre_cd4_not_LTFU)[8] <- "CD4_RECENT"
  
names(pierre_trt_cast)[1] <- "patient_id"
pierre_all <- merge(pierre_cd4_not_LTFU, pierre_trt_cast, by= "patient_id")

#percentage of treated patients in pierre
pierre_trt_yes <- subset(pierre_trt_cast, trt_yes == 1)
pierre_trt_merge <- merge(pierre_trt_yes, pierre_not_LTFU, by="ID_PATIENT", all=FALSE)

# 43% of Ghent patients unknown - not good enough drug data
date_unk <- subset(ghent_not_LTFU, START_DATE_ART == "UNK")

uniq_liege <- data.frame(unique(liege_art$IDENT_NR))
nrow(uniq_liege)
names(uniq_liege)[1] <- "IDENT_NR"
uniq_liege_merge <- merge(uniq_liege, liege_not_LTFU, by="IDENT_NR", all=FALSE)

(nrow(uniq_liege_merge))/(nrow(liege_not_LTFU))



#### avg recent CD4 count
names(erasme_cd4)[1] <- "patient_id"
erasme_cd4_merge <- merge(erasme_not_LTFU, erasme_cd4, by="patient_id", all=FALSE)
mean(erasme_cd4_merge$last.cd4_value, na.rm=TRUE)

#ghent
ghent_not_LTFU$MOST_RECENT_CD4 <- as.numeric(as.character(ghent_not_LTFU$MOST_RECENT_CD4))
mean(ghent_not_LTFU$MOST_RECENT_CD4, na.rm=TRUE)

#combine into one vector to get average recent CD4
cd4_all <- c(ghent_not_LTFU$MOST_RECENT_CD4, erasme_cd4_merge$last.cd4_value,
               liege_not_LTFU$CD4_RECENT, pierre_cd4_avg_df$avg)

mean(cd4_all, na.rm=TRUE)












###### fake datasets for examples

pierre_px <- as.data.table(pierre_px)
pierre_events <- as.data.table(pierre_events)

pierre_px_new <- pierre_px[, "COUNTRY_OF_ORIGIN" := NULL]
pierre_px_new <- pierre_px_new[, "NO_OUTCOME_NOT_LTFU" := NULL]
pierre_events_new <- pierre_events[, "AGE_AT_EVENT_CATEG" := NULL]

library(dplyr)
pierre_events_new <- pierre_events_new %>%
  mutate(OUTCOME = ifelse(is.na(OUTCOME),0,OUTCOME))

pierre_events_new2 <- subset(pierre_events_new, OUTCOME != "3 - HYPERTENSION")
