#Goals of this script: 
#1 - Summarize data from St Pierre and Liege


###something to consider: When a patient dies, there is also an event for end of follow up.
#need to check if it's identical to death data

#also, a more nuanced look at what's happening at the events will be necessary.
#for now, for example for HTA, I'm just looking at if the patient was ever treated

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

ghent_mortality <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/HIV/Data/Belgium/Ghent/Ghent_calculations.csv', header = T, na.strings=c(""))

##REDO ERASME LIVER CODING

###################### Liege - changes to orig. data ------------------

#make dates into as.Date and to change the fake dates to NA

names(liege_BAS)[11] <- 'DATE.START.ART'
liege_BAS[, cols <- grep("DATE", names(liege_BAS))] <- lapply(liege_BAS[, cols <- grep("DATE", names(liege_BAS))], as.Date, format = "%m/%d/%Y")

#make the fake placeholder date into NA
liege_BAS[liege_BAS == "1911-11-11"] <- NA

#subtract birth date from today's date, make new column for age
liege_BAS$sys_date <- Sys.Date()
#as.numeric(liege_BAS$sys_date, "%Y")

#from http://r.789695.n4.nabble.com/Calculate-difference-between-dates-in-years-td835196.html
age_years <- function(first, second) 
{ 
  df <- data.frame(first, second) 
  age <- as.numeric(format(df[,2],format="%Y")) - as.numeric(format(df[,1],format="%Y")) 
  first <- as.Date(paste(format(df[,2],format="%Y"),"-",format(df[,1],format="%m-%d"),sep="")) 
  age[which(first > df[,2])] <- age[which(first > df[,2])] - 1 
  age 
}

liege_BAS$age <- age_years(liege_BAS$BIRTH_DATE, liege_BAS$sys_date)

#do the same for the number of years on ART
liege_BAS$years_on_ART <- age_years(liege_BAS$DATE.START.ART, liege_BAS$sys_date)

#standardize spelling in ethnic
liege_BAS$ETHNIC <- gsub("africain", "Africain", liege_BAS$ETHNIC)
liege_BAS$ETHNIC <- gsub("caucasien", "Caucasien", liege_BAS$ETHNIC)

liege_CEP$CLIN_EVENT_DATE <- as.Date(liege_CEP$CLIN_EVENT_DATE, format = "%m/%d/%Y")
liege_CEP$CLIN_EVENT_DATE[liege_CEP$CLIN_EVENT_DATE == "1911-11-11"]<- NA

####################### Pierre - changes to orig. data -----------------

pierre_px$DOB <- as.Date(pierre_px$DOB, format = "%d/%m/%Y")

pierre_px$DOB[pierre_px$DOB == "1911-11-11"] <- NA

today <- Sys.Date()
pierre_px$sys_date <- as.Date(format(today, format = "%Y-%m-%d"))

#use previously defined age_years function
pierre_px$age <- age_years(pierre_px$DOB, pierre_px$sys_date)

##################### Age binning func. ----------------------

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

################## Master Liege df -------------------

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
#add to this as necessary throughout

############################### Num CMs ------------------

liege_master <- as.data.table(liege_master)

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


######################### Create LTFU dfs -----------

## Create dataframes of the not_LTFU px that should be used for 2015 snapshot
##### St Pierre

#first subset living patients
pierre_px <- binning_ages(pierre_px)

pierre_alive <- subset(pierre_px, DEATH == "0")

#subset to take a closer look at LTFU
pierre_LTFU <- subset(pierre_alive, LTFU == "1")
pierre_not_LTFU <- subset(pierre_alive, LTFU == "0")

pierre_not_LTFU <- binning_ages(pierre_not_LTFU)

#living, not LTFU px - Liege
liege_not_LTFU <- subset(liege_master2, STATUS == "Follow up")
liege_LTFU_tmp <- subset(liege_master2, STATUS != "Follow up")
liege_LTFU <- subset(liege_LTFU_tmp, STATUS != "Death")

liege_not_LTFU <- binning_ages(liege_not_LTFU)

#add to the not_LTFU df as necessary throughout

################# mortality ----------------------

names(erasme_base)[5] <- "age"
erasme_base$age <- as.numeric(erasme_base$age)
erasme_base <- binning_ages(erasme_base)

pierre_age_death <- subset(pierre_events, OUTCOME == "12 - DEATH")
names(pierre_age_death)[6] <- "age"
pierre_age_death$age <- as.numeric(pierre_age_death$age)
pierre_age_death <- binning_ages(pierre_age_death)
pierre_death <- merge(pierre_age_death, pierre_px, by = "ID_PATIENT")
names(pierre_death)[25] <- "age_binned"

liege_death <- subset(liege_master2, STATUS == "Death")
erasme_death <- subset(erasme_base, DEATH_Date != is.na(erasme_base$DEATH_Date))



tab <- function(df) {
  base <- as.data.frame(table(df$age_binned))
  base
}

pierre_tab <- tab(pierre_px)
pierre_tab_death <- tab(pierre_death)
liege_tab <- tab(liege_master2)
liege_tab_death <- tab(liege_death)
erasme_tab <- tab(erasme_base)
erasme_tab_death <- tab(erasme_death)

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

#make one data frame that combines the proportion of deaths in each age group
#include Ghent

avg_pops_df <- as.data.frame(cbind(liege_tab[1], liege_tab$V3, pierre_tab$V3, 
                                   erasme_tab$V3, ghent_mortality$Age_perc))

avg_deaths_df <- as.data.frame(cbind(liege_tab[1], liege_tab_death$V3, pierre_tab_death$V3, 
                                   erasme_tab_death$V3, ghent_mortality$Proportion))

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
ghent_mortality$crude <- (ghent_mortality$Age_perc*ghent_mortality$Total_deaths)

liege_tab$adj <- (avg_pops_df$avg*liege_tab_death$Freq)
pierre_tab$adj <- (avg_pops_df$avg*pierre_tab_death$Freq)
erasme_tab$adj <- (avg_pops_df$avg*erasme_tab_death$Freq)
ghent_mortality$adj <- (avg_pops_df$avg*ghent_mortality$Total_deaths)



mortal_tabs_crude <- as.data.frame(cbind(liege_tab[1], liege_tab$crude, 
                                         pierre_tab$crude, erasme_tab$crude, ghent_mortality$crude))
colnames(mortal_tabs_crude)[c(1:5)] <- c("Age", "Liege", "St. Pierre", "Erasme", "Ghent")


mortal_tabs_crude_long <- melt(mortal_tabs_crude, id="Age")  # convert to long format
names(mortal_tabs_crude_long)[2] <- "Cohort"

crude_mortality <- ggplot(data=mortal_tabs_crude_long,
                          aes(x=Age, y=value, group = Cohort, colour=Cohort)) +
  geom_line(size=1.5) + ylab("Mortality rate") + ggtitle("Age-adjusted crude mortality rates by cohort")


mortal_tabs <- as.data.frame(cbind(liege_tab[1], liege_tab$adj, pierre_tab$adj, erasme_tab$adj, ghent_mortality$adj))
colnames(mortal_tabs)[c(1:5)] <- c("Age", "Liege", "St. Pierre", "Erasme", "Ghent")

mortal_tabs_long <- melt(mortal_tabs, id="Age")  # convert to long format
names(mortal_tabs_long)[2] <- "Cohort"

standardized_mortality <- ggplot(data=mortal_tabs_long,
       aes(x=Age, y=value, group = Cohort, colour=Cohort)) +
  geom_line(size=1.5) + ylab("Mortality rate") + ggtitle("Age-adjusted standardized mortality rates by cohort")


grid.arrange(crude_mortality, standardized_mortality)


################ combined mortality graph--------------

all_tabs <- as.data.frame(cbind(liege_tab$Freq, pierre_tab$Freq, erasme_tab$Freq, ghent_mortality$Total_by_age))
all_tabs$sum <- rowSums(all_tabs)

all_tabs_death <- as.data.frame(cbind(liege_tab_death$Freq,
                                      pierre_tab_death$Freq, erasme_tab_death$Freq,
                                      ghent_mortality$Total_deaths))
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

########################## Pierre Hepatitis -------------------------------

#recode the HepB and C columns to be numeric
pierre_events$HBV_AT_EVENT <- as.character(pierre_events$HBV_AT_EVENT)

pierre_events$HBV_AT_EVENT[pierre_events$HBV_AT_EVENT=="No"] <- 0
pierre_events$HBV_AT_EVENT[pierre_events$HBV_AT_EVENT=="Yes"] <- 1
pierre_events$HBV_AT_EVENT[pierre_events$HBV_AT_EVENT=="UNK"] <- NA
pierre_events$HBV_AT_EVENT <- as.numeric(pierre_events$HBV_AT_EVENT)

pierre_events$HCV_AT_EVENT <- as.character(pierre_events$HCV_AT_EVENT)

pierre_events$HCV_AT_EVENT[pierre_events$HCV_AT_EVENT=="No"] <- 0
pierre_events$HCV_AT_EVENT[pierre_events$HCV_AT_EVENT=="Yes"] <- 1
pierre_events$HCV_AT_EVENT[pierre_events$HCV_AT_EVENT=="UNK"] <- NA
pierre_events$HCV_AT_EVENT <- as.numeric(pierre_events$HCV_AT_EVENT)

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


pierre_hepb_cast <- dichotomous(pierre_hepb_cast)
colnames(pierre_hepb_cast)[9] <- "hepb_yes"

pierre_hepc_cast <- dichotomous(pierre_hepc_cast)
colnames(pierre_hepc_cast)[9] <- "hepc_yes"

################### CD4 ----------------
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

###Pierre 

pierre_cd4 <- as.data.table(pierre_events[c(1, 8)])
pierre_cd4[, id_cd4 := 1:.N, by = ID_PATIENT]
pierre_cd4_cast <- dcast(pierre_cd4[,list(ID_PATIENT, id_cd4, CD4_AT_EVENT)], ID_PATIENT ~ id_cd4, value.var = 'CD4_AT_EVENT', fill = 0)

pierre_cd4_cast$avg <- round(rowMeans(replace(pierre_cd4_cast[2:7], pierre_cd4_cast[2:7]==0, NA), na.rm=TRUE), 2)

###########################################

pierre_cd4_nadir <- as.data.table(pierre_events[c(1, 9)])
pierre_cd4_nadir[, id_cd4_nadir := 1:.N, by = ID_PATIENT]
pierre_cd4_nadir_cast <- dcast(pierre_cd4_nadir[,list(ID_PATIENT, id_cd4_nadir, CD4_NADIR_EVENT)], ID_PATIENT ~ id_cd4_nadir, value.var = 'CD4_NADIR_EVENT', fill = 0)

pierre_cd4_all <- data.frame(ID_PATIENT=pierre_cd4_nadir_cast$ID_PATIENT, cd4_avg = pierre_cd4_cast$avg, cd4_nadir = pierre_cd4_nadir_cast[,2])

pierre_px_master <- merge(pierre_px, pierre_cd4_all, by = "ID_PATIENT", all.x=TRUE)



###################### LTFU graphs -------------------------------

#LTFU graphs
pierre_alive$LTFU[pierre_alive$LTFU == 0] <- "not_LTFU"
pierre_alive$LTFU[pierre_alive$LTFU == 1] <- "LTFU"

mean(pierre_LTFU$age, na.rm=TRUE)
#histograms

pierre_LTFU_histo <- ggplot(pierre_alive, binwidth = 5, aes(x=age, stat="count")) + 
  geom_histogram(data=subset(pierre_alive,LTFU == "not_LTFU"),fill = "red", 
                 alpha = 0.4, binwidth = 5) +
  geom_histogram(data=subset(pierre_alive,LTFU == "LTFU"),fill = "blue", 
                 alpha = 0.4, binwidth = 5) + 
  scale_x_continuous(breaks=seq(0,90,5)) + 
  ggtitle("St. Pierre") +
  theme(plot.title = element_text(size = 20)) +
  coord_cartesian(xlim=c(0,90))

liege_alive <- subset(liege_master2, STATUS != "Death")

liege_LTFU_histo <- ggplot(liege_alive, binwidth = 5, aes(x=age, stat="count")) + 
  geom_histogram(data=subset(liege_alive,STATUS == "Follow up"),fill = "red", 
                 alpha = 0.4, binwidth = 5) +
  geom_histogram(data=subset(liege_alive,STATUS != "Follow up"),fill = "blue", 
                 alpha = 0.4, binwidth = 5) + 
  scale_x_continuous(breaks=seq(0,90,5)) + 
  ggtitle("Liege") +
  theme(plot.title = element_text(size = 20)) +
  coord_cartesian(xlim=c(0,90))

grid.arrange(pierre_LTFU_histo, liege_LTFU_histo)

####################################

#do the same for Pierre

#create pierre death
pierre_death <- subset(pierre_events, OUTCOME == "12 - DEATH")
names(pierre_death)[6] <- "age"

#bin the age groups

pierre_not_LTFU_all <- subset(pierre_px, LTFU == "0")
pierre_not_LTFU_all <- binning_ages(pierre_not_LTFU_all)

#bin death ages
pierre_death <- binning_ages(pierre_death)


####################### Liege NICMs --------------------------
#look at liege_all CMs
#Create a new column that is binary instead of count for num_CMs

#liege_master2[, CM_yes := num_CMs]
#liege_master2$CM_yes <- ifelse(liege_master2$num_CMs>=1, 1, 0)

#prevalence in pop not LTFU?
liege_not_LTFU2 <- subset(liege_master, STATUS == "Follow up")



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


#################### Pierre NICMs -----------------------


table(pierre_not_LTFU$RENAL_DISEASE)



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
pierre_bar <- ggplot(data=pierre_events, aes(x=CM, y=Freq, fill=CM)) + geom_bar(stat="identity") + ggtitle("Comorbidities (non-AIDS defining) - St. Pierre") + scale_fill_discrete(name="Comorbidity Legend",
                                                                                                                                                                            breaks=c("DIA", "CVD", "NADM", "RD"),
                                                                                                                                                                            labels=c("Diabetes mellitus", "Cardiovascular disease", "Non-AIDS defining malignancies", "Renal disease"))
####

######################## lifestyle risk factors ---------------------------


###########
##treated for hypertension

pierre_hyp <- as.data.table(pierre_events)
pierre_hyp[, id_hyp := 1:.N, by = ID_PATIENT]
pierre_hyp_cast <- dcast(pierre_hyp[,list(ID_PATIENT, id_hyp, TRT_HYPERTENSION_AT_EVENT)], ID_PATIENT ~ id_hyp, value.var = 'TRT_HYPERTENSION_AT_EVENT', fill = 0)

#write a function to create a new column that is conditional on whether the sum column => 1

dichotomous <- function(DT) {
  DT$sum_col <- rowSums(DT[2:7])
  DT$var_yes <- ifelse(DT$sum_col>=1, 1, 0)
  DT
}

pierre_hyp_cast <- dichotomous(pierre_hyp_cast)
colnames(pierre_hyp_cast)[9] <- "hyp_yes"

######do the same for smoking

pierre_smoke <- as.data.table(pierre_events[c(1, 18)])
pierre_smoke[, id_smoke := 1:.N, by = ID_PATIENT]
pierre_smoke_cast <- dcast(pierre_smoke[,list(ID_PATIENT, id_smoke, EVER_SMOKED_AT_EVENT)], ID_PATIENT ~ id_smoke, value.var = 'EVER_SMOKED_AT_EVENT', fill = 0)

pierre_smoke_cast <- dichotomous(pierre_smoke_cast)
colnames(pierre_smoke_cast)[9] <- "smoke_yes"

########################## BMI ----------------------

pierre_bmi <- as.data.table(pierre_events[c(1, 19)])
pierre_bmi[, id_bmi := 1:.N, by = ID_PATIENT]
pierre_bmi_cast <- dcast(pierre_bmi[,list(ID_PATIENT, id_bmi, BMI_AT_EVENT)], ID_PATIENT ~ id_bmi, value.var = 'BMI_AT_EVENT', fill = 0)

#create a new column that is the mean of the bmi measurements, excluding NAs
pierre_bmi_cast$bmi_mean <- round(rowMeans(replace(pierre_bmi_cast[2:7], pierre_bmi_cast[2:7]==0, NA), na.rm=TRUE), 2)

###now find the BMI for the Liege patients
liege_not_LTFU$HEIGHT[liege_not_LTFU$HEIGHT == "999"] <- NA
liege_not_LTFU$WEIGHT[liege_not_LTFU$WEIGHT == "999"] <- NA
liege_not_LTFU$BMI <- (liege_not_LTFU$WEIGHT/liege_not_LTFU$HEIGHT/liege_not_LTFU$HEIGHT)*10000

#create boxplots - BMI by gender

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
pierre_gen_bmi <- data.frame(merge(pierre_bmi_cast, pierre_not_LTFU,
                                   by = "ID_PATIENT"))

pierre_bmi <- ggplot(pierre_gen_bmi, aes(GENDER, bmi_mean, fill=GENDER)) + geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=5, size=6) + 
  scale_fill_manual(values = c("seagreen3", "dodgerblue3")) + 
  scale_y_continuous(breaks=seq(0,50,5)) + 
  ggtitle("BMI Distribution - St. Pierre") + 
  xlab("Gender") +
  ylab("BMI") +
  guides(fill=FALSE)

grid.arrange(liege_bmi, pierre_bmi, ncol=2)
             

######################## Renaming ethnic var -------------------
#rename the country variable in Liege_not_LTFU to match pierre_not_LTFU's region of origin

Liege_not_LTFU <- within(Liege_not_LTFU, COUNTRY[COUNTRY == 'Belgique' | COUNTRY == 'Italie'|
                                                   COUNTRY == 'France'| COUNTRY == 'Espagne'| 
                                                   COUNTRY == 'Grece' | COUNTRY == 'Royaume Uni' |
                                                   COUNTRY == 'Portugal' | COUNTRY == 'Albanie'] <- 'Western Europe')

Liege_not_LTFU <- within(Liege_not_LTFU, COUNTRY[COUNTRY == 'Turquie' | COUNTRY == 'Inde'|
                                                   COUNTRY == 'Armenie'| COUNTRY == 'Indonesie'| 
                                                   COUNTRY == 'Japon' | COUNTRY == 'Thailande' |
                                                   COUNTRY == 'Liban'] <- 'Asia')

Liege_not_LTFU <- within(Liege_not_LTFU, COUNTRY[COUNTRY == 'Slovaquie' | COUNTRY == 'Russie'|
                                                   COUNTRY == 'Roumanie' | COUNTRY == 'Pologne'] <- 'Eastern Europe')

Liege_not_LTFU <- within(Liege_not_LTFU, COUNTRY[COUNTRY == 'Mali' | COUNTRY == 'Rwanda'|
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

Liege_not_LTFU <- within(Liege_not_LTFU, COUNTRY[COUNTRY == 'Cuba' | COUNTRY == 'Haiti'] <- 'Central America')

Liege_not_LTFU <- within(Liege_not_LTFU, COUNTRY[COUNTRY == 'Bresil' | COUNTRY == 'Perou' |
                                                   COUNTRY == 'Equateur'] <- 'South America')

Liege_not_LTFU <- within(Liege_not_LTFU, COUNTRY[COUNTRY == 'Maroc' | COUNTRY == 'Algerie'] <- 'North Africa')


Liege_not_LTFU <- within(Liege_not_LTFU, COUNTRY[COUNTRY == 'Canada'] <- 'North America')

Liege_not_LTFU$COUNTRY<- gsub("Cote d'Ivoire", "Sub-Saharan Africa", Liege_not_LTFU$COUNTRY)
Liege_not_LTFU$COUNTRY<- gsub("Inconnu", "Unknown", Liege_not_LTFU$COUNTRY)

#find out frequency of region of origin
names(Liege_not_LTFU)[5] <- "REGION_OF_ORIGIN"
ethnic_freq <- function(df) {
  site_region <- data.frame(table(df$REGION_OF_ORIGIN))
  site_region$Freq <- (site_region$Freq/sum(site_region$Freq))*100
  site_region
}

pierre_ethnic <- ethnic_freq(pierre_not_LTFU)
liege_ethnic <- ethnic_freq(Liege_not_LTFU)

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
################### started trtmnt at event ------------------

pierre_trt <- as.data.table(pierre_events[c(1, 12)])
pierre_trt[, id_trt := 1:.N, by = ID_PATIENT]
pierre_trt_cast <- dcast(pierre_trt[,list(ID_PATIENT, id_trt, STARTED_TREATMENT_AT_EVENT)], ID_PATIENT ~ id_trt, value.var = 'STARTED_TREATMENT_AT_EVENT', fill = 0)

pierre_trt_cast <- dichotomous(pierre_trt_cast)
colnames(pierre_trt_cast)[9] <- "trt_yes"



#### ADD TO PIERRE_PX_MASTER

pierre_age_death <- as.data.table(pierre_death[c(1, 25)])
pierre_age_death[, id_age_death := 1:.N, by = ID_PATIENT]
pierre_age_death_cast <- dcast(pierre_age_death[,list(ID_PATIENT, id_age_death, age_binned)],
                               ID_PATIENT ~ id_age_death, value.var = 'age_binned', fill = 0)

names(pierre_age_death_cast)[2] <- "age_at_death"

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

pierre_px_master_new <- as.data.table(pierre_px_master_new)
pierre_px_master_new <- pierre_px_master_new[ , paste0(c("1.x", "2.x", "3.x", "4.x", "1.y", "2.y", "3.y", "4.y")) := NULL]

pierre_px_master_new_notLTFU <- subset(pierre_px_master_new, DEATH == 0)
pierre_px_master_new_notLTFU <- subset(pierre_px_master_new_notLTFU, LTFU == 0)


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


######################################################### age distros ----

names(erasme_base)[5] <- "age"
erasme_base <- binning_ages(erasme_base)
erasme_alive <- erasme_base[is.na(erasme_base$DEATH_Date),]


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
erasme_age_df <- data.frame(id = erasme_alive$patient_id, age = erasme_alive$age, age_binned = erasme_alive$age_binned, gender = erasme_alive$GENDER)

#one combined DF
all_age <- rbind(pierre_age_df, liege_age_df, erasme_age_df)

#freq tables for histgrams

pierre_age_freq <- data.frame(table(pierre_age_df$age_binned))
liege_age_freq <- data.frame(table(liege_age_df$age_binned))
erasme_age_freq <- data.frame(table(erasme_age_df$age_binned))

#make freq a percentage of total
freq_perc <- function(df) {
  df$freq_percent <- as.vector(df$Freq/sum(df$Freq))
  return(df)
}

liege_age_freq_perc <- freq_perc(liege_age_freq)
pierre_age_freq_perc <- freq_perc(pierre_age_freq)
erasme_age_freq_perc <- freq_perc(erasme_age_freq)

#all age
all_age_freq <- data.frame(table(all_age$age_binned))
all_age_freq_perc <- freq_perc(all_age_freq)

#histograms
liege_bar_age <- ggplot(data = liege_age_freq_perc, aes(Var1, freq_percent) ) +
  geom_bar(stat="identity", fill = "seagreen4") + xlab("Age groups") + ylab("Frequency (as % of total)") +
  ggtitle("Liège") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

pierre_bar_age <- ggplot(data = pierre_age_freq_perc, aes(Var1, freq_percent) ) +
  geom_bar(stat="identity", fill = "seagreen4") + xlab("Age groups") + ylab("Frequency (as % of total)") +
  ggtitle("St. Pierre") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

erasme_bar_age <- ggplot(data = erasme_age_freq_perc, aes(Var1, freq_percent) ) +
  geom_bar(stat="identity", fill = "seagreen4") + xlab("Age groups") + ylab("Frequency (as % of total)") +
  ggtitle("Erasme") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

all_bar_age <- ggplot(data = all_age_freq_perc, aes(Var1, freq_percent) ) +
  geom_bar(stat="identity", fill = "dodgerblue4") + xlab("Age groups") + ylab("Frequency (as % of total)") +
  ggtitle("Age distribution - All") 


grid.arrange(all_bar_age, arrangeGrob(liege_bar_age, pierre_bar_age, 
                                      erasme_bar_age, ncol=3), heights=c(2.5/4, 1.5/4), ncol=1)


############ age boxplots --------------------
#boxplots, separate
#with diamond at the mean
liege_box_age <- ggplot(liege_age_df, aes(gender, age, fill=gender)) + geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=5, size=6) + 
  scale_fill_manual(values = c("seagreen3", "dodgerblue3", "salmon2")) + 
  scale_y_continuous(breaks=seq(0,95,5)) + 
  ggtitle("Liège")

pierre_box_age <- ggplot(pierre_age_df, aes(gender, age, fill=gender)) + geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=5, size=6) + 
  scale_fill_manual(values = c("seagreen3", "dodgerblue3")) + 
  scale_y_continuous(breaks=seq(0,95,5)) + 
  ggtitle("St. Pierre")

erasme_box_age <- ggplot(erasme_age_df, aes(gender, age, fill=gender)) + geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=5, size=6) + 
  scale_fill_manual(values = c("seagreen3", "dodgerblue3")) + 
  scale_y_continuous(breaks=seq(0,95,5)) + 
  ggtitle("Erasme")

all_box_age <- ggplot(all_age, aes(gender, age, fill=gender)) + geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=5, size=6) + 
  scale_fill_manual(values = c("seagreen3", "dodgerblue3", "salmon2")) + 
  scale_y_continuous(breaks=seq(0,95,5)) + 
  ggtitle("Age & Gender Distribution - All")

#to make a common legend for all
# go here: http://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots

grid.arrange(all_box_age + theme(legend.position="none"), arrangeGrob(liege_box_age + theme(legend.position="none"),
                                      pierre_box_age + theme(legend.position="none"), 
                                      erasme_box_age + theme(legend.position="none"),
                                      ncol=3), heights=c(2.5/4, 1.5/4), ncol=1)
                                                                                                                                         breaks=c("ACS", "ASCI", "DIA", "ESRD", "FRA", "NADM", "STR"),
                                                                                                                                                                            labels=c("Acute coronary syndrome", "Ascites", "Diabetes mellitus", "End stage renal disease", "Bone fracture", "Non-AIDS defining malignancies", "Stroke"))
################## Pierre drug data --------------------------------

#include gender in this
pierre_gender <- merge(pierre_events, pierre_px, by= "ID_PATIENT", all.x=TRUE)

pierre_renal <- subset(pierre_gender, OUTCOME == '1 - RENAL DISEASE')
pierre_cvd <- subset(pierre_gender, OUTCOME == '2 - CVD')


p_renal1 <- ggplot(data=pierre_renal, aes(x=DURATION_PI_MONTHS_AT_EVENT, y=AGE_AT_EVENT, colour= factor(GENDER))) +
  geom_point() + labs(colour = "Gender") + 
  xlab("Duration of PI exposure at event (months)") +
  ylab("Age at event") +
  ggtitle("St. Pierre - Renal")

p_renal2 <- ggplot(data=pierre_renal, aes(x=DURATION_NRTI_MONTHS_AT_EVENT, y=AGE_AT_EVENT, colour= factor(GENDER))) +
  geom_point() + labs(colour = "Gender") + 
  xlab("Duration of NRTI exposure at event (months)") +
  ylab("Age at event") 

p_renal3 <- ggplot(data=pierre_renal, aes(x=DURATION_NNRTI_MONTHS_AT_EVENT, y=AGE_AT_EVENT, colour= factor(GENDER))) +
  geom_point() + labs(colour = "Gender") + 
  xlab("Duration of NRTI exposure at event (months)") +
  ylab("Age at event")

grid.arrange(p_renal1, p_renal2, p_renal3)

p_cvd1 <- ggplot(data=pierre_cvd, aes(x=DURATION_PI_MONTHS_AT_EVENT, y=AGE_AT_EVENT, colour= factor(GENDER))) +
  geom_point() + labs(colour = "Gender") + 
  xlab("Duration of PI exposure at event (months)") +
  ylab("Age at event") +
  ggtitle("St. Pierre - CVD")

p_cvd2 <- ggplot(data=pierre_cvd, aes(x=DURATION_NRTI_MONTHS_AT_EVENT, y=AGE_AT_EVENT, colour= factor(GENDER))) +
  geom_point() + labs(colour = "Gender") + 
  xlab("Duration of NRTI exposure at event (months)") +
  ylab("Age at event")

p_cvd3 <- ggplot(data=pierre_cvd, aes(x=DURATION_NNRTI_MONTHS_AT_EVENT, y=AGE_AT_EVENT, colour= factor(GENDER))) +
  geom_point() + labs(colour = "Gender") + 
  xlab("Duration of NNRTI exposure at event (months)") +
  ylab("Age at event")

grid.arrange(p_cvd1, p_cvd2, p_cvd3)


######### Liege drug data ----------------------
#liege drug data

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





###################### Cohort summaries --------------------
#Use the living patients not LTFU 
#Liege_not_LTFU
#pierre_not_LTFU
#erasme_alive

#check to make sure there is only one row per id
#use duplicated, table, summary commands

erasme_ART_num <- subset(erasme_ART, art_name != is.na(art_name))

erasme_cd4_merge <- merge(erasme_alive, erasme_cd4, by = "patient_id", all.x=TRUE)
summary(erasme_cd4_merge$age)
erasme_ART_merge <- merge(erasme_alive, erasme_ART_num, by = "patient_id", all.x=FALSE)

names(erasme_cd4_merge)[9] <- "CD4_RECENT"

unique(erasme_ART_merge$patient_id)
926/997

erasme_ART_merge <- as.data.table(erasme_ART_merge)
erasme_ART_merge[, id := 1:.N, by = patient_id]
erasme_cast <- dcast(erasme_ART_merge[,list(patient_id, id, art_name)], patient_id ~ id, value.var = 'art_name', fill = 0)

erasme_cast_merge <- merge(erasme_cast, erasme_alive, by = "patient_id", all.x=TRUE)
#make an age combined vector

#bring in Pierre CD4
names(pierre_cd4_cast)[1] <- "patient_id"
pierre_cd4_not_LTFU <- merge(pierre_cd4_cast, pierre_not_LTFU, by="patient_id")
names(pierre_cd4_not_LTFU)[8] <- "CD4_RECENT"
  
names(pierre_trt_cast)[1] <- "patient_id"
pierre_all <- merge(pierre_cd4_not_LTFU, pierre_trt_cast, by= "patient_id")

#library("plyr")
mylist <- list(erasme_cd4_merge, pierre_cd4_not_LTFU, Liege_not_LTFU)
all_dfs <- do.call(rbind.fill, mylist)


###################### Erasme NICMs -------------------------


