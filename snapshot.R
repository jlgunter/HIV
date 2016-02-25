#Goals of this script: 
#1 - Summarize data from St Pierre and Liege


###something to consider: When a patient dies, there is also an event for end of follow up.
#need to check if it's identical to death data

#also, a more nuanced look at what's happening at the events will be necessary.
#for now, for example for HTA, I'm just looking at if the patient was ever treated

#########################################################################
library(data.table)
library(ltm)
#library(stringr)
#library(aod)
#library(brglm)
library(Hmisc)
library(reshape2)
##########################################################################
liege_CD4 <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/HIV/Data/Belgium/Université de Liège -Sart Tilman/csv/ULG_CD4_2.csv', header = T, na.strings=c(""))

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

##########################################################################
#not doing anything with CD4_percent for now because it is unlikely 
#that we will get this from all other centres

liege_CD4_percent <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/HIV/Data/Belgium/Université de Liège -Sart Tilman/csv/ULG_CD4_PERCENT_2.csv', header = T, na.strings=c(""))

##########################################################################

liege_BAS <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/HIV/Data/Belgium/Université de Liège -Sart Tilman/csv/ULG_BAS_2.csv', header = T, na.strings=c(""), stringsAsFactors = FALSE)

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
liege_BAS$age <- as.vector(liege_BAS$age)

#do the same for the number of years on ART
liege_BAS$years_on_ART <- age_years(liege_BAS$DATE.START.ART, liege_BAS$sys_date)

#combine CD4_comb with this database
liege_BAS_CD4 <- merge(liege_BAS, CD4_comb, by = "IDENT_NR", all=FALSE)

##########################################################################

liege_CEP <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/HIV/Data/Belgium/Université de Liège -Sart Tilman/csv/ULG_CEP_2.csv', header = T, na.strings=c(""))
liege_CEP$CLIN_EVENT_DATE <- as.Date(liege_CEP$CLIN_EVENT_DATE, format = "%m/%d/%Y")
liege_CEP$CLIN_EVENT_DATE[liege_CEP$CLIN_EVENT_DATE == "1911-11-11"]<- NA

#add it on to the master database
liege_master <- as.data.table(merge(liege_BAS_CD4, liege_CEP, by = "IDENT_NR", all = TRUE))

#recode GENDER variable
liege_master$gender_con <- ifelse(liege_master$GENDER == 'M', 0, 1)

#contrast code 'smoking' variable
liege_master$smoke_unk  <- -2*(liege_master$SMOKE_YES=='UNK') + 1*(liege_master$SMOKE_YES %in% c('N', 'Y'))
liege_master$smoke_YN <- 0*(liege_master$SMOKE_YES=='UNK') - 1*(liege_master$SMOKE_YES == 'N') + 1*(liege_master$SMOKE_YES == 'Y')

#standardize 'ethnic' var spelling

liege_master$ETHNIC <- gsub("africain", "Africain", liege_master$ETHNIC)
liege_master$ETHNIC <- gsub("caucasien", "Caucasien", liege_master$ETHNIC)

#make a new column that is number of co-morbidities each patient experienced
liege_master[, num_CMs := .N, by=IDENT_NR][is.na(CLIN_EVENT_ID), num_CMs := 0]
liege_master[, num_CMs := as.numeric(num_CMs)]

#stack overflow solution to solve the problem of multiple rows for IDs
liege_master$CLIN_EVENT_ID <- as.character(liege_master$CLIN_EVENT_ID)
liege_master <- liege_master[, c(CLIN_EVENT_ID_NEW = paste(CLIN_EVENT_ID, collapse = "; "), .SD), by = IDENT_NR][!duplicated(liege_master$IDENT_NR)]

#split the strings of multiple numbers into 4 new cols
liege_master[, c("CLIN_EVENT_ID1", "CLIN_EVENT_ID2", "CLIN_EVENT_ID3", "CLIN_EVENT_ID4") := tstrsplit(as.character(CLIN_EVENT_ID_NEW), "; ", fixed=TRUE)]

#eliminate unnecessary cols
liege_master[, CLIN_EVENT_ID_NEW := NULL]


#############################################
#bring in death data
liege_death <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/HIV/Data/Belgium/Université de Liège -Sart Tilman/csv/ULG_DEATH_2.csv', header = T, na.strings=c(""))
liege_death$DEATH_DATE <- as.Date(liege_death$DEATH_DATE, format = "%m/%d/%Y")
liege_death$DEATH_DATE[liege_death$DEATH_DATE == "1911-11-11"]<- NA

#merge liege_death and liege_master
liege_all <- as.data.table(merge(liege_death, liege_master, by = "IDENT_NR", all = TRUE))

#remove rows that are deceased patients
liege_alive <- as.data.table(liege_death[-c(1:26),])

#remove death columns
liege_alive[, c("CAUSE_DEATH", "DEATH_DATE") := NULL]

#Create a new column that is binary instead of count for num_CMs

liege_all[, CM_yes := num_CMs]

liege_all$CM_yes <- ifelse(liege_all$num_CMs>=1, 1, 0)


##########################################################################


#remove rows with missing data
#liege_master2 <- na.omit(liege_master2)
#liege_master2$GENDER <- as.factor(liege_master2$GENDER)
#liege_master2 <- as.data.frame(liege_master2)

#######################

liege_CD4_percent <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/HIV/Data/Belgium/Université de Liège -Sart Tilman/csv/ULG_CD4_PERCENT_2.csv', header = T, na.strings=c(""))

################################################

st_pierre_px <- read.csv2('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/HIV/Data/Belgium/St Pierre/PATIENTS.csv',
                          header = TRUE, quote = "\"", dec = ",", na.strings=c(""))

st_pierre_events <- read.csv2('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/HIV/Data/Belgium/St Pierre/EVENTS.csv',
                              header = TRUE, quote = "\"", dec = ",", na.strings=c(""))


#age
st_pierre_px$DOB <- as.Date(st_pierre_px$DOB, format = "%d/%m/%Y")

st_pierre_px$DOB[st_pierre_px$DOB == "1911-11-11"] <- NA

today <- Sys.Date()
st_pierre_px$sys_date <- as.Date(format(today, format = "%Y-%m-%d"))

#use previously defined age_years function
st_pierre_px$age <- age_years(st_pierre_px$DOB, st_pierre_px$sys_date)

########################################
#LTFU graphs

##### St Pierre

#first subset living patients
st_pierre_px$DEATH <- as.numeric(st_pierre_px$DEATH)

st_pierre_alive <- subset(st_pierre_px, DEATH == "0")

#subset to take a closer look at LTFU
pierre_LTFU <- subset(st_pierre_alive, LTFU == "1")
pierre_not_LTFU <- subset(st_pierre_alive, LTFU == "0")


#plot the two age distributions 
pierre_LTFU_hist <- hist(pierre_LTFU$age)
pierre_not_LTFU_hist <- hist(pierre_not_LTFU$age)
plot(pierre_LTFU_hist, col=rgb(0,0,139, max = 255, alpha = 255), ylim = c(0, 600), xlab = "Age", ylab = "Patients", main = "Age distribution of patients LTFU - St. Pierre")
plot(pierre_not_LTFU_hist, col=rgb(0,191,255, max = 255, alpha = 125), ylim = c(0, 600), add=T)
legend("topright", c("LTFU", "not_LTFU"), col = c(rgb(0,0,139, max = 255, alpha = 220), rgb(0,191,255, max = 255, alpha = 125)), lwd = 10)
minor.tick(nx = 10)

#### Liege

#subset to take a closer look at LTFU
Liege_LTFU <- subset(DT_alive, STATUS != "Follow up")
Liege_not_LTFU <- subset(DT_alive, STATUS == "Follow up")
#Liege_TRANSFERRED <- subset(DT_alive, STATUS == "Transferred")

Liege_LTFU_hist <- hist(Liege_LTFU$age)
Liege_not_LTFU_hist <- hist(Liege_not_LTFU$age)
plot(Liege_LTFU_hist, col=rgb(0,0,139, max = 255, alpha = 255), ylim = c(0, 200), xlab = "Age", ylab = "Patients", main = "Age distribution of patients LTFU - Liège")
plot(Liege_not_LTFU_hist, col=rgb(0,191,255, max = 255, alpha = 125), ylim = c(0, 200), add=T)

legend("topright", c("LTFU/Transf.", "not_LTFU"), col = c(rgb(0,0,139, max = 255, alpha = 230), rgb(0,191,255, max = 255, alpha = 125)), lwd = 10)
minor.tick(nx = 10)

#######

###clean up gender var
Liege_not_LTFU <- as.data.frame(Liege_not_LTFU)
names(Liege_not_LTFU)[3] <- 'gender'
Liege_not_LTFU$gender <- gsub("F", "Female", Liege_not_LTFU$gender)
Liege_not_LTFU$gender <- gsub("M", "Male", Liege_not_LTFU$gender)
Liege_not_LTFU$gender <- gsub("O", "Other", Liege_not_LTFU$gender)

names(pierre_not_LTFU)[3] <- 'gender'

#boxplot(Liege_not_LTFU$age, horizontal=TRUE, main="Age - Liege")
#with diamond at the mean
liege_plot <- ggplot(Liege_not_LTFU, aes(gender, age, fill=gender)) + geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=5, size=6) + scale_fill_manual(values = c("seagreen3", "dodgerblue3", "salmon2")) + scale_y_continuous(breaks=seq(0,85,5)) + ggtitle("Age & Gender Distribution - Liège")

pierre_plot <- ggplot(pierre_not_LTFU, aes(gender, age, fill=gender)) + geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=5, size=6) + scale_fill_manual(values = c("seagreen3", "dodgerblue3")) + scale_y_continuous(breaks=seq(0,95,5)) + ggtitle("Age & Gender Distribution - St. Pierre")

#require(gridExtra)
grid.arrange(liege_plot, pierre_plot, nrow=2)

#create liege death
liege_death <- subset(liege_all, STATUS == "Death")
liege_death$death_age <- age_years(liege_death$BIRTH_DATE, liege_death$DEATH_DATE)

#bin the age groups by 5 years
breaks <- seq(0,85, by=5)
liege_all$liege_binned <- cut(liege_all$age, breaks, include.lowest = T)
levels(liege_all$liege_binned) <- c("0-5","6-10","11-15",
                                 "16-20","21-25", "26-30","31-35","36-40",
                                 "41-45","46-50","51-55", "56-60", "61-65",
                                 "66-70", "71-75", "76-80", "81-85")

#bin death ages
liege_death$death_binned <- cut(liege_death$age, breaks, include.lowest = T)
levels(liege_death$death_binned) <- c("0-5","6-10","11-15",
                                      "16-20","21-25", "26-30","31-35","36-40",
                                      "41-45","46-50","51-55", "56-60", "61-65",
                                      "66-70", "71-75", "76-80", "81-85")


#do the same for Pierre

#create pierre death
pierre_death <- subset(st_pierre_events, OUTCOME == "12 - DEATH")

#bin the age groups by 5 years
pierre_not_LTFU_all <- subset(st_pierre_px, LTFU == "0")
pierre_not_LTFU_all$pierre_binned <- cut(pierre_not_LTFU_all$age, breaks, include.lowest = T)
levels(pierre_not_LTFU_all$pierre_binned) <- c("0-5","6-10","11-15",
                                 "16-20","21-25", "26-30","31-35","36-40",
                                 "41-45","46-50","51-55", "56-60", "61-65",
                                 "66-70", "71-75", "76-80", "81-85")

#bin death ages
pierre_death$death_binned <- cut(pierre_death$AGE_AT_EVENT, breaks, include.lowest = T)
levels(pierre_death$death_binned) <- c("0-5","6-10","11-15",
                                      "16-20","21-25", "26-30","31-35","36-40",
                                      "41-45","46-50","51-55", "56-60", "61-65",
                                      "66-70", "71-75", "76-80", "81-85")

#now skip down to bottom for table mortality calculations

###consider average age vs avg age LTFU
pierre_avg_LTFU <- pierre_LTFU$age
pierre_avg_LTFU <- na.omit(pierre_avg_LTFU)
mean(pierre_avg_LTFU)

pierre_avg_not_LTFU <- pierre_not_LTFU$age
pierre_avg_not_LTFU <- na.omit(pierre_avg_not_LTFU)
mean(pierre_avg_not_LTFU)


liege_avg_LTFU <- Liege_LTFU$age
liege_avg_LTFU <- na.omit(liege_avg_LTFU)
mean(liege_avg_LTFU)

liege_avg_not_LTFU <- Liege_not_LTFU$age
liege_avg_not_LTFU <- na.omit(liege_avg_not_LTFU)
mean(liege_avg_not_LTFU)


####looking at the average number of years on ART
Over50_Liege <- subset(DT_alive, age >= 50)
avg_ART <- Over50_Liege$years_on_ART
avg_ART <- na.omit(avg_ART)
mean(avg_ART)

#######################
#look at liege_all CMs

liege_events <- as.data.frame(table(liege_all$CLIN_EVENT_ID1))
liege_events <- liege_events[!(liege_events$Var1 == "NA"),]
names(liege_events)[1] <- "CM"

#take out Hep C for now
liege_events <- liege_events[-c(6),]

#add in the second, third, and fourth CMs
liege_events$Freq[liege_events$Freq == 44] <- 54
liege_events$Freq[liege_events$Freq == 14] <- 20
liege_events$Freq[liege_events$Freq == 63] <- 72
liege_events$Freq[liege_events$Freq == 1] <- 3
liege_events$Freq[liege_events$Freq == 18] <- 23

bp <- ggplot(data=liege_events, aes(x=1, y=Freq, fill = CM)) + geom_bar(stat="identity")
p <- bp + ggtitle("Comorbidities (non-AIDS defining) - Liège") + coord_polar(theta='y')
p <- p + geom_bar(stat="identity", color="black") + guides(fill=guide_legend(override.aes=list(colour=NA)))
p <- p + theme(axis.ticks=element_blank(), axis.title=element_blank(), axis.text.y=element_blank()) 

y.breaks <- cumsum(liege_events$Freq) - liege_events$Freq/2

liege_pie <- p + theme(axis.text.x=element_text(color='black')) +
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
####

grid.arrange(liege_pie, liege_bar, ncol=2)


####################
#CMs in st pierre data

st_pierre_px$new_counts <- st_pierre_px$RENAL_DISEASE + st_pierre_px$CVD + 
  st_pierre_px$DIABETE + st_pierre_px$HEPATIC_DECOMP + st_pierre_px$LIVER_CANCER + 
  st_pierre_px$HODG_LYMP + st_pierre_px$LUNG_CANCER + st_pierre_px$ANAL_CANCER

st_pierre_px$CM_yes <- ifelse(st_pierre_px$new_counts>=1, 1, 0)

#create a new column that is Non-AIDS defining malignancies to match Liege data
st_pierre_px$NADM <- st_pierre_px$LUNG_CANCER + st_pierre_px$ANAL_CANCER + st_pierre_px$HODG_LYMP + st_pierre_px$LIVER_CANCER

pierre_events <- as.data.frame(c("RD", "DIA", "CVD", "NADM"))
pierre_events$Freq <- (c(402, 160, 79, 43))
names(pierre_events)[1] <- "CM"

bp <- ggplot(data=pierre_events, aes(x=1, y=Freq, fill = CM)) + geom_bar(stat="identity")
p <- bp + ggtitle("Comorbidities (non-AIDS defining) - St. Pierre") + coord_polar(theta='y')
p <- p + geom_bar(stat="identity", color="black") + guides(fill=guide_legend(override.aes=list(colour=NA)))
p <- p + theme(axis.ticks=element_blank(), axis.title=element_blank(), axis.text.y=element_blank()) 

y.breaks <- cumsum(pierre_events$Freq) - pierre_events$Freq/2

pierre_pie <- p + theme(axis.text.x=element_text(color='black')) +
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
#mortality rates

pierre_not_LTFU_dead <- subset(st_pierre_px, LTFU == "0")

liege_not_LTFU_dead <- subset(liege_all, STATUS != "Contact lost")
liege_dead_alive <- subset(liege_not_LTFU_dead, STATUS != "Transferred")
age_tab_liege <- table(liege_dead_alive$liege_binned)
age_tab_liege_dead <- table(liege_death$liege_binned)
death_rates_liege <- age_tab_liege_dead/age_tab_liege

#multiply by 1000 for mortality rate
mortality_tab_liege <- as.data.frame(death_rates_liege*1000)

#do the same for Pierre
age_tab_pierre <- table(pierre_not_LTFU_all$pierre_binned)
age_tab_pierre_dead <- table(pierre_death$death_binned)
death_rates_pierre <- age_tab_pierre_dead/age_tab_pierre

#multiply by 1000 for mortality rate
mortality_tab_pierre <- as.data.frame(death_rates_pierre*1000)

mortality_tabs <- merge(mortality_tab_liege, mortality_tab_pierre, by = "Var1", all = TRUE)
#colnames(mortality_tabs)[1:3] <- c("Age", "Liege", "St. Pierre")
colnames(mortality_tabs)[2:3] <- c("Liege", "St. Pierre")

mortality_tabs_long <- melt(mortality_tabs, id="Age")  # convert to long format
names(mortality_tabs_long)[2] <- "Cohort"

ggplot(data=mortality_tabs_long,
       aes(x=Age, y=value, group = Cohort, colour=Cohort)) +
  geom_line(size=1.5) + ylab("Mortality rate (per 1,000)") + ggtitle("Age-specific mortality rates per 1,000 (crude)")

#######################################################
#lifestyle risk factors

###########
##treated for hypertension

pierre_hyp <- as.data.table(st_pierre_events)
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

pierre_smoke <- as.data.table(st_pierre_events[c(1, 18)])
pierre_smoke[, id_smoke := 1:.N, by = ID_PATIENT]
pierre_smoke_cast <- dcast(pierre_smoke[,list(ID_PATIENT, id_smoke, EVER_SMOKED_AT_EVENT)], ID_PATIENT ~ id_smoke, value.var = 'EVER_SMOKED_AT_EVENT', fill = 0)

pierre_smoke_cast <- dichotomous(pierre_smoke_cast)
colnames(pierre_smoke_cast)[9] <- "smoke_yes"

######BMI

pierre_bmi <- as.data.table(st_pierre_events[c(1, 19)])
pierre_bmi[, id_bmi := 1:.N, by = ID_PATIENT]
pierre_bmi_cast <- dcast(pierre_bmi[,list(ID_PATIENT, id_bmi, BMI_AT_EVENT)], ID_PATIENT ~ id_bmi, value.var = 'BMI_AT_EVENT', fill = 0)

#create a new column that is the mean of the bmi measurements, excluding NAs
pierre_bmi_cast$bmi_mean <- round(rowMeans(replace(pierre_bmi_cast[2:7], pierre_bmi_cast[2:7]==0, NA), na.rm=TRUE), 2)

###now find the BMI for the Liege patients
liege_all$HEIGHT[liege_all$HEIGHT == "999"] <- NA
liege_all$WEIGHT[liege_all$WEIGHT == "999"] <- NA
liege_all$BMI <- (liege_all$WEIGHT/liege_all$HEIGHT/liege_all$HEIGHT)*10000

#create boxplots - BMI by gender

###clean up gender var

names(liege_all)[5] <- 'gender'
liege_all$gender <- gsub("F", "Female", liege_all$gender)
liege_all$gender <- gsub("M", "Male", liege_all$gender)
liege_all$gender <- gsub("O", "Other", liege_all$gender)

#names(pierre_not_LTFU)[3] <- 'gender'

#with diamond at the mean
liege_bmi <- ggplot(liege_all, aes(gender, BMI, fill=gender)) + geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=5, size=6) + 
  scale_fill_manual(values = c("seagreen3", "dodgerblue3", "salmon2")) + 
  scale_y_continuous(breaks=seq(0,50,5)) + ggtitle("BMI Distribution - Liège") + geom_hline(yintercept = 30)

#####pierre
#gender and bmi are in different databases
pierre_gen_bmi <- data.frame(merge(pierre_bmi_new, st_pierre_px, by = "ID_PATIENT", all = TRUE))
names(pierre_gen_bmi)[32] <- 'gender'
names(pierre_gen_bmi)[30] <- 'BMI'

pierre_bmi <- ggplot(pierre_gen_bmi, aes(gender, BMI, fill=gender)) + geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=5, size=6) + 
  scale_fill_manual(values = c("seagreen3", "dodgerblue3")) + 
  scale_y_continuous(breaks=seq(0,50,5)) + ggtitle("BMI Distribution - St. Pierre") + geom_hline(yintercept = 30)




######
#average age
mean(Liege_not_LTFU$age)
mean(Liege_not_LTFU$years_on_ART, na.rm = TRUE)

mean(pierre_not_LTFU$age)
mean(pierre_not_LTFU$years_on_ART)

#rename the country variable in Liege_not_LTFU to match pierre_not_LTFU's region of origin


sorted <- Liege_not_LTFU[order(Liege_not_LTFU$ETHNIC),]
sorted2 <- pierre_not_LTFU[order(pierre_not_LTFU$REGION_OF_ORIGIN),]


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

###### graph graph it out
liege_country_tab <- as.data.frame(table(Liege_not_LTFU$COUNTRY))
liege_country_plot <- ggplot(data=liege_country_tab, aes(x=Var1, y=Freq, fill=Var1)) + geom_bar(stat="identity") 

LP <- ggplot(liege_country_tab, aes(x=1, y=Freq, fill=Var1)) +
  geom_bar(stat="identity")

LP <- LP + coord_polar(theta='y')
LP <- LP + geom_bar(stat="identity", color="black") + guides(fill=guide_legend(override.aes=list(colour=NA)))
LP <- LP + theme(axis.ticks=element_blank(), axis.title=element_blank(), axis.text.y=element_blank()) 

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
################################################
###CD4 counts

pierre_cd4 <- as.data.table(st_pierre_events[c(1, 8)])
pierre_cd4[, id_cd4 := 1:.N, by = ID_PATIENT]
pierre_cd4_cast <- dcast(pierre_cd4[,list(ID_PATIENT, id_cd4, CD4_AT_EVENT)], ID_PATIENT ~ id_cd4, value.var = 'CD4_AT_EVENT', fill = 0)

pierre_cd4_cast$avg <- round(rowMeans(replace(pierre_cd4_cast[2:7], pierre_cd4_cast[2:7]==0, NA), na.rm=TRUE), 2)

###########################################

pierre_cd4_nadir <- as.data.table(st_pierre_events[c(1, 9)])
pierre_cd4_nadir[, id_cd4_nadir := 1:.N, by = ID_PATIENT]
pierre_cd4_nadir_cast <- dcast(pierre_cd4_nadir[,list(ID_PATIENT, id_cd4_nadir, CD4_NADIR_EVENT)], ID_PATIENT ~ id_cd4_nadir, value.var = 'CD4_NADIR_EVENT', fill = 0)

pierre_cd4_all <- data.frame(ID_PATIENT=pierre_cd4_nadir_cast$ID_PATIENT, cd4_avg = pierre_cd4_cast$avg, cd4_nadir = pierre_cd4_nadir_cast[,2])


#take mean of CD4 measures?

#######################################################################
#avg CD4 count by age bin?
#pierre_not_LTFU_all
#liege_dead_alive
#^ these have the age bins

#merge age bin DFs with DFs used for CD4 counts just before this

#liege_all is good to go

#pierre is not, redo this part
pierre_binned_cd4 <- as.data.frame(st_pierre_px)
pierre_binned_cd4$pierre_binned <- cut(st_pierre_px$age, breaks, include.lowest = T)
levels(pierre_binned_cd4$pierre_binned) <- c("0-5","6-10","11-15",
                                               "16-20","21-25", "26-30","31-35","36-40",
                                               "41-45","46-50","51-55", "56-60", "61-65",
                                               "66-70", "71-75", "76-80", "81-85")

pierre_binned_cd4_merged <- merge(pierre_binned_cd4, cd4s_pierre, by = "ID_PATIENT", all = TRUE)

#now, more graphs!



#this is just repeat from earlier to harvest code
mortality_tabs <- merge(mortality_tab_liege, mortality_tab_pierre, by = "Var1", all = TRUE)
colnames(mortality_tabs)[1:3] <- c("Age", "Liege", "St. Pierre")

mortality_tabs_long <- melt(mortality_tabs, id="Age")  # convert to long format
names(mortality_tabs_long)[2] <- "Cohort"

ggplot(data=mortality_tabs_long,
       aes(x=Age, y=value, group = Cohort, colour=Cohort)) +
  geom_line(size=1.5) + ylab("Mortality rate (per 1,000)") + ggtitle("Age-specific mortality rates per 1,000 (crude)")


#########started treatment at event 

pierre_trt <- as.data.table(st_pierre_events[c(1, 12)])
pierre_trt[, id_trt := 1:.N, by = ID_PATIENT]
pierre_trt_cast <- dcast(pierre_trt[,list(ID_PATIENT, id_trt, STARTED_TREATMENT_AT_EVENT)], ID_PATIENT ~ id_trt, value.var = 'STARTED_TREATMENT_AT_EVENT', fill = 0)

pierre_trt_cast <- dichotomous(pierre_trt_cast)
colnames(pierre_trt_cast)[9] <- "trt_yes"

######find out how many missing data points there are for years on ART

DT_ART <- subset(liege_all, select = c(1, 22))
DT_ART2 <- na.omit(DT_ART)
nrow(DT_ART2)
1103/1241
DT_ART2$yes <- ifelse(DT_ART2$years_on_ART >= 1, 1, 0)
1241-1103








############## old code

#restructure the data so that there is only one row per ID
#pierre_events_smoke <- pierre_events_smoke[, c(smoke_new = paste(EVER_SMOKED_AT_EVENT, collapse = "; "), .SD), by = ID_PATIENT]
#pierre_events_smoke <- pierre_events_smoke[!duplicated(pierre_events_smoke$ID_PATIENT)]

#split the strings of multiple numbers into 4 new cols
#pierre_events_smoke[, c("smoke1", "smoke2", "smoke3", "smoke4") := tstrsplit(as.character(smoke_new), "; ", fixed=TRUE)]

#make columns numeric
#pierre_events_smoke <- pierre_events_smoke[, lapply(.SD, as.numeric), by = ID_PATIENT]

#call the function to replace NA with 0
#func_na(pierre_events_smoke)