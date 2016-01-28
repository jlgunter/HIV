#Goals of this script: 
#1 - Summarize data from St Pierre and Liege

#########################################################################
library(data.table)
library(ltm)
#library(stringr)
#library(aod)
#library(brglm)
library(Hmisc)
##########################################################################
CD4 <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/HIV/Data/Belgium/Université de Liège -Sart Tilman/csv/ULG_CD4_2.csv', header = T, na.strings=c(""))

#find nadir CD4 count for each subject
CD4_nadir <- aggregate(CD4$CD4_VALUE ~ CD4$IDENT_NR, CD4, min)
names(CD4_nadir)[1] <- 'IDENT_NR'
names(CD4_nadir)[2] <- 'CD4_NADIR'

#find mean CD4 count for each subject
CD4_avg <- aggregate(CD4$CD4_VALUE ~ CD4$IDENT_NR, CD4, mean)
names(CD4_avg)[1] <- 'IDENT_NR'
names(CD4_avg)[2] <- 'CD4_AVG'

#find most recent CD4 count
#convert dates from a factor to a date
CD4$CD4_DATE <- as.Date(CD4$CD4_DATE, format = "%m/%d/%Y")
CD4_recent <- aggregate(CD4$CD4_DATE ~ CD4$IDENT_NR, CD4, max)
names(CD4_recent)[1] <- 'IDENT_NR'
names(CD4_recent)[2] <- 'CD4_DATE'
CD4_rec <- merge(CD4, CD4_recent, by = c("CD4_DATE", "IDENT_NR"), all=FALSE)
names(CD4_rec)[3] <- 'CD4_RECENT'
CD4_rec_ord <- CD4_rec[order(CD4_rec[,2]),]

#merge into one dataframe by ident
CD4_tmp <- merge(CD4_nadir, CD4_avg, by = "IDENT_NR", all = FALSE)
CD4_comb <- merge(CD4_tmp, CD4_rec, by = "IDENT_NR", all = FALSE)
names(CD4_comb)[4] <- 'RECENT_DATE'

##########################################################################
#not doing anything with CD4_percent for now because it is unlikely 
#that we will get this from all other centres

CD4_percent <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/HIV/Data/Belgium/Université de Liège -Sart Tilman/csv/ULG_CD4_PERCENT_2.csv', header = T, na.strings=c(""))


##########################################################################

BAS <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/HIV/Data/Belgium/Université de Liège -Sart Tilman/csv/ULG_BAS_2.csv', header = T, na.strings=c(""), stringsAsFactors = FALSE)

#make dates into as.Date and to change the fake dates to NA

names(BAS)[11] <- 'DATE.START.ART'
BAS[, cols <- grep("DATE", names(BAS))] <- lapply(BAS[, cols <- grep("DATE", names(BAS))], as.Date, format = "%m/%d/%Y")

#make the fake placeholder date into NA
BAS[BAS == "1911-11-11"] <- NA

#subtract birth date from today's date, make new column for age
BAS$sys_date <- Sys.Date()
#as.numeric(BAS$sys_date, "%Y")

#from http://r.789695.n4.nabble.com/Calculate-difference-between-dates-in-years-td835196.html
age_years <- function(first, second) 
{ 
  df <- data.frame(first, second) 
  age <- as.numeric(format(df[,2],format="%Y")) - as.numeric(format(df[,1],format="%Y")) 
  first <- as.Date(paste(format(df[,2],format="%Y"),"-",format(df[,1],format="%m-%d"),sep="")) 
  age[which(first > df[,2])] <- age[which(first > df[,2])] - 1 
  age 
}

BAS$age <- age_years(BAS$BIRTH_DATE, BAS$sys_date)
BAS$age <- as.vector(BAS$age)

#do the same for the number of years on ART
BAS$years_on_ART <- age_years(BAS$DATE.START.ART, BAS$sys_date)

#combine CD4_comb with this database
BAS_CD4 <- merge(BAS, CD4_comb, by = "IDENT_NR", all=FALSE)

##########################################################################

CEP <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/HIV/Data/Belgium/Université de Liège -Sart Tilman/csv/ULG_CEP_2.csv', header = T, na.strings=c(""))
CEP$CLIN_EVENT_DATE <- as.Date(CEP$CLIN_EVENT_DATE, format = "%m/%d/%Y")
CEP$CLIN_EVENT_DATE[CEP$CLIN_EVENT_DATE == "1911-11-11"]<- NA

#add it on to the master database
master <- merge(BAS_CD4, CEP, by = "IDENT_NR", all = TRUE)

DT_master <- data.table(master)

#recode GENDER variable
DT_master$gender_con <- ifelse(DT_master$GENDER == 'M', 0, 1)

#contrast code 'smoking' variable
DT_master$smoke_unk  <- -2*(DT_master$SMOKE_YES=='UNK') + 1*(DT_master$SMOKE_YES %in% c('N', 'Y'))
DT_master$smoke_YN <- 0*(DT_master$SMOKE_YES=='UNK') - 1*(DT_master$SMOKE_YES == 'N') + 1*(DT_master$SMOKE_YES == 'Y')

#standardize 'ethnic' var spelling

DT_master$ETHNIC <- gsub("africain", "Africain", DT_master$ETHNIC)
DT_master$ETHNIC <- gsub("caucasien", "Caucasien", DT_master$ETHNIC)

#make a new column that is number of co-morbidities each patient experienced
DT_master[, num_CMs := .N, by=IDENT_NR][is.na(CLIN_EVENT_ID), num_CMs := 0]
DT_master[, num_CMs := as.numeric(num_CMs)]

#stack overflow solution to solve the problem of multiple rows for IDs
DT_master$CLIN_EVENT_ID <- as.character(DT_master$CLIN_EVENT_ID)
DT_master <- DT_master[, c(CLIN_EVENT_ID_NEW = paste(CLIN_EVENT_ID, collapse = "; "), .SD), by = IDENT_NR]
DT_master <- DT_master[!duplicated(DT_master$IDENT_NR)]

#split the strings of multiple numbers into 4 new cols
DT_master[, c("CLIN_EVENT_ID1", "CLIN_EVENT_ID2", "CLIN_EVENT_ID3", "CLIN_EVENT_ID4") := tstrsplit(as.character(CLIN_EVENT_ID_NEW), "; ", fixed=TRUE)]

#eliminate unnecessary cols
DT_master[, CLIN_EVENT_ID_NEW := NULL]


#############################################
#bring in death data
DEATH <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/HIV/Data/Belgium/Université de Liège -Sart Tilman/csv/ULG_DEATH_2.csv', header = T, na.strings=c(""))
DEATH$DEATH_DATE <- as.Date(DEATH$DEATH_DATE, format = "%m/%d/%Y")
DEATH$DEATH_DATE[DEATH$DEATH_DATE == "1911-11-11"]<- NA

#merge death and DT_master
DT_all <- merge(DEATH, DT_master, by = "IDENT_NR", all = TRUE)

#remove rows that are deceased patients
DT_alive <- DT_death[-c(1:26),]

DT_all <- as.data.table(DT_all)
DT_alive <- as.data.table(DT_alive)

#remove death columns
DT_alive[, c("CAUSE_DEATH", "DEATH_DATE") := NULL]

#Create a new column that is binary instead of count for num_CMs

DT_all[, CM_yes := num_CMs]

DT_all$CM_yes <- ifelse(DT_all$num_CMs>=1, 1, 0)



##########################################################################


#remove rows with missing data
#DT_master2 <- na.omit(DT_master2)
#DT_master2$GENDER <- as.factor(DT_master2$GENDER)
#DT_master2 <- as.data.frame(DT_master2)

#######################

CD4_percent <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/HIV/Data/Belgium/Université de Liège -Sart Tilman/csv/ULG_CD4_PERCENT_2.csv', header = T, na.strings=c(""))

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


#from http://r.789695.n4.nabble.com/Calculate-difference-between-dates-in-years-td835196.html
age_years <- function(first, second) 
{ 
  df <- data.frame(first, second) 
  age <- as.numeric(format(df[,2],format="%Y")) - as.numeric(format(df[,1],format="%Y")) 
  first <- as.Date(paste(format(df[,2],format="%Y"),"-",format(df[,1],format="%m-%d"),sep="")) 
  age[which(first > df[,2])] <- age[which(first > df[,2])] - 1 
  age 
}

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
#subset those over 50 years old





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
#look at DT_all CMs

liege_events <- as.data.frame(table(DT_all$CLIN_EVENT_ID1))
liege_events <- liege_events[!(liege_events$Var1 == "NA"),]
names(liege_events)[1] <- "CM"

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
                                                  breaks=c("ACS", "ASCI", "DIA", "ESRD", "FRA", "HEPC", "NADM", "STR"),
                                                  labels=c("Acute coronary syndrome", "Ascites", "Diabetes mellitus", "End stage renal disease", "Bone fracture", "Acute Hepatitis C", "Non-AIDS defining malignancies", "Stroke"))
####

grid.arrange(liege_pie, liege_bar, ncol=2)




####################
#Cms in st pierre data

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
