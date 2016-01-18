#Goals of this script: 
#1 - Merge data provided by Karine Fombellida into one large database
#2 - Work out kinks of merging such as multiple rows for one individual
#3 - Test the feasibility of regression models with these data

#VIF for multicolinearity?

#########################################################################
library(data.table)
library(ltm)
#library(stringr)
library("aod")
library("logistf")
library("brglm")
##########################################################################
CD4 <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/HIV/Data/Belgium/Université de Liège -Sart Tilman/csv/ULG_CD4.csv', header = T, na.strings=c(""))

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

CD4_percent <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/HIV/Data/Belgium/Université de Liège -Sart Tilman/csv/ULG_CD4_PERCENT.csv', header = T, na.strings=c(""))


##########################################################################

BAS <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/HIV/Data/Belgium/Université de Liège -Sart Tilman/csv/ULG_BAS.csv', header = T, na.strings=c(""), stringsAsFactors = FALSE)

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

CEP <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/HIV/Data/Belgium/Université de Liège -Sart Tilman/csv/ULG_CEP.csv', header = T, na.strings=c(""))
CEP$CLIN_EVENT_DATE <- as.Date(CEP$CLIN_EVENT_DATE, format = "%m/%d/%Y")
CEP$CLIN_EVENT_DATE[CEP$CLIN_EVENT_DATE == "1911-11-11"]<- NA

#add it on to the master database
master <- merge(BAS_CD4, CEP, by = "IDENT_NR", all = TRUE)

DT_master <- data.table(master)

#contrast code GENDER variable
DT_master$gender_con <- ifelse(DT_master$GENDER == 'M', -.5, .5)

#contrast code 'smoking' variable
DT_master$smoke_unk  <- -2*(DT_master$SMOKE_YES=='UNK') + 1*(DT_master$SMOKE_YES %in% c('N', 'Y'))
DT_master$smoke_YN <- 0*(DT_master$SMOKE_YES=='UNK') - 1*(DT_master$SMOKE_YES == 'N') + 1*(DT_master$SMOKE_YES == 'Y')

#centering age variable to age 40
summary(DT_master$age) #range is from 17-87
DT_master$cenage <- DT_master$age - 40

#create contrast codes for 'ethnic' variable
#first ensure that spellings are standardized
#order of contrast codes may need to be changed *****************************

DT_master$ETHNIC <- gsub("africain", "Africain", DT_master$ETHNIC)
DT_master$ETHNIC <- gsub("caucasien", "Caucasien", DT_master$ETHNIC)

DT_master$eth_unk <- -7*(DT_master$ETHNIC=='Inconnu') + 1*(DT_master$ETHNIC %in% c('Africain', 'Asiatique', 'Blanc/Noir', 'Caucasien', 'Hispanique', 'Maghrebin', 'Autre'))
DT_master$eth_african <- -6*(DT_master$ETHNIC=='Africain') + 1*(DT_master$ETHNIC %in% c('Asiatique', 'Blanc/Noir', 'Caucasien', 'Hispanique', 'Maghrebin', 'Autre'))
DT_master$eth_asian <- -5*(DT_master$ETHNIC=='Asiatique') + 1*(DT_master$ETHNIC %in% c( 'Blanc/Noir', 'Caucasien', 'Hispanique', 'Maghrebin', 'Autre'))
DT_master$eth_whiteblack <- -4*(DT_master$ETHNIC=='Blanc/Noir') + 1*(DT_master$ETHNIC %in% c('Caucasien', 'Hispanique', 'Maghrebin', 'Autre'))
DT_master$eth_caucasian <- -3*(DT_master$ETHNIC=='Caucasien') + 1*(DT_master$ETHNIC %in% c('Hispanique', 'Maghrebin', 'Autre'))
DT_master$eth_hispanic <- -2*(DT_master$ETHNIC=='Hispanique') + 1*(DT_master$ETHNIC %in% c('Maghrebin', 'Autre'))
DT_master$eth_northafrican <- -1*(DT_master$ETHNIC=='Maghrebin') + 1*(DT_master$ETHNIC %in% c('Autre'))


#Cronbach's alpha for CD4 counts
CD4_df <- data.frame(DT_master$CD4_NADIR, DT_master$CD4_RECENT)
cronbach.alpha(CD4_df, standardized = FALSE, CI = FALSE, B = 500)

#make a new column that is number of co-morbidities each patient experienced
DT_master[, num_CMs := .N, by=IDENT_NR][is.na(CLIN_EVENT_ID), num_CMs := 0]
DT_master[, num_CMs := as.numeric(num_CMs)]

#DT_master$CLIN_EVENT_DATE = as.character(DT_master$CLIN_EVENT_DATE)
#DT_master <- aggregate(list(CLIN_EVENT_DATE = I(DT_master$CLIN_EVENT_DATE)), by = list(IDENT_NR = DT_master$IDENT_NR), c)

#stack overflow solution to solve the problem of multiple rows for IDs
DT_master$CLIN_EVENT_ID <- as.character(DT_master$CLIN_EVENT_ID)
DT_master <- DT_master[, c(CLIN_EVENT_ID_NEW = paste(CLIN_EVENT_ID, collapse = "; "), .SD), by = IDENT_NR]
DT_master <- DT_master[!duplicated(DT_master$IDENT_NR)]

#split the strings of multiple numbers into 4 new cols
DT_master[, c("CLIN_EVENT_ID1", "CLIN_EVENT_ID2", "CLIN_EVENT_ID3", "CLIN_EVENT_ID4") := tstrsplit(as.character(CLIN_EVENT_ID_NEW), "; ", fixed=TRUE)]

#eliminate unnecessary cols
DT_master[, CLIN_EVENT_ID_NEW := NULL]

#convert the 4 new cols back to numeric
#DT_master[, CLIN_EVENT_ID1 := as.numeric(CLIN_EVENT_ID1)]


#############################################
#bring in death data
DEATH <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/HIV/Data/Belgium/Université de Liège -Sart Tilman/csv/ULG_DEATH.csv', header = T, na.strings=c(""))
DEATH$DEATH_DATE <- as.Date(DEATH$DEATH_DATE, format = "%m/%d/%Y")
DEATH$DEATH_DATE[DEATH$DEATH_DATE == "1911-11-11"]<- NA

#merge death and DT_master
DT_death <- merge(DEATH, DT_master, by = "IDENT_NR", all = TRUE)

#remove rows that are deceased patients
DT_alive <- DT_death[-c(1:24),]

DT_alive <- as.data.table(DT_alive)
#remove death columns
DT_alive[, c("CAUSE_DEATH", "DEATH_DATE") := NULL]

#Create a new column that is binary instead of count for num_CMs for logistic reg models

DT_alive[, CM_yes := num_CMs]

DT_alive$CM_yes <- ifelse(DT_alive$num_CMs>=1, 1, 0)

#remove rows that are transferred patients
DT_alive <- subset(DT_alive, STATUS != "Transfere")

#Boxplot(~cenage, data=DT_alive, id.n=Inf)

##########################################################################

#Firth's penalized likelihood due to rare outcome

#center the age and years on ART variables?

#make new dataframe with just the vars for regression
DT_master2 <- subset(DT_alive, select= c(1,3,4,14,19,21,24,33))

#remove rows with missing data
DT_master2 <- na.omit(DT_master2)
DT_master2$GENDER <- as.factor(DT_master2$GENDER)
DT_master2 <- as.data.frame(DT_master2)


#there are too many missing values for years on ART, remove from analysis 
library(Amelia)
missmap(DT_master2, main = "Missing values vs observed")

#make response variable a factor?
DT_master2$CM_yes <- as.factor(DT_master2$CM_yes)


options(scipen=999)
logis1 <-logistf(CM_yes ~ GENDER + ETHNIC + BP_SYS + age + CD4_NADIR, CD4_RECENT, data = DT_master2)
glm <-glm(CM_yes ~ GENDER + ETHNIC + BP_SYS + age + CD4_NADIR + CD4_RECENT, family = binomial("logit"), data = DT_master2)
drop1(logis1, test= "PLR")


allClass <- function(x) {unlist(lapply(unclass(x),class))}
allClass(DT_master2)

                        

#######################

CD4_percent <- read.csv('/Users/cda/Dropbox (CfDA)/Titan - CDA Only/HIV/Data/Belgium/Université de Liège -Sart Tilman/csv/ULG_CD4_PERCENT.csv', header = T, na.strings=c(""))



levels(DT_master2$ETHNIC)
data <- data.frame(X = sample(c(1, 2, 2, 3, 4, 5, 5)), Y = seq(2005-03-15, 2013-11-15, length.out = X))






################################################
library(Hmisc)

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


#subset to take a closer look at LTFU
LTFU <- subset(st_pierre_px, LTFU == "1")
not_LTFU <- subset(st_pierre_px, LTFU == "0")


#plot the two age distributions 
LTFU_hist <- hist(LTFU$age)
not_LTFU_hist <- hist(not_LTFU$age)
plot(LTFU_hist, col=rgb(0,0,1,1/4), ylim = c(0, 600), xlab = "Age", main = "Age distributions in LTFU and not LTFU patients")
plot(not_LTFU_hist, col=rgb(1,0,0,1/4), ylim = c(0, 600), add=T)
legend("topright", c("LTFU", "not_LTFU"), col = c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)), lwd = 10)
minor.tick(nx = 10)

