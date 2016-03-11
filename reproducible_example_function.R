id <- c(1, 1, 1, 1, 2, 3, 4, 4, 5, 5, 5)
bmi <- c(18, 22, 23, 23, 20, 38, 30, 31, 21, 22, 24)
other_data <- c("north_africa", "north_africa", "north_africa", "north_africa", "western_europe", "south_america", "eastern_europe", "eastern_europe", "ss_africa", "ss_africa", "ss_africa")
other_data2 <- c(0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0)

big_df <- data.frame(id, bmi, other_data, other_data2)


big_dt <- as.data.table(big_df)
big_dt[, id_bmi := 1:.N, by = id]
bmi_dt <- dcast(big_dt[, list(id, id_bmi, bmi)], id ~ id_bmi, value.var = 'bmi', fill = 0)

#first make a data table with just the id and bmi columns
bmi_dt <- as.data.table(big_df[c(1, 2)])


#restructure data so that each ID only has one row
bmi_dt <- bmi_dt[, c(bmi_new = paste(bmi, collapse = "; "), .SD), by = id][!duplicated(bmi_dt$id)]

#split the strings of multiple numbers into 4 new cols
bmi_dt[, c("bmi1", "bmi2", "bmi3", "bmi4") := tstrsplit(as.character(bmi_new), "; ", fixed=TRUE)]

#make columns numeric
bmi_dt <- bmi_dt[, lapply(.SD, as.numeric), by = id]

#function to replace NA with 0 in a data table
func_na <- function(DT) {
  for (i in names(DT))
    DT[is.na(get(i)), i:=0, with=FALSE]
}

func_na(bmi_dt)








big_func <- function(DT, old_col, id_col) {
  DT <- DT[, c(new_col = paste(old_col, collapse = "; "), .SD), by = id_col][!duplicated(id_col)]
  DT
}  

test <- big_func(bmi_dt, bmi, id)


  DT[, c("new_col1", "new_col2", "new_col3", "new_col4") := tstrsplit(as.character(new_col), "; ", fixed=TRUE)]
  DT[, lapply(.SD, as.numeric), by = id]
  func_na <- function(DT) {
    for (i in names(DT))
      DT[is.na(get(i)), i:=0, with=FALSE]
  }
  func_na(DT)
}




#remove unnecessary columns from bmi_dt
bmi_dt[, c(2, 3):=NULL]






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
