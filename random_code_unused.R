# drug_plots <- function(df) {
#   ggplot(df, aes(x=drug_class, y=art_name, fill=art_name)) + 
#     stat_summary(fun.y="mean", geom="bar", position="dodge") + 
#     scale_fill_brewer(palette="Set1", name="ART name") + theme(axis.title.x = element_blank())
# }

grid.arrange(drug_plots(erasme_art_comb), drug_plots(erasme_art_pi), drug_plots(erasme_art_nnrti), 
             drug_plots(erasme_art_nrti))



# test1 <- ggplot(data = erasme_art_new, aes(x=drug_class, y=freq, color=art_name) ) +
#   geom_bar(stat="identity")
# 
# test2 <- ggplot(data = erasme_art_new, aes(x=drug_class, y=freq, fill=art_name) ) +
#   geom_bar(stat="identity") + scale_fill_manual(values=getPalette(colorCount))

#try doing separate graphs that share a y axis, change the color scale for each
# erasme_art_pi <- subset(erasme_art_new, drug_class == "PI")
# erasme_art_nnrti <- subset(erasme_art_new, drug_class == "NNRTI")
# erasme_art_nrti <- subset(erasme_art_new, drug_class == "NRTI")
# erasme_art_comb <- subset(erasme_art_new, drug_class == "comb")
# erasme_art_int_inhib <- subset(erasme_ART2015, drug_class == "Integrase_inhib")
# 













#try a ggplot with the data in long format

#ggplot(data = erasme_ART2015, aes(x=patient_id, y=art_duration, color= drug_class)) + 
#geom_point(shape=1)

#ggplot(data = erasme_ART2015, aes(x=patient_id, y=art_duration, fill= drug_class)) + 
#geom_bar(stat="identity")

#ggplot(data = erasme_ART2015, aes(x=art_name, y=art_duration, fill= drug_class)) + 
#geom_bar(stat="identity") 


#expand color palette
erasme_ART2015$art_name <- factor(erasme_ART2015$art_name)

colorCount = length(unique(erasme_ART2015$art_name))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

ggplot(erasme_ART2015, aes(x=drug_class, y=art_duration, fill=art_name)) + 
  stat_summary(fun.y="mean", geom="bar", position="dodge") + coord_flip() + 
  scale_fill_manual(values=getPalette(colorCount))

#try breaking them up


###########
#revamp so the y axis is freq of px
#do something else with art_duration

ggplot(erasme_ART2015, aes(x=drug_class, y=art_duration, fill=art_name)) + 
  stat_summary(fun.y="mean", geom="bar", position="dodge") + 
  scale_fill_manual(values=getPalette(colorCount)) 

#avg duration by drug class
#erasme_art_wide <- dcast(erasme_ART2015, patient_id ~ drug_class, 
#value.var = "art_duration", fun.aggregate = mean)

#erasme_art_long <- melt(erasme_art_wide,
#  id.vars= "patient_id",
#  measure.vars=c("comb",  "Entry_inhib", "Integrase_inhib",
#               "NNRTI", "NRTI",	"Other_test_drug", "PI"),
#  variable.name="drug_class",
#  value.name="art_duration"
#)

#see what happens if I remove art drugs which have fewer than 6 patients
#erasme_ART2015 <- erasme_ART2015[erasme_ART2015$art_name %in% names(which(table(erasme_ART2015$art_name) > 5)),]



#use tidyr to convert the data from long to wide format
#erasme_ART2015_wide <-spread(erasme_ART2015, art_name, art_duration)
#erasme_ART2015_wide <- dcast(erasme_ART2015, patient_id ~ art_name, value.var = "art_duration", sum)


######## CONTRAST CODING ------

#contrast code 'smoking' variable
liege_master$smoke_unk  <- -2*(liege_master$SMOKE_YES=='UNK') + 1*(liege_master$SMOKE_YES %in% c('N', 'Y'))
liege_master$smoke_YN <- 0*(liege_master$SMOKE_YES=='UNK') - 1*(liege_master$SMOKE_YES == 'N') + 1*(liege_master$SMOKE_YES == 'Y')
