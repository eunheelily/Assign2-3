
ca <- read.delim("../wrangle/data/CA_Features_20170401.txt", header=TRUE, sep="|",na="")

library(tidyverse)
colnames(ca)[1] <- "FEATURE_ID"

ca <- select(ca,FEATURE_ID,FEATURE_NAME, FEATURE_CLASS, STATE_ALPHA, COUNTY_NAME, PRIM_LAT_DEC, PRIM_LONG_DEC, SOURCE_LAT_DEC, SOURCE_LONG_DEC, ELEV_IN_M, MAP_NAME, DATE_CREATED, DATE_EDITED)
ca <-ca[!(is.na(ca$PRIM_LAT_DEC)) | !(is.na(ca$PRIM_LONG_DEC)),]
ca <- filter(ca, STATE_ALPHA == "CA")
write.table(ca,"CAdata.csv", sep = "|")

as_tibble(ca)

Analysis1 <- group_by(ca,FEATURE_CLASS)
AnalyFre <- summarise(Analysis1, count=n())
leastclass=which.min(AnalyFre$count)
AnalyFre[leastclass,c("FEATURE_CLASS","count")]

Analysis2 <- group_by(ca,FEATURE_NAME)
AnalyFre2 <- summarise(Analysis2, count=n())
mostname=which.max(AnalyFre2$count)
AnalyFre2[mostname,c("FEATURE_NAME","count")]

Analysis3a <- group_by(ca,COUNTY_NAME)%>% 
  summarise(Centercounty = mean(PRIM_LONG_DEC, na.rm=TRUE))
colnames(Analysis3a)[2] <- "MEAN_LONG_DEC"
Analysis3b <- group_by(ca,COUNTY_NAME)%>% 
  summarise(Centercounty = mean(PRIM_LAT_DEC, na.rm=TRUE)) 
colnames(Analysis3b)[2] <- "MEAN_LAT_DEC"
Analysis3 <- left_join(Analysis3a, Analysis3b)

Featureclass <-read.table("Featureclass.csv", header=TRUE, sep=",")
colnames(Featureclass) = c("FEATURE_CLASS", "CHARACTERISTIC")
Featureclass <- as_tibble(Featureclass)
caF <- merge(ca, Featureclass, by="FEATURE_CLASS")
Analysis4a <- group_by(caF, COUNTY_NAME)
Analysis4 <- summarise(Analysis4a,Naturalfraction = nrow(subset(Analysis4a, CHARACTERISTIC == "Natural"))/nrow(Analysis4a)*100,
            Manmadefraction = nrow(subset(Analysis4a, CHARACTERISTIC == "Manmade"))/nrow(Analysis4a)*100)


        