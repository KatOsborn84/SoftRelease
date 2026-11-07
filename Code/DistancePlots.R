#Title: Distance Plots for Experimental Release
#Author: Katie Osborn
#Date: September 23, 2024

#####################################################################

#load your libraries
library(sf)
library(tidyverse)
library(devtools)
devtools::install_github("InteragencyEcologicalProgram/deltamapr")
library(deltamapr)
library(ggspatial)
library(effects)
library(lme4)
library(lmerTest)
library(emmeans)
library(lubridate)
library(ggpubr)
#Now, get the Delta into WGS 84:
newDelta = st_transform(WW_Delta, crs = 4326)

#Okay, now ready to get started. Read in the data:
Recaps = read.csv('Data/DScatch_Aug25.csv') #all DS catch, incl. recaps, created two identical fishID columns, FishID and FishID2
View(Recaps)

#Let R know it has some fish point data, treating recap location as coordinates of interest (ignoring release location for now):
fishpts = st_as_sf(Recaps,
                   coords = c("Longitude", "Latitude"), #identify coordinate columns, note that longitude must come first
                   crs = 4326,#I am using the ESPG number 4326, which is WGS 84, and what we usually deal with
                   remove = FALSE) #keep the latitude and longitude columns


#Load Sam Bashevkin's spacetools"

library(spacetools)

#generate the distances traveled:
fishpts$FishID

fishdistances = Waterdist(Water_map = Delta,
                          Points = fishpts,
                          Latitude_column = Latitude,
                          Longitude_column = Longitude,
                          PointID_column = FishID)

fishdist<-write.csv(fishdistances, file="Output/RiverDistanceMatrix.csv")

#Once printed, run below:
fishdistances = read.csv("Output/RiverDistanceMatrix.csv")#adds an "X" along header row for each fish

#Now, we have a matrix formed of two triangles that mirror each other,
#We just want one triangle so we can take means from the other
#take out the lower triangle so we just have the upper triangle
fishdistances[lower.tri(fishdistances, diag = T)] = NA #turns lower triangle into NA's

#transform that triangle into a table:
fishd = as.data.frame(fishdistances) %>%
  mutate(Fish1 = row.names(fishdistances)) %>%
  pivot_longer(cols = -Fish1, names_to = "Fish2", values_to = "Distance") %>%
  filter(!is.na(Distance))

print(fishd)

fishd<-write.csv(fishd, file="Output/InterFishDistances.csv")
fishd = read.csv("Output/InterFishDistances.csv") #deleted autonumber column, renamed Fish1 to FishID and Fish2 to FishID2
RecapD = merge(x=Recaps, y=fishd, by="FishID", all.x=TRUE)
view(RecapD)
RecapD<- write.csv(RecapD, file="Output/InterFishRecapD.csv") #deleted empty and duplicate columns, renamed as needed
RecapD<- read.csv("Output/InterFishRecapD.csv")
RecapD23 <- RecapD %>%
  filter(BroodYear!= "2021", BroodYear!= "2020", Mark !="None")#reduce to recaps from differential tagging years
RecapD23<- write.csv(RecapD23, file="Output/InterFishRecapD23.csv")#deleted spare X. columns
RecapD23<- read.csv("Output/InterFishRecapD23.csv")

FishID2 = merge(x=Recaps, y=fishd, by="FishID2", all.x=TRUE)#brings in fields for second fish
FishID2<-write.csv(FishID2, file="Output/FishID2.csv")
#manually deleted FishID and Distance columns (to prevent later duplicates),
#removed duplicates so there's just one record for each FishID2,
#added number 2 to columns,
FishID2<-read.csv("Output/FishID2.csv")
RecapID2<-FishID2 %>%
  filter(BroodYear2 != "2021", BroodYear2 != "2020", Mark2 !="None")#reduce to recaps from differential tagging years
RecapID2<- write.csv(RecapID2, file="Output/RecapFish2.csv")
RecapID2<- read.csv("Output/RecapFish2.csv")


ShoalDist <- merge(x=RecapID2, y=RecapD23, by="FishID2", all.x=TRUE)#brings in fields specific to fish 2
GroupDist <- ShoalDist %>%
  filter(ReleaseEvent==ReleaseEvent2, FishID!=FishID2)#reduce to comparisons between fish from same group, remove self-comparisons
GroupDist <- write.csv(GroupDist, file = "Output/GroupDist.csv") #results in correct number of distances (454 for 2023+2024)!
#Added DistanceKm column (distance/1000) and transport method column
GroupDist <- read.csv("Output/GroupDist.csv")
GroupDist23 <- GroupDist %>%
  filter(BroodYear!= "2024")#reduce to recaps from years 2 and 3
GroupDist23<-write.csv(GroupDist23, file="Output/GroupDist23.csv") #added transport method column in excel
GroupDist23<-read.csv("Output/GroupDist23.csv")

#plot distance between recaps by release type and brood year
ggplot(filter(GroupDist23), aes(x = Method2, y = DistanceKm, fill = Method2)) + geom_boxplot()+
  facet_wrap(~WaterYear)+
  xlab("Release Method")+
   ylab("River distance between recaptures (RKM)")+
  labs(fill = "Release Methods")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust =1, vjust =1))

#plot distance by release habitat and brood year, where carboy = mid-channel and large-scale = nearshore
#As the one nearshore release using carboys had zero recaps, can use transport method as stand-in for release habitat
ggplot(filter(GroupDist23), aes(x = TransportMethod, y = DistanceKm, fill = TransportMethod)) + geom_boxplot()+
  facet_wrap(~WaterYear)+
  xlab("Transport Method")+
  ylab("River distance between recaptures (RKM)")+
  labs(fill = "TransportMethod")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust =1, vjust =1))

#let's look at significance with release event as a random factor
GroupDist23=mutate(GroupDist23, BroodYear = as.factor(BroodYear))
recapmod = lmer(DistanceKm ~ Method2 + BroodYear + (1|ReleaseEvent), data = GroupDist23)
summary(recapmod)
plot(allEffects(recapmod))
#removing brood year as a predictive variable
recapmod2 = lmer(DistanceKm ~ Method2 + (1|ReleaseEvent), data = GroupDist23)
summary(recapmod2)
plot(allEffects(recapmod2))

#repeating above with transport method alone:
recapmod3 = lmer(DistanceKm ~ TransportMethod + BroodYear + (1|ReleaseEvent), data = GroupDist23)
summary(recapmod3)
plot(allEffects(recapmod3)) #there may be something with transport method, remove BY as predictor
recapmod4 = lmer(DistanceKm ~ TransportMethod + (1|ReleaseEvent), data = GroupDist23)
summary(recapmod4)
plot(allEffects(recapmod4))#p-value of 0.0989: not significant but could be with more testing

#Just for kicks, repeat with WY2023-25, just looking at Transport method
recapmod5 = lmer(DistanceKm ~ TransportMethod + (1|ReleaseEvent), data = GroupDist)
summary(recapmod5) #p-value of 0.0269
plot(allEffects(recapmod5))#p-value of 0.0989: not significant but could be with more testing

aovmod = anova(recapmod)
aovmod2 = anova(recapmod2)
aovmod3 = anova(recapmod3)
aovmod4 = anova(recapmod4)
aovmod5 = anova(recapmod5)

tukey = emmeans(recapmod, pairwise ~ Method2)
summary(tukey)
tukey2 = emmeans(recapmod2, pairwise ~ Method2)
summary(tukey2)

#Just for kicks, what does it look like with both years combined?
ggplot(filter(GroupDist23), aes(x = Method2, y = DistanceKm, fill = Method2)) + geom_boxplot()+
  xlab("Release Method")+
  ylab("Distance between recaptures (RKM)")+
  labs(fill = "Release Methods")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust =1, vjust =1))

#To plot as a bubbleplot, need the average distance between recaps for each group
InterFishAvgs <- aggregate(GroupDist$Distance, list(GroupDist$ReleaseEvent), FUN=mean)
InterFishAvgs <-write.csv(InterFishAvgs, file="Output/InterFishAvgs.csv")#renamed columns to match rates file, added DistanceKm
rates<-read.csv("Data/RecapRates.csv")
InterFishAvgs <-read.csv("Output/InterFishAvgs.csv")
RecapDist = merge(x=rates, y=InterFishAvgs, by="ReleaseEvent", all.x=TRUE)
RecapRates<- write.csv(RecapDist, file="Output/RecapRates.csv")#manually changed value to NA for one event with 2 recaps, removed NA rows
RecapRatesR<- read.csv("Output/RecapRatesReduced.csv")

#plotted once with all three years, then removed WY2025 and replotted
ggplot(RecapRatesR, aes(x = Type2, y = AvgGroupDistKm, size = recapsN, color=BroodYear)) +
         geom_point(alpha=0.5)+
  scale_size(range = c(4,24), name="Number of Recaptures")+
  theme_bw()+
  xlab("Release Method")+
  ylab("Distance between recaptures (RKM)")+
    theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust =1, vjust =1))

### DISTANCE TRAVELED##############
#Distance traveled is simpler. First, need to bring in each release site as its own dataframe
RV = data.frame(Latitude = 38.124736, Longitude = -121.699521)
RVs = data.frame(Latitude = 38.1013, Longitude = -121.72813)
Sandy = data.frame(Latitude = 38.139752, Longitude = -121.694706)
SDWSCa = data.frame(Latitude = 38.275563, Longitude = -121.661374)
SDWSCb = data.frame(Latitude = 38.237842, Longitude = -121.673802)

#Subset the data by release site, based on release event (5 sites used in Y2 and Y3)
#Same release location for soft releases BY2023 1b, 3b, and 4b (RVs: 38.1013	-121.72813)
#Same release location for carboy releases BY2022 2a, BY2023 1a and 3a (RV: 38.124736	-121.699521)
#Same release location for large-scale releases BY2022 2b, BY2023 4a, LS1 and LS2 (Sandy: 38.139752	-121.694706)
#Different release locations for the two SDWSC releases, BY2022 3a and 3b

SDWSCafish <-fishpts %>%
  filter(ReleaseEvent== "BY2022 3a")
SDWSCbfish <-fishpts %>%
  filter(ReleaseEvent== "BY2022 3b")
RVfish <- fishpts %>%
  filter(ReleaseEvent %in% c("BY2022 2a","BY2023 1a", "BY2023 3a"))
RVsfish <- fishpts %>%
  filter(ReleaseEvent %in% c("BY2023 1b","BY2023 3b", "BY2023 4b"))
sandyfish <- fishpts %>%
  filter(ReleaseEvent %in% c("BY2022 2b","BY2023 4a", "BY2023 LS1", "BY2023 LS2"))


DistSDWSCa = GGdist(Water_map = spacetools::Delta, Points = SDWSCafish, Latitude_column = `Latitude`,
                Longitude_column = `Longitude`, PointID_column = FishID, EndPoint = SDWSCa)
DistSDWSCb = GGdist(Water_map = spacetools::Delta, Points = SDWSCbfish, Latitude_column = `Latitude`,
                    Longitude_column = `Longitude`, PointID_column = FishID, EndPoint = SDWSCb)
DistSDWSCa<-write.csv(DistSDWSCa, file="Output/DistanceTraveledSDWSCa.csv")
DistSDWSCb<-write.csv(DistSDWSCb, file="Output/DistanceTraveledSDWSCb.csv")

DistRV = GGdist(Water_map = spacetools::Delta, Points = RVfish, Latitude_column = `Latitude`,
                Longitude_column = `Longitude`, PointID_column = FishID, EndPoint = RV)
DistRVs = GGdist(Water_map = spacetools::Delta, Points = RVsfish, Latitude_column = `Latitude`,
                Longitude_column = `Longitude`, PointID_column = FishID, EndPoint = RVs)
DistRV<-write.csv(DistRV, file="Output/DistanceTraveledRV.csv")
DistRVs<-write.csv(DistRVs, file="Output/DistanceTraveledRVs.csv")

DistSandy = GGdist(Water_map = spacetools::Delta, Points = sandyfish, Latitude_column = `Latitude`,
                Longitude_column = `Longitude`, PointID_column = FishID, EndPoint = Sandy)
DistSandy<-write.csv(DistSandy, file="Output/DistanceTraveledSandy.csv")

#manually combined data in Excel, read in combined values for both years:
DistTraveled = read.csv("Output/DistanceTraveledY2and3.csv")
DistTraveled$rkm<-(DistTraveled$Distance/1000)
#Join distance traveled data to existing:
RecapTravel = merge(x=Recaps, y=DistTraveled, by="FishID", all.x=TRUE)
view(RecapTravel)
RecapTravel <- write.csv(RecapTravel, file="Output/DistTravel.csv")
RecapTravel <- read.csv("Output/DistTravel.csv")

#####PLOTS#############
#plot distance traveled by release type and brood year:
ggplot(filter(RecapTravel, Mark != "None", BroodYear != "2020", BroodYear != "2021", BroodYear != "2024"), aes(x = Method2, y = rkm, fill = Method2)) + geom_boxplot()+
  facet_wrap(~WaterYear)+
  xlab("Release Method")+
  ylab("Distance Traveled from Release Site (RKM)")+
  labs(fill = "Release Method")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust =1, vjust =1))

#now plot by transport method
ggplot(filter(RecapTravel, Mark != "None", BroodYear != "2020", BroodYear != "2021", BroodYear != "2024"), aes(x = Method2, y = rkm, fill = Method2)) + geom_boxplot()+
  facet_wrap(~WaterYear)+
  xlab("Release Method")+
  ylab("Distance Traveled from Release Site (RKM)")+
  labs(fill = "Release Method")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust =1, vjust =1))

#Let's run some stats
RecapTravel <- RecapTravel %>%
  filter(Mark != "None", BroodYear != "2020")
RecapTravel23 <- RecapTravel %>%
  filter(BroodYear != "2021", BroodYear != "2024")

dispersemod=lmer(RKMperDay ~ Method2 + (1|ReleaseEvent), data = RecapTravel23)
summary(dispersemod)
plot(allEffects(dispersemod))#large-scale releases travel farther than carboy, but is it significant?
anova(dispersemod) #p-value 0.6308
dispersemod2=lmer(RKMperDay ~ TransportMethod + (1|ReleaseEvent), data = RecapTravel23)#trying transport method instead
summary(dispersemod2) #p-value .37326
plot(allEffects(dispersemod2)) #large-scale transport results in faster dispersion, on average, apparently non-significant
anova(dispersemod2)
distmod = lmer(rkm ~ Method2 + BroodYear + (1|ReleaseEvent), data = RecapTravel23)
summary(distmod)
plot(allEffects(distmod)) #remove BY as predictor
distmod2 = lmer(rkm ~ Method2 + (1|ReleaseEvent), data = RecapTravel23)
summary(distmod2)
plot(allEffects(distmod2))#nope, now looking at the last three years
distmod3 = lmer(rkm ~ TransportMethod + (1|ReleaseEvent), data = RecapTravel23)
summary(distmod3) #still nothing
plot(allEffects(distmod3))

aov1 = anova(distmod)#p=0.2340
aov2 = anova(distmod2)#p=0.1826
aov3 = anova(distmod3)#p=0.1386

####But does release or transport method affect recap rates?
ggplot(filter(Rates23), aes(x = Type2, y = RecapRate, fill = Type2)) + geom_boxplot()+
  xlab("Release Method")+
  ylab("Recapture Rate (%)")+
  labs(fill = "Release Method")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust =1, vjust =1))

ggplot(filter(Rates23), aes(x = TransportMethod, y = RecapRate, fill = TransportMethod)) + geom_boxplot()+
  xlab("Transport Method")+
  ylab("River distance between recaptures (RKM)")+
  labs(fill = "TransportMethod")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust =1, vjust =1))

recapsmod= lm(RecapRate ~ Type2, data = Rates23)
summary(recapsmod)
anova(recapsmod)#p=.809
recapsmod2= lm(RecapRate ~ TransportMethod, data = Rates23)
summary(recapsmod2)
anova(recapsmod2)#p=0.5448
salvagemod = lm(SalvageRate ~ Type2, data = Rates23)
summary(salvagemod)
anova(salvagemod)#p=0.6298 for non-salvage recap rate and p=0.3334 for salvage rate
salvagemod2 = lm(SalvageRate ~ TransportMethod, data = Rates23)
summary(salvagemod2)
anova(salvagemod2)#p=0.296 for salvage rate

recaps1 <- allEffects(recapsmod)
recapsplot<-plot(recaps1,
                 xlab =  "Release Method", ylab= "Recapture Rate (%)",
                 main="")
recapsplot
aovrecaps = anova(recapsmod)
trecaps = emmeans(recapsmod, pairwise ~ Type2)
summary(trecaps)

recaps2 <- allEffects(recapsmod2)
recapsplot2<-plot(recaps2,
                  xlab =  "Transport Method", ylab= "Recapture Rate (%)",
                  main="")
recapsplot2
aovrecaps2 = anova(recapsmod2)
trecaps2 = emmeans(recapsmod2, pairwise ~ TransportMethod)
summary(trecaps)

