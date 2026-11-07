#Title: Distribution Maps for Experimental Release
#Author: Katie Osborn
#Date: September 17, 2024

#load your libraries
library(sf)
library(tidyverse)
library(devtools)
devtools::install_github("InteragencyEcologicalProgram/deltamapr")
library(deltamapr)
library(ggspatial)
library(ggpubr)

#Now, get the Delta into WGS 84:
newDelta = st_transform(WW_Delta, crs = 4326)

#Okay, now ready to get started. Read in the data:
Recaps = read.csv('Data/DScatch_Aug25.csv') #all DS catch, incl. recaps
View(Catch)
getwd()
Releases = read.csv('Data/ReleaseSites.csv') #all release sites, pulled from summary reports
view(Releases)

#Let R know it has some fish point data, treating recap location as coordinates of interest (ignoring release location for now):
fishpts = st_as_sf(Recaps,
                      coords = c("Longitude", "Latitude"), #identify coordinate columns, note that longitude must come first
                      crs = 4326,#I am using the ESPG number 4326, which is WGS 84, and what we usually deal with
                      remove = FALSE) #keep the latitude and longitude columns
str(fishpts)
sites = st_as_sf(Releases,
                   coords = c("LongitudeR", "LatitudeR"), #identify coordinate columns, note that longitude must come first
                   crs = 4326,#I am using the ESPG number 4326, which is WGS 84, and what we usually deal with
                   remove = FALSE) #keep the latitude and longitude columns

#Subset points by study year
fishpts$RecapDate <- as.Date(fishpts$RecapDate)

y1fish <-fishpts %>%
  filter(BroodYear== "2021", LifeStage %in% c("Adult","200 dph Juvenile"), !RecapMonth %in% c(7,8,9))


view(y1fish)

y2fish <-fishpts %>%
  filter(BroodYear== "2022", LifeStage %in% c("Adult","200 dph Juvenile"))
y3fish <-fishpts %>%
  filter(BroodYear== "2023", LifeStage %in% c("Adult","200 dph Juvenile"))
View(y3fish)

#plot those fish by year, using year 1 as a test case:
ggplot()+
  geom_sf(data = WW_Delta)+ #add waterways
  geom_sf(data = y1fish) #add fish points
#That worked nicely, now to add some colors to the points:

ggplot()+
  geom_sf(data = WW_Delta, fill = "azure")+ #color waterways
  geom_sf(data = y1fish)+ #add fish points
  coord_sf(xlim = c(-121.4, -122.2), ylim = c(37.8, 38.4))+ #zoom into area of interest
  annotation_scale()+
  annotation_north_arrow(location = "tl")+
  ylab(NULL)+
  xlab(NULL)+
  theme_bw()

#########ReleaseSiteMap
ggplot()+
  geom_sf(data = WW_Delta, fill = "azure", color="lightblue4", size=0.06)+
  geom_sf(data = sites, aes(color = as.factor(Sites)), size=2)+
  scale_color_manual(values = c("gold","turquoise","lightgreen","forestgreen","blue","red","pink","purple"), name = "Sites")+
  coord_sf(xlim = c(-121.4, -122.2), ylim = c(37.8, 38.4))+
  annotation_scale()+
  annotation_north_arrow(location = "tl")+
  ylab(NULL)+
  xlab(NULL)+
  theme_bw() +
  theme(
    axis.text = element_text(size = 12, color="black", face = "bold"),
    legend.text = element_text(size = 11, color="black"),
    legend.title = element_text(size=12, color = "black", face="bold"))

#Now that it works, apply to year 1-3 data. Narrowed latitude range to fit multiple maps side by side
#adjusted color palette to be colorblind friendly, gave darker outlines to points
Y1 <- ggplot()+
  geom_sf(data = WW_Delta, fill = "azure", color="lightblue4", size=0.6)+
  geom_sf(data = y1fish, aes(shape = as.factor(Method2), fill = as.factor(Method2), color=as.factor(Method2)),
          size=2.5, stroke=1.5)+
  scale_shape_manual(values = c(24,22), name = "Release Method")+
  scale_fill_manual(values = c("#4B0082","#228B22"), name = "Release Method")+
  scale_color_manual(values = c("#320061", "#145216"),name = "Release Method")+
  scale_x_continuous(breaks = seq(-121.5, -122.1, by = -0.2))+
  coord_sf(xlim = c(-121.5, -122.1), ylim = c(37.8, 38.4))+
  annotation_north_arrow(location = "bl")+
  ylab(NULL)+
  xlab(NULL)+
  theme_bw()+
  theme(
    axis.text = element_text(size = 11, color="black", face = "bold"),
    legend.text = element_text(size = 11, color="black"),
    legend.title = element_text(size=12, color = "black", face="bold"))

Y2 <- ggplot()+
  geom_sf(data = WW_Delta, fill = "azure", color="lightblue4", size=0.6)+
  geom_sf(data = y2fish, aes(shape = as.factor(Method2), fill = as.factor(Method2), color = as.factor(Method2)),
          size=2.5, stroke = 1.5)+
  scale_shape_manual(values = c(21,23,24,25), name = "Release Method")+
  scale_fill_manual(values = c("#2E8B57","#B8860B","#4B0082","#FF8C00"), name = "Release Method")+
  scale_color_manual(values = c("#145236", "#8B6508", "#320061", "#CC7000"),name = "Release Method")+
  scale_x_continuous(breaks = seq(-121.5, -122.1, by = -0.2))+
  coord_sf(xlim = c(-121.5, -122.1), ylim = c(37.8, 38.4))+
  ylab(NULL)+
  xlab(NULL)+
  theme_bw()+
  theme(
    axis.text = element_text(size = 11, color="black", face = "bold"),
    legend.text = element_text(size = 11, color="black"),
    legend.title = element_text(size=12, color = "black", face="bold"))

Y3<- ggplot()+
  geom_sf(data = WW_Delta, fill = "azure", color="lightblue4", size=0.6)+
  geom_sf(data = y3fish, aes(shape = as.factor(Method2), fill = as.factor(Method2), color = as.factor(Method2)),
          size=2.5, stroke = 1.5)+
  scale_shape_manual(values = c(21,23,24,25), name = "Release Method")+
  scale_fill_manual(values = c("#2E8B57","#B8860B","#4B0082","#FF8C00"), name = "Release Method")+
  scale_color_manual(values = c("#145236", "#8B6508", "#320061", "#CC7000"),name = "Release Method")+
  scale_x_continuous(breaks = seq(-121.5, -122.1, by = -0.2))+
  coord_sf(xlim = c(-121.5, -122.1), ylim = c(37.8, 38.4))+
  annotation_scale()+
  ylab(NULL)+
  xlab(NULL)+
  theme_bw()+
  theme(
    axis.text = element_text(size = 11, color="black", face = "bold"),
    legend.text = element_text(size = 11, color="black"),
    legend.title = element_text(size=12, color = "black", face="bold"))
