library(cder)
library(zoo)

#pull velocity and turbidity data from the Sac R at Freeport for all release years
veldat = cdec_query("FPT", sensors = 20, start.date = as.Date("2010-01-01"), end.date = as.Date("2025-02-28")) #cfs
turbdat = cdec_query("FPT", sensors = 221, start.date = as.Date("2010-01-01"), end.date = as.Date("2025-02-28")) #FNU

veldat = mutate(veldat, Month = month(DateTime), Date = date(DateTime), Year = year(DateTime))
turbdat = mutate(turbdat, Month = month(DateTime), Date = date(DateTime), Year = year(DateTime))

#calculate 3-day rolling average for FPT flow and turbidity
veldat$Date <- as.Date(veldat$DateTime)
veldat$Year <- year(veldat$Date)
veldat$Month <- month(veldat$Date)
veldat_ave = group_by(veldat, Date, Month, Year) %>%
  summarize(velocity = mean(Value, na.rm =T))

turbdat$Date <- as.Date(turbdat$DateTime)
turbdat$Year <- year(turbdat$Date)
turbdat$Month <- month(turbdat$Date)
turbdat_ave = group_by(turbdat, Date, Month, Year) %>%
  summarize(turbidity = mean(Value, na.rm =T))

vel = rollmean(veldat_ave$velocity, 3, align = "right", na.pad = TRUE)
veldat_ave$FPTvel = vel
turb = rollmean(turbdat_ave$turbidity, 3, align = "right", na.pad = TRUE)
turbdat_ave$FPTturb = turb

#join turbidity and flow data
envdata = left_join(veldat_ave, turbdat_ave, by = c("Date" = "Date"))
write.csv(envdata, file = "Output/FPTenvdata.csv")

#########RecapRate v Velocity at RV (no data for SDWSC)
Rates <- read.csv("Data/RecapRates.csv") #pasted in turbidity and velocity values, converted to m/s
velrates = lm(RecapRate ~ Velocity, data = Rates)
summary(velrates)
Rates2 = filter(Rates, BroodYear != 2021)
Rates23 = filter(Rates, BroodYear != 2021, BroodYear != 2024)
Rates4 = filter(Rates, Site != "SDWSC", BroodYear != 2021, BroodYear != 2024)
velrates2 = lm(RecapRate ~ Velocity, data = Rates2)
velrates3 = lm(RecapRate ~ Velocity, data = Rates23)
velrates4= lm(RecapRate ~ Velocity, data = Rates4)
summary(velrates2)
summary(velrates3)
summary(velrates4)

vel3 <- allEffects(velrates3)
velplot<-plot(vel3,
              xlab =  "Daily average velocity (m/sec)", ylab= "Recapture Rate (%)",
              main="")
velplot
aovvel = anova(velrates3)
tukey3 = emmeans(velrates3, pairwise ~ Velocity)
summary(tukey3)

vel4 <- allEffects(velrates4)
velplot4<-plot(vel4,
               xlab =  "Daily average velocity (m/sec)", ylab= "Recapture Rate (%)",
               main="")
velplot4
aovvel4 = anova(velrates4)
tukey4 = emmeans(velrates4, pairwise ~ Velocity)
summary(tukey4)

