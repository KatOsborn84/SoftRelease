library(cder)
library(zoo)

#pull all the turbidity data from Rio Vista (for that release, you might want a different station for releases elsewhere)
turbdat = cdec_query("RVB", sensors = 27, start.date = as.Date("2021-10-01"), end.date = as.Date ("2025-03-01"))

turbdat = mutate(turbdat, Month = month(DateTime), Date = date(DateTime), Year = year(DateTime))

#calculate daily average
turbdat$Date <- as.Date(turbdat$DateTime)
turbdat$Year <- year(turbdat$Date)
turbdat$Month <- month(turbdat$Date)
turbdat_ave = group_by(turbdat, Date, Month, Year) %>%
  summarize(Turbidity = mean(Value, na.rm =T))

#calculate the weekly average
test = rollmean(turbdat_ave$Turbidity, 7, align = "right", na.pad = TRUE)

turbdat_ave$weekturb = test

#Now calculate for Lookout Slough/SDWSC
turbdat2 = cdec_query("SGG", sensors = 221, start.date = as.Date("2021-10-01"), end.date = as.Date ("2025-03-01")) #looked up station locations
#website:here: https://cdec.water.ca.gov/webgis/?appid=cdecstation
#Closest station is in shag slough at stair steps, all sensors in region use FNU and lack values for recent years, RVB uses NTU
#Trialed LIB, but it is missing data for the last week of January in 2023, when both SDWSC releases occurred

turbdat2$Date <- as.Date(turbdat2$DateTime)
turbdat2$Year <- year(turbdat2$Date)
turbdat2$Month <- month(turbdat2$Date)
turbdat_ave2 = group_by(turbdat2, Date, Month, Year) %>%
  summarize(Turbidity = mean(Value, na.rm =T))

test2 = rollmean(turbdat_ave2$Turbidity, 7, align = "right", na.pad = TRUE)

turbdat_ave2$weekturb = test2

#join to your fish data
releasestat = read.csv('Data/RecapRates.csv') #all events
releasestat$Release.Date <- as.Date(releasestat$ReleaseDate, format="%m/%d/%Y")
releasestat = left_join(releasestat, turbdat_ave, by = c("Release.Date" = "Date"))
write.csv(releasestat, file = "Output/RecapRatesTurb.csv")

#########RecapRate v Turbidity
Rates <- read.csv(('Output/RecapRatesTurb.csv'))
RatesAll <- read.csv('Data/RecapRatesAllYrs.csv')
rateslm <-lm(RecapRate ~ FFyn, data = RatesAll)
rates1 = lm(RecapRate ~ weekturb, data = Rates)
summary(rates1)
summary(rateslm)
Rates21 = filter(RatesAll, BroodYear != 2021, BroodYear != 2025)
Rates23 = filter(Rates, BroodYear != 2021, BroodYear != 2024)
rates12 = lm(RecapRate ~ weekturb, data = Rates21)
rates23 = lm(RecapRate ~ weekturb, data = Rates23)

summary(rates2)
turb12 <- allEffects(rates2)
turbplot12<-plot(turb2,
                 xlab =  "7-day average turbidity (NTU)", ylab= "Recapture Rate (%)",
                 main="")
turb23 <- allEffects(rates23)
turbplot<-plot(turb23,
               xlab =  "7-day average turbidity (NTU)", ylab= "Recapture Rate (%)",
               main="")
aovmod23 = anova(rates23)
tukey23 = emmeans(rates23, pairwise ~ weekturb)

#testing effects of first flush and turbidity
ratesbest23 = lm(RecapRate ~ FFyn , data = Rates23)
summary(ratesbest23)
plot(allEffects(ratesbest23))
anova(ratesbest23)#p=0.0004559
ratesturb23 = lm(RecapRate ~ weekturb , data = Rates23)
summary(ratesturb23)
plot(allEffects(ratesturb23))
anova(ratesturb23)#p=0.005426


#plotting turbidity and FF relationship
FF<- ggplot(Rates23, aes(x = FFyn, y = RecapRate, fill = FFyn)) + geom_boxplot()+
  xlab("") +
  ylab("Recapture Rate (%)")+
  labs(fill = "FFyn")+
  theme_bw() #boxplot before and after first flush

Rates23$BroodYear <- factor(Rates23$BroodYear)
turbplot <- ggplot(Rates23, aes(x = weekturb, y = RecapRate, shape = Type2, fill = BroodYear, color=BroodYear)) +
  geom_point(size = 3, stroke=1.2) +
  scale_shape_manual(values = c(21,22,24)) +
  scale_fill_manual(values = c("#4B0082","#FF8C00"))+
  scale_color_manual(values = c("#320061", "#CC7000"))+
  geom_smooth(aes(x = weekturb, y = RecapRate), inherit.aes = FALSE, method = "lm", se = TRUE, color = "blue", linewidth = 1.5) +
  xlab("Mean Turbidity 7 days post-release (NTU)") +
  ylab("Recapture Rate (%)")+
  theme_bw() #scatterplot with regression line of recap rate v turb

ggarrange(FF, turbplot,
          labels = c("A)", "B)"), hjust = -3.5, vjust = 2.5,
          ncol = 1, nrow = 2)
ggsave("Output/FFturbplots.png", height=8, width=7, units="in")

ratemod = lmer(RecapRate ~ Type2 + (1|BroodYear), data = Rates23)
summary(ratemod)
plot(allEffects(ratemod))
aovrate = anova(ratemod)
tukeyrate = emmeans(ratemod, pairwise ~ Type2)
summary(tukeyrate)

