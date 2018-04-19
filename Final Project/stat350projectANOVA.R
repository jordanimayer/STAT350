#####
# Jordan Mayer
# STAT 350
# Project
# April 19, 2018
#####

### setup ###
setwd("C:/Users/jordan/Google Drive/Courses Spring 2018/STAT 350/STAT 350 Labs/Project")
# set working directory
library(ggplot2)  # set up ggplot2 for plotting
graphics.off()  # close any open figures
USData <- read.table("US_Data.txt", header=TRUE, sep="\t")  # get US Data
US_clean <- USData[complete.cases(USData),]  # clean US Data

# data of interest: robbery rate (RobberiesPerPopulation) across 
# regions of US (Region)

### Part b ###

# check normality via histograms and normal probability plots
title <- "Robberies Per 100,000 People"
# calculate theoretical density curves
xbar <- tapply(US_clean$RobberiesPerPopulation, US_clean$Region, mean)
sd <- tapply(US_clean$RobberiesPerPopulation, US_clean$Region, sd)
US_clean$normal.density <- apply(US_clean, 1, function(x) {
  dnorm(as.numeric(x["RobberiesPerPopulation"]),
        xbar[x["Region"]], sd[x["Region"]])
})
# create histograms
hist <- ggplot(US_clean,aes(x=RobberiesPerPopulation))+
  geom_histogram(aes(y=..density..),bins=sqrt(nrow(US_clean))+2,
                 fill="grey",col="black")+
  facet_grid(Region ~ .)+
  geom_density(col="red",lwd=1)+
  geom_line(aes(y=normal.density),col="blue",lwd=1)+
  ggtitle(title)
ggsave(hist,filename="hist.jpg",width=6,height=6)
# calculate slopes and intercepts for lines of normality
US_clean$intercept <- apply(US_clean, 1, function(x){xbar[x["Region"]]})
US_clean$slope <- apply(US_clean, 1, function(x){sd[x["Region"]]})
# create normal probability plots
qq <- ggplot(US_clean,aes(sample=RobberiesPerPopulation))+
  stat_qq()+
  facet_grid(Region ~ .)+
  geom_abline(data=US_clean,aes(intercept=intercept,slope=slope))+
  ggtitle(title)
ggsave(qq,filename="qq.jpg",width=6,height=6)

# transform data
US_clean$logRobberiesPerPopulation <- log(US_clean$RobberiesPerPopulation)
# remove non-finite data
US_finite <- subset(US_clean, US_clean$logRobberiesPerPopulation != -Inf)
title = "Robberies Per 100,000 People (Transformed)"
# calculate theoretical density curves
xbar_log <- tapply(US_finite$logRobberiesPerPopulation, US_finite$Region, mean)
sd_log <- tapply(US_finite$logRobberiesPerPopulation, US_finite$Region, sd)
US_finite$normal.density.log <- apply(US_finite, 1, function(x) {
  dnorm(as.numeric(x["logRobberiesPerPopulation"]),
        xbar_log[x["Region"]], sd_log[x["Region"]])
})
# create histograms
hist_log <- ggplot(US_finite,aes(x=logRobberiesPerPopulation))+
  geom_histogram(aes(y=..density..),bins=sqrt(nrow(US_finite))+2,
                 fill="grey",col="black")+
  facet_grid(Region ~ .)+
  geom_density(col="red",lwd=1)+
  geom_line(aes(y=normal.density.log),col="blue",lwd=1)+
  ggtitle(title)
ggsave(hist_log,filename="hist_log.jpg",width=6,height=6)
# calculate slopes and intercepts for lines of normality
US_finite$intercept <- apply(US_finite, 1, function(x){xbar_log[x["Region"]]})
US_finite$slope <- apply(US_finite, 1, function(x){sd_log[x["Region"]]})
# create normal probability plots
qq_log <- ggplot(US_finite,aes(sample=logRobberiesPerPopulation))+
  stat_qq()+
  facet_grid(Region ~ .)+
  geom_abline(data=US_finite,aes(intercept=intercept,slope=slope))+
  ggtitle(title)
ggsave(qq_log,filename="qq_log.jpg",width=6,height=6)

### Part c ###
# display sample statistics
# sample sizes
tapply(US_finite$logRobberiesPerPopulation, US_finite$Region, length)
# sample means
tapply(US_finite$logRobberiesPerPopulation, US_finite$Region, mean)
# sample standard deviations
tapply(US_finite$logRobberiesPerPopulation, US_finite$Region, sd)

### Part d ###
# create side-by-side boxplots
box_log <- ggplot(US_finite, aes(x=US_finite$Region,
                                 y=US_finite$logRobberiesPerPopulation))+
  geom_boxplot()+
  stat_boxplot(geom="errorbar")+
  stat_summary(fun.y=mean,col="black",geom="point",size=3)+
  ggtitle(title)
ggsave(box_log,filename="box_log.jpg",width=6,height=6)
# create effects plot
effects_log <- ggplot(data=US_finite,aes(x=US_finite$Region,
                                         y=US_finite$logRobberiesPerPopulation))+
  stat_summary(fun.y=mean,geom="point")+
  stat_summary(fun.y=mean,geom="line",aes(group=1))+
  ggtitle(title)
ggsave(effects_log,filename="effects_log.jpg",width=6,height=6)

### Part e ###
# ANOVA significance test
# perform ANOVA significance test
fit <- aov(logRobberiesPerPopulation ~ Region, data=US_finite)
summary(fit)
# perform multiple-comparison via Tukey procedure
test.Tukey <- TukeyHSD(fit, conf.level=0.95)
test.Tukey