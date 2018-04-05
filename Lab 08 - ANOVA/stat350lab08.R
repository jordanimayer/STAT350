#####
# Jordan Mayer
# STAT 350
# Lab 07
# March 29, 2018
#####

# setup
setwd("W:/Courses Spring 2018/STAT 350/STAT 350 Labs/Lab 08")
  # set working directory
library(ggplot2)  # set up ggplot2 for plotting
graphics.off()  # close any open figures
USData <- read.table("US_Data.txt", header=TRUE, sep="\t")  # get US data
US_clean <- USData[complete.cases(USData),]  # clean US Data
US_NE <- subset(US_clean, Region == "NE") # subset for Northeast region only
US_NC <- subset(US_clean, Region == "NC") # subset for North Central region only
US_SO <- subset(US_clean, Region == "SO") # subset for South region only
US_WE <- subset(US_clean, Region == "WE") # subset for West region only

attach(US_clean)

### PART B ###
# data of interest: Average Test Score (TestScore) across regions of US (Region)
# create side-by-side boxplots and effects plot
title = "Average Test Score by Region"
# side-by-side boxplots
box <- ggplot(US_clean, aes(x=Region,y=TestScore))+
  geom_boxplot()+
  stat_boxplot(geom="errorbar")+
  stat_summary(fun.y=mean,col="black",geom="point",size=3)+
  ggtitle(title)
ggsave(box,filename="box.jpg",width=6,height=6)
# effects plot
effects <- ggplot(data=US_clean,aes(x=Region,y=TestScore))+
  stat_summary(fun.y=mean,geom="point")+
  stat_summary(fun.y=mean,geom="line",aes(group=1))+
  ggtitle(title)
ggsave(effects,filename="effects.jpg",width=6,height=6)


# display sample statistics
tapply(TestScore, Region, length)  # display sample sizes
tapply(TestScore, Region, mean)  # display sample means
tapply(TestScore, Region, sd)  # display sample standard deviations

# check normality via histograms
# calculate theoretical density curves
xbar <- tapply(TestScore, Region, mean)
sd <- tapply(TestScore, Region, sd)
detach(US_clean)
US_clean$normal.density <- apply(US_clean, 1, function(x) {
  dnorm(as.numeric(x["TestScore"]),
        xbar[x["Region"]], sd[x["Region"]])
})
# create histograms
hist <- ggplot(US_clean,aes(x=TestScore))+
  geom_histogram(aes(y=..density..),bins=sqrt(nrow(US_clean))+2,
                 fill="grey",col="black")+
  facet_grid(Region ~ .)+
  geom_density(col="red",lwd=1)+
  geom_line(aes(y=normal.density),col="blue",lwd=1)+
  ggtitle(title)
ggsave(hist,filename="hist.jpg",width=6,height=6)

# check normality via normal probability plots
US_clean$intercept <- apply(US_clean, 1, function(x){xbar[x["Region"]]})
US_clean$slope <- apply(US_clean, 1, function(x){sd[x["Region"]]})
# create normal probability plots
qq <- ggplot(US_clean,aes(sample=TestScore))+
  stat_qq()+
  facet_grid(Region ~ .)+
  geom_abline(data=US_clean,aes(intercept=intercept,slope=slope))+
  ggtitle(title)
ggsave(qq,filename="qq.jpg",width=6,height=6)

# perform ANOVA significance test
fit <- aov(TestScore ~ Region, data=US_clean)
summary(fit)

# perform multiple-comparison via Tukey procedure
test.Tukey <- TukeyHSD(fit, conf.level=0.999)
test.Tukey