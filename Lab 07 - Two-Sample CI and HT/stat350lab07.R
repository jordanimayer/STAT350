#####
# Jordan Mayer
# STAT 350
# Lab 07
# March 29, 2018
#####

# setup
setwd("W:/Courses Spring 2018/STAT 350/STAT 350 Labs/Lab 07")
  # set working directory
library(ggplot2)  # set up ggplot2 for plotting
graphics.off()  # close any open figures
USData <- read.table("US_Data.txt", header=TRUE, sep="\t")  # get US Data
US_clean <- USData[complete.cases(USData),]  # clean US Data
US_NE <- subset(US_clean, Region == "NE") # subset for Northeast region only
US_NC <- subset(US_clean, Region == "NC") # subset for North Central region only
US_ESDiff <- US_clean$EducationSpendingP2 - US_clean$EducationSpending
  # differences in education spending between period 2 and period 1

### PART B ###
# create plots for Northeast median income and North Central median income
for (reg in c("NE", "NC")) {
  # attach dataset
  if (reg == "NE") {
    attach(US_NE)
    title = "Median Household Income in Northeast US"
  }
  else {
    attach(US_NC)
    title = "Median Household Income in North Central US"
  }
  # boxplot
  box <- ggplot(data.frame(MedianIncome=MedianIncome),aes(x="",y=MedianIncome))+
    stat_boxplot(geom="errorbar")+
    geom_boxplot()+
    ggtitle("Test Scores")+
    stat_summary(fun.y=mean,col="black",geom="point",size=3)+
    ggtitle(title)
  ggsave(filename=paste("box",reg,".png"),box,height=6,width=6)
  
  # histogram
  hist <- ggplot(data.frame(MedianIncome=MedianIncome),aes(MedianIncome))+
    geom_histogram(aes(y=..density..),
                   bins=sqrt(length(MedianIncome))+2,
                   fill="grey",col="black")+
    geom_density(col="red",lwd=1)+
    stat_function(fun=dnorm,args=list(mean=mean(MedianIncome),
                                      sd=sd(MedianIncome)),
                  col="blue",lwd=1)+
    ggtitle(title)+
    xlab("Data")+
    ylab("Proportion")
  ggsave(filename=paste("hist",reg,".png"),hist,height=6,width=6)
  # normal probability plot
  #windows()
  qq <- ggplot(data.frame(MedianIncome),aes(sample=MedianIncome))+
    stat_qq()+
    geom_abline(slope=sd(MedianIncome),intercept=mean(MedianIncome))+
    ggtitle(title)+
    xlab("Theoretical")+
    ylab("Sample")
  ggsave(filename=paste("qq",reg,".png"),qq,height=6,width=6)
  # detach dataset
  if (reg == "NE") {
    detach(US_NE)
  }
  else {
    detach(US_NC)
  }
}

# data are not normally distributed -> transform using log
US_NE_MIlog <- log(US_NE$MedianIncome)  # transformed NE median income
US_NC_MIlog <- log(US_NC$MedianIncome)  # transformed NC median income
# create plots for transformed data
### PART B ###
# create plots for Northeast median income and North Central median income
for (reg in c("NElog", "NClog")) {
  # attach dataset
  if (reg == "NElog") {
    var = US_NC_MIlog 
    title = "Median Household Income in Northeast US (transformed)"
  }
  else {
    var = US_NC_MIlog
    title = "Median Household Income in North Central US (transformed)"
  }
  # boxplot
  box <- ggplot(data.frame(var),aes(x="",y=var),height=6,width=6)+
    stat_boxplot(geom="errorbar")+
    geom_boxplot()+
    ggtitle("Test Scores")+
    stat_summary(fun.y=mean,col="black",geom="point",size=3)+
    ggtitle(title)
  ggsave(filename=paste("box",reg,".png"),box,height=6,width=6)
  
  # histogram
  hist <- ggplot(data.frame(var),aes(var),height=6,width=6)+
    geom_histogram(aes(y=..density..),
                   bins=sqrt(length(var))+2,
                   fill="grey",col="black")+
    geom_density(col="red",lwd=1)+
    stat_function(fun=dnorm,args=list(mean=mean(var),
                                      sd=sd(var)),
                  col="blue",lwd=1)+
    ggtitle(title)+
    xlab("Data")+
    ylab("Proportion")
  ggsave(filename=paste("hist",reg,".png"),hist,height=6,width=6)
  # normal probability plot
  #windows()
  qq <- ggplot(data.frame(var),aes(sample=var),height=6,width=6)+
    stat_qq()+
    geom_abline(slope=sd(var),intercept=mean(var))+
    ggtitle(title)+
    xlab("Theoretical")+
    ylab("Sample")
  ggsave(filename=paste("qq",reg,".png"),qq,height=6,width=6)
}

# conduct two-sample independent hypothesis test, two-sided
t.test(US_NE_MIlog, US_NC_MIlog, mu=0, conf.level=0.95,
       alternative="two.sided", paired=FALSE)

### PART C ###
# create plots of difference in mean
title="Difference in Education Spending between Period 2 and Period 1"
# boxplot
box <- ggplot(data.frame(US_ESDiff),aes(x="",y=US_ESDiff))+
  stat_boxplot(geom="errorbar")+
  geom_boxplot()+
  ggtitle(title)+
  stat_summary(fun.y=mean,col="black",geom="point",size=3)
ggsave(filename="box ED.png",box,height=6,width=6)
# histogram
hist <- ggplot(data.frame(US_ESDiff),aes(US_ESDiff))+
  geom_histogram(aes(y=..density..),
                 bins=sqrt(length(US_ESDiff))+2,
                 fill="grey",col="black")+
  geom_density(col="red",lwd=1)+
  stat_function(fun=dnorm,args=list(mean=mean(US_ESDiff),
                                    sd=sd(US_ESDiff)),
                col="blue",lwd=1)+
  ggtitle(title)+
  xlab("Data")+
  ylab("Proportion")
ggsave(filename="hist ED.png",hist,height=6,width=6)
# normal probability plot
qq <- ggplot(data.frame(US_ESDiff),aes(sample=US_ESDiff))+
  stat_qq()+
  geom_abline(slope=sd(US_ESDiff),intercept=mean(US_ESDiff))+
  ggtitle(title)+
  xlab("Theoretical")+
  ylab("Sample")
ggsave(filename="qq ED.png",qq,height=6,width=6)

# conduct one-sided hypothesis test with difference, greater
t.test(US_ESDiff, mu=70, conf.level=0.95,alternative="greater")
