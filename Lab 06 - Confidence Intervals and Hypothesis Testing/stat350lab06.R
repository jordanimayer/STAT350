#####
# Jordan Mayer
# STAT 350
# Lab 06
# March 22, 2018
#####

# set working directory to STAT 350
setwd("C:/Users/jordan/Google Drive/Courses Spring 2018/STAT 350/STAT 350 Labs/Lab 06")
# set up ggplot2 for plotting
library(ggplot2)
# close open figures
graphics.off()
# get US Data
USData <- read.table("US_Data.txt", header=TRUE, sep="\t")
USData_clean <- USData[complete.cases(USData),]
attach(USData_clean)

# create histogram
windows()
ggplot(data.frame(TestScore=TestScore), aes(TestScore))+
  geom_histogram(aes(y=..density..),
                 bins=sqrt(length(TestScore))+2,
                 fill="grey",col="black")+
  geom_density(col="red",lwd=1)+
  stat_function(fun=dnorm,args=list(mean=mean(TestScore),
                                    sd=sd(TestScore)),
                col="blue",lwd=1)+
  ggtitle("Test Scores")+
  xlab("Data")+
  ylab("Proportion")
# create boxplot
windows()
ggplot(data.frame(TestScore=TestScore), aes(x="", y=TestScore))+
  stat_boxplot(geom="errorbar")+
  geom_boxplot()+
  ggtitle("Test Scores")+
  stat_summary(fun.y=mean,col="black",geom="point",size=3)
# create normal probability plot
windows()
ggplot(data.frame(TestScore=TestScore),aes(sample=TestScore))+
  stat_qq()+
  geom_abline(slope=sd(TestScore),intercept=mean(TestScore))+
  ggtitle("Test Scores")+
  xlab("Theoretical")+
  ylab("Sample")

# display sample mean, sample standard deviation, and sample
# standard error of the mean
sample_mean<-mean(TestScore)  # sample mean
sample_stdev<-sd(TestScore)      # sample standard deviation
sample_sem<-sd(TestScore)/sqrt(length(TestScore))  # sample standard
                                            # error of mean
sample_mean
sample_stdev
sample_sem

# find 95% confidence interval for mean TestScore
t.test(TestScore,conf.level=0.95,mu=1800,alternative="two.sided")
t.test(TestScore,conf.level=0.95,mu=1608,alternative="two.sided")
t.test(TestScore,conf.level=0.95,mu=1800,alternative="greater")
