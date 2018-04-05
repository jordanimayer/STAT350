# set working directory to Lab 03
setwd("C:/Users/jordan/Google Drive/Courses Spring 2018/STAT 350/STAT 350 Labs/Lab 03")
# set up ggplot2 for plotting
library(ggplot2)
# close any open plots
graphics.off()

### PART B ###
# generate 10 observations from a normal distribution with
# mean = 9 and standard deviation = 4.5
NormalB <- rnorm(10,mean=9,sd=4.5)
write.table(NormalB, "NormalB.txt", sep="\t")
Normal <- NormalB
# create histogram
windows()
title <- "Normal Distribution, Part B1"
ggplot(data.frame(Normal=Normal),aes(x=Normal))+
  geom_histogram(aes(y=..density..),bins=sqrt(length(Normal))+2,
                 fill="grey",col="black")+
  geom_density(col="red",lwd=1)+
  stat_function(fun=dnorm,args=list(mean=mean(Normal),sd=sd(Normal)),
                col="blue",lwd=1)+
  ggtitle(title)+
  xlab("Data")+
  ylab("Proportion")
# create normal probability plot, AKA QQ plot
windows()
title <- "Normal Distribution, Part B2"
ggplot(data.frame(Normal=Normal),aes(sample=Normal))+
  stat_qq()+
  geom_abline(slope=sd(Normal),intercept=mean(Normal))+
  ggtitle(title)+
  xlab("Theoretical")+
  ylab("Sample")

### PART C ###
# generate 100 observations from a normal distribution with
# mean = 9 and standard deviation = 4.5
NormalC <- rnorm(100,mean=9,sd=4.5)
write.table(NormalC, "NormalC.txt", sep="\t")
Normal <- NormalC
# create histogram
windows()
title <- "Normal Distribution, Part C1"
ggplot(data.frame(Normal=Normal),aes(x=Normal))+
  geom_histogram(aes(y=..density..),bins=sqrt(length(Normal))+2,
                 fill="grey",col="black")+
  geom_density(col="red",lwd=1)+
  stat_function(fun=dnorm,args=list(mean=mean(Normal),sd=sd(Normal)),
                col="blue",lwd=1)+
  ggtitle(title)+
  xlab("Data")+
  ylab("Proportion")
# create normal probability plot, AKA QQ plot
windows()
title <- "Normal Distribution, Part C2"
ggplot(data.frame(Normal=Normal),aes(sample=Normal))+
  stat_qq()+
  geom_abline(slope=sd(Normal),intercept=mean(Normal))+
  ggtitle(title)+
  xlab("Theoretical")+
  ylab("Sample")

### PART D ###
# generate 100 observations for various distributions, then
# create histograms and normal probability plots
n=100
## (I) right skewed (Exponential) ##
# generate data
Right <- rexp(n,rate=5)
write.table(Right, "Right.txt", sep="\t")
Normal <- Right
title2 <- "Right Skewed Distribution, Part D2"
title3 <- "Right Skewed Distribution, Part D3"
# create histogram
windows()
ggplot(data.frame(Normal=Normal),aes(x=Normal))+
  geom_histogram(aes(y=..density..),bins=sqrt(length(Normal))+2,
                 fill="grey",col="black")+
  geom_density(col="red",lwd=1)+
  stat_function(fun=dnorm,args=list(mean=mean(Normal),sd=sd(Normal)),
                col="blue",lwd=1)+
  ggtitle(title2)+
  xlab("Data")+
  ylab("Proportion")
# create normal probability plot
windows()
ggplot(data.frame(Normal=Normal),aes(sample=Normal))+
  stat_qq()+
  geom_abline(slope=sd(Normal),intercept=mean(Normal))+
  ggtitle(title3)+
  xlab("Theoretical")+
  ylab("Sample")
## (II) left skewed (Beta) ##
Left <- rbeta(n,7,0.8)
Normal <- Left
write.table(Left, "Left.txt", sep="\t")
title2 <- "Left Skewed Distribution, Part D2"
title3 <- "Left Skewed Distribution, Part D3"
# create histogram
windows()
ggplot(data.frame(Normal=Normal),aes(x=Normal))+
  geom_histogram(aes(y=..density..),bins=sqrt(length(Normal))+2,
                 fill="grey",col="black")+
  geom_density(col="red",lwd=1)+
  stat_function(fun=dnorm,args=list(mean=mean(Normal),sd=sd(Normal)),
                col="blue",lwd=1)+
  ggtitle(title2)+
  xlab("Data")+
  ylab("Proportion")
# create normal probability plot
windows()
ggplot(data.frame(Normal=Normal),aes(sample=Normal))+
  stat_qq()+
  geom_abline(slope=sd(Normal),intercept=mean(Normal))+
  ggtitle(title3)+
  xlab("Theoretical")+
  ylab("Sample")
## (III) short tailed (Uniform) ##
Short <- runif(n,min=-3,max=3)
write.table(Short, "Short.txt", sep="\t")
Normal <- Short
title2 <- "Short Tailed Distribution, Part D2"
title3 <- "Short Tailed Distribution, Part D3"
# create histogram
windows()
ggplot(data.frame(Normal=Normal),aes(x=Normal))+
  geom_histogram(aes(y=..density..),bins=sqrt(length(Normal))+2,
                 fill="grey",col="black")+
  geom_density(col="red",lwd=1)+
  stat_function(fun=dnorm,args=list(mean=mean(Normal),sd=sd(Normal)),
                col="blue",lwd=1)+
  ggtitle(title2)+
  xlab("Data")+
  ylab("Proportion")
# create normal probability plot
windows()
ggplot(data.frame(Normal=Normal),aes(sample=Normal))+
  stat_qq()+
  geom_abline(slope=sd(Normal),intercept=mean(Normal))+
  ggtitle(title3)+
  xlab("Theoretical")+
  ylab("Sample")
# (IV) long tailed (t-distribution)
Long <- rt(n,df=1)
write.table(Long, "Long.txt", sep="\t")
Normal <- Long
title2 <- "Long Tailed Distribution, Part D2" 
title3 <- "Long Tailed Distribution, Part D3"
# create histogram
windows()
ggplot(data.frame(Normal=Normal),aes(x=Normal))+
  geom_histogram(aes(y=..density..),bins=sqrt(length(Normal))+2,
                 fill="grey",col="black")+
  geom_density(col="red",lwd=1)+
  stat_function(fun=dnorm,args=list(mean=mean(Normal),sd=sd(Normal)),
                col="blue",lwd=1)+
  ggtitle(title2)+
  xlab("Data")+
  ylab("Proportion")
# create normal probability plot
windows()
ggplot(data.frame(Normal=Normal),aes(sample=Normal))+
  stat_qq()+
  geom_abline(slope=sd(Normal),intercept=mean(Normal))+
  ggtitle(title3)+
  xlab("Theoretical")+
  ylab("Sample")

### PART F ###
# analyze Number of Assaults per 100,000 people
USData <- read.table("USData_Spring.txt", header=TRUE, sep="\t")
USData_clean <- USData[complete.cases(USData),]
attach(USData_clean)
# create histogram
windows()
ggplot(data.frame(AssaultsPerPopulation=AssaultsPerPopulation), aes(AssaultsPerPopulation))+
  geom_histogram(aes(y=..density..),
                 bins=sqrt(length(AssaultsPerPopulation))+2,
                 fill="grey",col="black")+
  geom_density(col="red",lwd=1)+
  stat_function(fun=dnorm,args=list(mean=mean(AssaultsPerPopulation),
                                    sd=sd(AssaultsPerPopulation)),
                col="blue",lwd=1)+
  ggtitle("Assaults per 100,000 people, Part F2")+
  xlab("Data")+
  ylab("Proportion")
# create normal probability plot
windows()
ggplot(data.frame(AssaultsPerPopulation=AssaultsPerPopulation),aes(sample=AssaultsPerPopulation))+
  stat_qq()+
  geom_abline(slope=sd(AssaultsPerPopulation),
              intercept=mean(AssaultsPerPopulation))+
  ggtitle("Assaults per 100,000 people, Part F3")+
  xlab("Theoretical")+
  ylab("Sample")

# clean up
detach(USData_clean)