#####
# Jordan Mayer
# STAT 350
# Lab 09
# April 19, 2018
#####

### setup ###
setwd("C:/Users/jordan/Google Drive/Courses Spring 2018/STAT 350/STAT 350 Labs/Lab 09 - Linear Regression")
# set working directory
library(ggplot2)  # set up ggplot2 for plotting
graphics.off()  # close any open figures
USData <- read.table("US_Data.txt", header=TRUE, sep="\t")  # get US Data
US_clean <- USData[complete.cases(USData),]  # clean US Data
# analyze only "typical counties"
m <- mean(US_clean$MedianIncome)
s <- sd(US_clean$MedianIncome)
US_typ <- subset(US_clean, m-2*s < US_clean$MedianIncome & 
                   US_clean$MedianIncome < m+2*s)

### Part B ###

# create scatterplot
scatter <- ggplot(US_typ, aes(x=MedianIncome, y=TestScore)) +
  geom_point(shape = 1) +
  geom_smooth(method = lm, se = FALSE) +
  ggtitle('Relationship between Test Score and Income') +
  xlab('Median Income') +
  ylab('Test Score')
ggsave(scatter, filename='scatter.jpg', width=6, height=6)

# check correlation
cor(US_typ$MedianIncome, US_typ$TestScore)

# perform linear regression and display results
US_lm <- lm(TestScore ~ MedianIncome, data=US_typ)
summary(US_lm)

# create scatterplot of residuals
res_scatter <- ggplot(data.frame(residuals = US_lm$residuals, 
                                 MedianIncome = US_typ$MedianIncome),
                      aes(x = MedianIncome, y = residuals)) +
  geom_point(shape = 1) +
  geom_smooth(method = lm, se = FALSE) +
  ggtitle('Residuals from Income/Test Score Analysis') +
  xlab('Median Income') +
  ylab('Residuals')
ggsave(res_scatter, filename='res_scatter.jpg',width=6,height=6)

# check normality of residuals using histogram and normal probability
# plot
attach(US_lm)
res_hist <- ggplot(data.frame(residuals=residuals),aes(residuals))+
  geom_histogram(aes(y=..density..),
                 bins=sqrt(length(residuals))+2,
                 fill='grey',col='black')+
  geom_density(col='red',lwd=1)+
  stat_function(fun=dnorm,args=list(mean=mean(residuals),
                                    sd=sd(residuals)),
                col='blue',lwd=1)+
  ggtitle('Residuals from Income/Test Score Analysis')+
  xlab('Data')+
  ylab('Proportion')
ggsave(res_hist, filename='res_hist.jpg',width=6,height=6)
res_qq <- ggplot(data.frame(residuals),aes(sample=residuals))+
  stat_qq()+
  geom_abline(slope=sd(residuals),intercept=mean(residuals))+
  ggtitle('Residuals from Income/Test Score Analysis')+
  xlab('Theoretical')+
  ylab('Sample')
ggsave(res_qq, filename='res_qq.jpg',width=6,height=6)
detach(US_lm)

# check two-sided 99% confidence interval for slope and intercept
confint(US_lm, level=0.99)