#FIRST WE HAVE TO READ THE DATASET
#the data has been preprocessed and cleaned

cdata<-read.csv('world_development_ds.csv')

#getting the first 5 rows and last rows
head(cdata)
tail(cdata)
#summarizing and describing the data set
summary(cdata)
 
#DESCRIPTIVE STATISTICAL ANALYSIS
#fBasic was installed for the descriptive analysis

install.packages('fBasics')
library(fBasics)
attach(cdata)

#summarizing the needed columns from the dataset
basicStats(data.frame(IU,SIS,FBS,FTS,ACFT,AE,REC,TNRR,RPCT))

#values shown are in exponential form, converts to absolute values using
options(scipen = 999)
basicStats(data.frame(IU,SIS,FBS,FTS,ACFT,AE,REC,TNRR,RPCT))

#performing further descriptive statistics on the data set coefficient of variance, etc
install.packages('pastecs')
library(pastecs)           
stat.desc(cdata)

#performing further descriptive statistics/psychometric analysis on the indicators/variables separately
install.packages(psych)
library(psych)
psych::describe(cdata)

#performing further psychometric analysis on the indicators/variables separately, group by country Name
describeBy(cdata~Country.Name)


#CORRELATIONAL ANALYSIS
#determining the degree of correlation between variables/indicators using various functions and methods
#parametric test to determine the level of correlation
#to know the level of correlation between internet users and secured internet servers
cor.test(cdata$IU,cdata$SIS)

#correlation between indicators
#renaming columns
iu = cdata$IU
sis= cdata$SIS
fbs = cdata$FBS
fts = cdata$FTS
acft = cdata$ACFT
ae=cdata$AE
tnrr = cdata$TNRR
rpct= cdata$RPCT
#correlation between indicators
cor.test(iu,sis)
cor.test(fbs,fts)
cor.test(ae,acft)
cor.test(iu,acft)
cor.test(iu,fbs)
cor.test(sis,fbs)
cor.test(iu,rpct)

#To visualize the level of correlation amongst indicators, assign dataset to a new variable and plot
fn<-cdata
plot(fn)
#unwanted columns, country and year, was removed using the code below and replotted
fn1<-cdata[c(-1,-2)]
plot(fn1)
#dataset correlation line of code, shows as each indicator relates to another
#uses pearson correlation by default
cor(fn1)
cor(fn1, method ='spearman')
cor(fn1, method ='kendall')
#the three methods above shows almost similar values, no much difference

#now we have to plot the dataset correlation graph using corrplot
install.packages("corrplot")
library(corrplot)
#we assigned to a new variable to enable plotting, using circle default method,pie,color and number methods as well
nf<-cor(fn1)
corrplot(nf)
#corrplot(nf, method = 'pie')
corrplot(nf, method = 'color')
corrplot(nf, method = 'number',type ='upper')

plot(density(cdata$FBS))#density of distribution
hist(cdata$FBS)#frequency of distribution



#ANALYSIS OF VARIANCE (ANOVA), TEST OF MEANS OF INDICATORS
rc<-c(rep('IU',170),rep('ACFT',170),rep('AE',170))
rc
#putting vectors together and assigning under two columns to enable ANOVA
weight<-c(cdata$IU,cdata$ACFT,cdata$AE)
weight

ds<-data.frame(rc,weight)
ds

#to visualize the ANOVA after cleaning and preparation, weight depends on rc, using ds data set
boxplot(weight~rc,data = ds)
#ANOVA
fn1.aov<-aov(weight~rc,data = ds)
summary(fn1.aov)



#REGRESSION ANALYSIS
#here we  install/load the needed libraries for this analysis
library(fBasics)
library(lmtest)
library(tseries)
library(faraway)
options(scipen = 999)

#simple and Multiple Linear regression on correlated indicators(variables)
#linear model lmtest on the dataset
y=lm(IU~ACFT+AE)
summary(y)
hist(y$residuals)

#To test the assumptions, 
#The mean of the residual should be as good as zero(0)
ET=residuals(y)
mean(ET)

#Test the null of normality for x, using jarque bera test(x)
jarque.bera.test(ET)
#P-value is significant and data is not normally distributed
#in the above p-value is less than significance value, so we reject null hypothesis

#we test for autocorrelation of disturbances using Durbin watson test
#checking if it's serial or auto correlation

dwtest(y)

#we test against heteroskedasticty, using Breusch paga test
bptest(y)
#p-value is more than 0.05 sig. level,so its homoskedastic

#we check for multiconnectivity using faraway
faraway::vif(y)

#we check for robustness of the model
ETy=y$residuals
Fit = y$fitted.values
Fit
#values from the above are displayed in log format, so we want it to appear in absolute values format
Apv = 2.718^Fit
Apv

#we plot columns(using the log form of Fit)
plot(IU,Fit)
#we need to add line to show the regression level of actual and predicted
abline(lm(IU~Fit))
#if the difference between actual and predicted value is positive, the data points will be above regression line 
#and vice versa
#Here we have positive outcome as data points are above lines
#less difference between regression line, more desirable
plot(abline(Fit))

#T-TEST ANALYSIS
#H-null = percentage access to electricity is 100 in Developed countries compared to underdeveloped
#H-alt. = percentage access to electricity is not 100 in Developed countries compared to underdeveloped
#@ 5% significant level

library(tidyverse)
library(patchwork)

#One Sample t-test
#we pipe into Ttest
#the argument is piped into the Ttest: mu as the population mean we're using
cdata%>%
filter(Country.Name == "Cameroon")%>%
select(AE)%>%
t.test(mu = 100)

#Two-sided test for difference of means
#A sample each from undeveloped and developed countries
cdata%>%
filter(Country.Name %in% c("Cameroon","United Kingdom"))%>%
t.test(AE~Country.Name,data = .,
       alternative = "two.sided")

#One-sided test for difference of means
#A sample each from undeveloped and developed countries
cdata%>%
  filter(Country.Name %in% c("Cameroon","United Kingdom"))%>%
  t.test(AE~Country.Name,data = .,
         alternative = "less",
         conf.level=0.95)

#paired t-test
#A sample each from undeveloped and developed countries
cdata%>%
filter(Year %in% c(2015,2019) & Country.Name %in% c("Cameroon",'United Kingdom'))%>%
mutate(Year=factor(Year,levels = c(2019,2015)))%>%
t.test(AE~Year,data = .,paired = TRUE,alternative='less', conf.level=0.95)

#check for variance
var(cdata$AE[AE&Country.Name=="Cameroon"])
var(cdata$AE[AE&Country.Name=="United Kingdom"])

rm(list = ls())
