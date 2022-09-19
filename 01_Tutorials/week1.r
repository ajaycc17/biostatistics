#####################################################
#Biostatistics course in R - Week 4
######################################################

install.packages("gapminder")
library(gapminder)
plot(gapminder_unfiltered)

boxplot(lifeExp~continent, data=gapminder_unfiltered, outline=F)

plot(lifeExp~log(gdpPercap),col=gdpPercap,data=gapminder_unfiltered)

#tabulation
tabulation <- table(gapminder$continent)
barplot(tabulation)
# cross tabulation
table(gapminder$year, gapminder$continent)->crossTab
barplot(crossTab)
barplot(crossTab,beside=TRUE,col=c("red","blue","green","yellow",
                                   "brown","pink","grey","black"))

#comparing more than 2 vectors ----
x = rnorm(10,sd=10,mean=5)
y = rnorm(10,sd=10,mean=6)
z = rnorm(10,sd=10,mean=7)

data.frame(values=c(x,y,z)
           ,variable=c(rep("x",length(x)),rep("y",length(y)),
                       rep("z",length(z))))->xyzBox

boxplot(xyzBox$values~xyzBox$variable)


a = rnorm(10,sd=10,mean=50)
b = rnorm(10,sd=10,mean=6)
c = rnorm(10,sd=10,mean=7)

data.frame(values=c(a,b,c)
           ,variable=c(rep("a",length(a)),rep("b",length(b)),
                       rep("c",length(c))))->abcBox

boxplot(abcBox$values~abcBox$variable)

#rbind will append the rows
rbind(abcBox,xyzBox)->abcxyzBox
boxplot(abcxyzBox$values~abcxyzBox$variable)

#One way ANOVA
res.aov <- aov(values ~ variable, data = abcxyzBox)
#Null hypothesis: All groups have the same mean
res.aov
summary(res.aov)
#Alternative hypothesis:All groups don't have the same mean

res.aov <- aov(values ~ variable, data = xyzBox)
res.aov
summary(res.aov)

res.aov <- aov(values ~ variable, data = abcBox)
res.aov
summary(res.aov)

#Pairwise TukeyHSD ----
TukeyHSD(res.aov)

##########################
#Parametric assumptions
##########################
#Normality: Data have a normal distribution (or at least is symmetric)
#---histogram
#---shapiro test
#---Q-Q plot
#Homogeneity of variances: Data from multiple groups have the same variance
#---levene test
#---check normality of the residuals distribution
#Linearity: Data have a linear relationship - for correlation tests
#Independence: Data are independent
#No Outliers: There should be no extreme outliers.

# 1. Homogeneity of variances
plot(res.aov, 1)

install.packages("car")
library(car)
?leveneTest
leveneTest(values ~ variable, data = abcBox)
#Null hypothesis: the population variances are equal

a = rnorm(10,sd=100,mean=6)
b = rnorm(10,sd=10,mean=6)
c = rnorm(10,sd=10,mean=6)

data.frame(values=c(a,b,c)
           ,variable=c(rep("a",length(a)),rep("b",length(b)),
                       rep("c",length(c))))->abcBox

leveneTest(values ~ variable, data = abcBox)
#Null hypothesis: the population variances are equal

# 2. Normality
plot(res.aov, 2)

# Extract the residuals
aov_residuals <- residuals(object = res.aov )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )

#one way anova when variances are different
oneway.test(abcxyzBox$values~abcxyzBox$variable)

oneway.test(xyzBox$values~xyzBox$variable)
oneway.test(abcBox$values~abcBox$variable)

#Pairwise t-test
pairwise.t.test(abcxyzBox$values,abcxyzBox$variable,
                p.adjust.method = "BH")


pairwise.t.test(xyzBox$values,xyzBox$variable,
                p.adjust.method = "BH")
pairwise.t.test(abcBox$values,abcBox$variable,
                p.adjust.method = "BH")

pairwise.t.test(abcBox$values,abcBox$variable,
                p.adjust.method = "holm")

pairwise.t.test(abcBox$values,abcBox$variable,
                p.adjust.method = "hochberg")

pairwise.t.test(abcBox$values,abcBox$variable,
                p.adjust.method = "hommel")

pairwise.t.test(abcBox$values,abcBox$variable,
                p.adjust.method = "bonferroni")

pairwise.t.test(abcBox$values,abcBox$variable,
                p.adjust.method = "BY")

pairwise.t.test(abcBox$values,abcBox$variable,
                p.adjust.method = "fdr")

pvalues<-c(0.1,0.01,0.02,0.045,0.0001,0.009,0.0099,0.7)
pvalues2<-c(0.01,0.02,0.045,0.0001,0.009,0.0099)

p.adjust(pvalues2,method="BH")

?pairwise.t.test

#Testing equality of variance
#Compute F-test in R

x = rnorm(10,sd=10,mean=5)
y = rnorm(10,sd=100,mean=5)

var.test(x, y, alternative = "two.sided")

data.frame(values=c(x,y),groups=c(rep("x",length(x)),rep("y",length(y))))->data
# using values stored in a data frame
var.test(values ~ groups, data, alternative = "two.sided")


x = rnorm(10,sd=10)
y = rnorm(10,sd=100)

var.test(x, y, alternative = "two.sided")
#one-tailed tests for variance
var.test(x, y, alternative = "less")
var.test(x, y, alternative = "greater")


#Non-parametric test for more than 2 variables

a = rnorm(10,sd=10,mean=50)
b = rnorm(10,sd=10,mean=6)
c = rnorm(10,sd=10,mean=7)

data.frame(values=c(a,b,c)
           ,variable=c(rep("a",length(a)),rep("b",length(b)),
                       rep("c",length(c))))->abcBox

boxplot(abcBox$values~abcBox$variable)
#non-parametric test for more than 2 continuous variables
kruskal.test(abcBox$values~abcBox$variable)
oneway.test(abcBox$values~abcBox$variable)


x = rnorm(10,sd=10,mean=5)
y = rnorm(10,sd=10,mean=6)
z = rnorm(10,sd=10,mean=7)

data.frame(values=c(x,y,z)
           ,variable=c(rep("x",length(x)),rep("y",length(y)),
                       rep("z",length(z))))->xyzBox

boxplot(xyzBox$values~xyzBox$variable)

kruskal.test(xyzBox$values~xyzBox$variable)

# what would be the code to test India, Sweden and Bangladesh together?

setwd("E:/8_Eighth Sem/Biostatistics/Assignment 1/data")#But this will work

lifeExpectancyInYears <- read.csv("life_expectancy_years.csv", header = T, check.names = FALSE)

na.omit(as.numeric(unlist(lifeExpectancyInYears[lifeExpectancyInYears$country=="Sweden",])))->SwedenLife
na.omit(as.numeric(unlist(lifeExpectancyInYears[lifeExpectancyInYears$country=="India",])))->IndiaLife
na.omit(as.numeric(unlist(lifeExpectancyInYears[lifeExpectancyInYears$country=="Bangladesh",])))->BanglaLife
na.omit(as.numeric(unlist(lifeExpectancyInYears[lifeExpectancyInYears$country=="Australia",])))->AussieLife
na.omit(as.numeric(unlist(lifeExpectancyInYears[lifeExpectancyInYears$country=="China",])))->ChinaLife

data.frame(values=c(SwedenLife,IndiaLife,BanglaLife,AussieLife,ChinaLife)
           ,variable=c(rep("Sweden",length(SwedenLife)),rep("India",length(IndiaLife)),
                       rep("Bangladesh",length(BanglaLife)),
                       rep("Australia",length(AussieLife)),
                       rep("China",length(ChinaLife))))->countryBox

boxplot(countryBox$values~countryBox$variable)

kruskal.test(countryBox$values~countryBox$variable)
?kruskal.test
pairwise.wilcox.test(countryBox$values,countryBox$variable)

#Two way ANOVA ----
#add another grouping vector to abcxyzBox

rbind(abcBox,xyzBox)->abcxyzBox
boxplot(abcxyzBox$values~abcxyzBox$variable)

grouping2classes<-c("India","Mandya","Paniput","Amravati","Kanpur","Baroda")

Season<-c("Summer","Winter","Monsoon","Spring","Autumn")

sample(grouping2classes, length(abcxyzBox$variable),replace=TRUE)->grouping2random
abcxyzBox$grouping2random<-grouping2random

sample(Season, length(abcxyzBox$variable),replace=TRUE)->Season2random
abcxyzBox$Season2random<-Season2random

boxplot(values~variable, data=abcxyzBox)
boxplot(values~grouping2random, data=abcxyzBox)

#boxplot with two factors
boxplot(values ~ variable * grouping2random, data=abcxyzBox, frame = FALSE,las=2,
        col = c("red","blue","green","orange","yellow","brown"))

# Two-way interaction plot
interaction.plot(x.factor = abcxyzBox$variable, trace.factor = abcxyzBox$grouping2random,
                 response = abcxyzBox$values, fun = mean,
                 type = "b")

res.aov2 <- aov(values ~ variable + grouping2random, data = abcxyzBox)
summary(res.aov2)

# Two-way ANOVA with interaction effect
# These two calls are equivalent
res.aov3 <- aov(values ~ variable * grouping2random, data = abcxyzBox)
res.aov3 <- aov(values ~ variable + grouping2random + variable:grouping2random, data = abcxyzBox)
summary(res.aov3)

res.aov4 <- aov(values ~ variable * grouping2random * Season2random, data = abcxyzBox)
summary(res.aov4)

res.aov4 <- aov(NumericValues ~ VectorID * City * Season, data = abcxyzBox)
summary(res.aov4)

TukeyHSD(res.aov3, which = "variable")
TukeyHSD(res.aov3, which = "grouping2random")

#Using Non-parametric data

boxplot(NumericValues~VectorID, data=abcxyzBox)

boxplot(NumericValues~VectorID, data=abcxyzBox,outline=F)
attach(abcxyzBox)
kruskal.test(NumericValues, VectorID)

#pairwise wilcox test

pairwise.wilcox.test(NumericValues, VectorID, p.adjust.method = 'BH')

#dunn test does not come with base R. You will need to install a package for this.
install.packages("dunn.test")
library(dunn.test)

dunn.test::dunn.test(NumericValues, VectorID, method = 'bh')  


#Pairwise Multiple Comparison of Mean Ranks (PMCMR)

install.packages("PMCMRplus")

library(PMCMRplus)

PMCMRplus::kwAllPairsNemenyiTest(NumericValues, VectorID,method="Tukey",p.adjust.methods=  'BH')  

#Does not auto convert characters to factors
PMCMRplus::kwAllPairsNemenyiTest(NumericValues, as.factor(VectorID),method="Tukey",p.adjust.methods=  'BH')  

#IMPORTANT WARNING: Use new R packages with caution. They can have "bugs".

#Conover-Iman Test of Multiple Comparisons Using Rank Sums, p.adjust.method is Benjamini-Hochberg
install.packages("conover.test")
library(conover.test)
conover.test::conover.test(NumericValues, VectorID, method = 'bh')


######################################################
#Biostatistics course in R - Week 5
######################################################


LunchAmount<-c(300,154,72,899,1234,36,764,321,540,684)
TipAmount<-c(30,15,7,89,123,3,76,32,54,68)

plot(LunchAmount,TipAmount,xlab="Lunch Amount",ylab="Tip Amount")

reg_model <- lm(TipAmount~LunchAmount)
#plotting the regression fit
abline(reg_model, col="blue")
?lm
summary(reg_model)

a<-data.frame(newdata=40)
colnames(a)<-"LunchAmount"
#what is the Tip if cost of lunch is 40 INR?
predict(reg_model,a)

a<-data.frame(newdata=394)
colnames(a)<-"LunchAmount"
#what is the Tip if cost of lunch is 394 INR?
predict(reg_model,a)

RestaurantClass<-c(2,1,1,3,4,1,2,1,2,2)

reg_model2 <- lm(TipAmount~LunchAmount+RestaurantClass)
#plotting the regression fit
abline(reg_model2, col="red")
summary(reg_model2)


reg_model3 <- lm(TipAmount~LunchAmount*RestaurantClass)
#plotting the regression fit
abline(reg_model3, col="green")
summary(reg_model3)

AIC(reg_model)
AIC(reg_model2)
AIC(reg_model3)

#null model is the coefficient/intercept is equal to zero
# A significant result indicates rejection of the null model. Hence, a non-zero coefficient/intercept.


#Interest rate, Unemployment Rate and Stock Index Price example

Year <- c(2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016)
Month <- c(12, 11,10,9,8,7,6,5,4,3,2,1,12,11,10,9,8,7,6,5,4,3,2,1)
Interest_Rate <- c(2.75,2.5,2.5,2.5,2.5,2.5,2.5,2.25,2.25,2.25,2,2,2,1.75,1.75,1.75,1.75,1.75,1.75,1.75,1.75,1.75,1.75,1.75)
Unemployment_Rate <- c(5.3,5.3,5.3,5.3,5.4,5.6,5.5,5.5,5.5,5.6,5.7,5.9,6,5.9,5.8,6.1,6.2,6.1,6.1,6.1,5.9,6.2,6.2,6.1)
Stock_Index_Price <- c(1464,1394,1357,1293,1256,1254,1234,1195,1159,1167,1130,1075,1047,965,943,958,971,949,884,866,876,822,704,719)

data.frame(Year,Month,Interest_Rate,Unemployment_Rate,Stock_Index_Price)->Stock_data

#Check for linearity
plot(x=Interest_Rate, y=Stock_Index_Price)

plot(x=Unemployment_Rate, y=Stock_Index_Price)

#Multiple linear regression
model1 <- lm(Stock_Index_Price ~ Interest_Rate + Unemployment_Rate)
summary(model1)

model2 <- lm(Stock_Index_Price ~ Interest_Rate * Unemployment_Rate)
summary(model2)

AIC(model1)
AIC(model2)

#get the ANOVA table from a regression model -- another use of avov function
aov(model1)->aov_model1
summary(aov_model1)
summary(model1)


aov(model2)->aov_model2
summary(aov_model2)


#model comparison with anova function

anova(model1,model2)

anova(model2,model1)

#Visualize multiple regression model

##will update R to the latest version
install.packages("installr")
library(installr)
updateR(F, T, T, F, T, F, T)


install.packages("devtools")
devtools::install_github("cardiomoon/ggiraphExtra")

require(ggplot2)
ggplot(Stock_data,aes(y=Stock_Index_Price,x=Interest_Rate))+geom_point()+geom_smooth(method="lm")

ggplot(Stock_data,aes(y=Stock_Index_Price,x=Unemployment_Rate))+geom_point()+geom_smooth(method="lm")

require(ggiraph)
require(ggiraphExtra)
require(plyr)

ggPredict(model1,interactive=TRUE)
ggPredict(model2,interactive=TRUE)

