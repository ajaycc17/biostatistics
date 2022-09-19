# set directory and load data
setwd("E:\\8_Eighth Sem\\Biostatistics\\Load_data\\data")

################################################################################
# load data set
lifeExpectancyInYears <- read.csv("life_expectancy_years.csv", header = T, check.names = F) # remove X from X1800...

# Plot
na.omit(as.numeric(unlist(lifeExpectancyInYears[lifeExpectancyInYears$country=="United States",])))->USLife
na.omit(as.numeric(unlist(lifeExpectancyInYears[lifeExpectancyInYears$country=="Germany",])))->GermanLife
na.omit(as.numeric(unlist(lifeExpectancyInYears[lifeExpectancyInYears$country=="United Kingdom",])))->UKLife
na.omit(as.numeric(unlist(lifeExpectancyInYears[lifeExpectancyInYears$country=="China",])))->ChinaLife
na.omit(as.numeric(unlist(lifeExpectancyInYears[lifeExpectancyInYears$country=="India",])))->IndiaLife
na.omit(as.numeric(unlist(lifeExpectancyInYears[lifeExpectancyInYears$country=="Japan",])))->JapanLife

min(USLife)
max(USLife)
sd(USLife)
mean(USLife)
median(USLife)

min(GermanLife)
max(GermanLife)
sd(GermanLife)
mean(GermanLife)
median(GermanLife)

min(UKLife)
max(UKLife)
sd(UKLife)
mean(UKLife)
median(UKLife)

min(ChinaLife)
max(ChinaLife)
sd(ChinaLife)
mean(ChinaLife)
median(ChinaLife)

min(IndiaLife)
max(IndiaLife)
sd(IndiaLife)
mean(IndiaLife)
median(IndiaLife)

min(JapanLife)
max(JapanLife)
sd(JapanLife)
mean(JapanLife)
median(JapanLife)

par(mfrow=c(3,2))
hist(USLife)
abline(v=mean(USLife))
text(x=mean(USLife), y=75, label=mean(USLife), col=2)

hist(GermanLife)
abline(v=mean(GermanLife))
text(x=mean(GermanLife), y=75, label=mean(GermanLife), col=2)

hist(UKLife)
abline(v=mean(UKLife))
text(x=mean(UKLife), y=50, label=mean(UKLife), col=2)

hist(ChinaLife)
abline(v=mean(ChinaLife))
text(x=mean(ChinaLife), y=75, label=mean(ChinaLife), col=2)

hist(IndiaLife)
abline(v=mean(IndiaLife))
text(x=mean(IndiaLife), y=75, label=mean(IndiaLife), col=2)

hist(JapanLife)
abline(v=mean(JapanLife))
text(x=mean(JapanLife), y=75, label=mean(JapanLife), col=2)

par(mfrow=c(1,1))
plot(c(1800:2100),USLife,col="red",main="Life Expectancy in Years",pch=15,ylim=c(0,100),xlab="Years",ylab="Age", lwd=2.0, type = "l")
lines(c(1800:2100),GermanLife,col="skyblue", lwd=2.0)
lines(c(1800:2100),UKLife,col="green", lwd=2.0)
lines(c(1800:2100),ChinaLife,col="orange", lwd=2.0)
lines(c(1800:2100),IndiaLife,col="black", lwd=2.0)
lines(c(1800:2100),JapanLife,col="yellow", lwd=2.0)

legend(x = "topleft",                    # Position
       legend = c("United States", "Germany", "United Kingdom", "China", "India", "Japan"),  # Legend texts
       fill = c("red", "skyblue","green", "orange", "black", "yellow"))              # Colors

abline(v=1959)
abline(v=1962)
abline(v=1966)

# box plot
boxplot(USLife, IndiaLife, GermanLife, UKLife, ChinaLife, JapanLife, names = c("US", "India", "Germany", "UK", "China", "Japan"))

# Inferential Statistics
# Shapiro test
shapiro.test(USLife) # null hypo- the distribution is normal (rejected for all the data below)
shapiro.test(GermanLife)
shapiro.test(UKLife)
shapiro.test(ChinaLife)
shapiro.test(IndiaLife)
shapiro.test(JapanLife)

# Wilcoxon Signed Rank test
wilcox.test(USLife, GermanLife, paired = T, alternative = "greater")
wilcox.test(ChinaLife[160:163], ChinaLife[167:170], paired = T, alternative = "greater")
wilcox.test(IndiaLife,JapanLife, paired = T, alternative = "less")

################################################################################
# Population
population <- read.csv("population_total.csv", header = T, check.names = F) # remove X from X1800...

# Plot
na.omit(as.numeric(unlist(population[population$country=="United States",])))->USLife
na.omit(as.numeric(unlist(population[population$country=="Germany",])))->GermanLife
na.omit(as.numeric(unlist(population[population$country=="United Kingdom",])))->UKLife
na.omit(as.numeric(unlist(population[population$country=="China",])))->ChinaLife
na.omit(as.numeric(unlist(population[population$country=="India",])))->IndiaLife
na.omit(as.numeric(unlist(population[population$country=="Japan",])))->JapanLife

min(USLife)
max(USLife)
sd(USLife)
mean(USLife)
median(USLife)

min(GermanLife)
max(GermanLife)
sd(GermanLife)
mean(GermanLife)
median(GermanLife)

min(UKLife)
max(UKLife)
sd(UKLife)
mean(UKLife)
median(UKLife)

min(ChinaLife)
max(ChinaLife)
sd(ChinaLife)
mean(ChinaLife)
median(ChinaLife)

min(IndiaLife)
max(IndiaLife)
sd(IndiaLife)
mean(IndiaLife)
median(IndiaLife)

min(JapanLife)
max(JapanLife)
sd(JapanLife)
mean(JapanLife)
median(JapanLife)

par(mfrow=c(3,2))
hist(USLife)
abline(v=mean(USLife))
text(x=mean(USLife), y=75, label=mean(USLife), col=2)

hist(GermanLife)
abline(v=mean(GermanLife))
text(x=mean(GermanLife), y=40, label=mean(GermanLife), col=2)

hist(UKLife)
abline(v=mean(UKLife))
text(x=mean(UKLife), y=30, label=mean(UKLife), col=2)

hist(ChinaLife)
abline(v=mean(ChinaLife))
text(x=mean(ChinaLife), y=70, label=mean(ChinaLife), col=2)

hist(IndiaLife)
abline(v=mean(IndiaLife))
text(x=mean(IndiaLife), y=75, label=mean(IndiaLife), col=2)

hist(JapanLife)
abline(v=mean(JapanLife))
text(x=mean(JapanLife), y=75, label=mean(JapanLife), col=2)

# trend plot
par(mfrow=c(1,1))
plot(c(1800:2100),USLife,col="red",main="Total Population",pch=15,ylim=c(0,1.7E+09),xlab="Years",ylab="Population", lwd=2.0, type = "l")
lines(c(1800:2100),GermanLife,col="skyblue", lwd=2.0)
lines(c(1800:2100),UKLife,col="green", lwd=2.0)
lines(c(1800:2100),ChinaLife,col="orange", lwd=2.0)
lines(c(1800:2100),IndiaLife,col="black", lwd=2.0)
lines(c(1800:2100),JapanLife,col="yellow", lwd=2.0)

legend(x = "topleft",                    # Position
       legend = c("United States", "Germany", "United Kingdom", "China", "India", "Japan"),  # Legend texts
       fill = c("red", "skyblue","green", "orange", "black", "yellow"))              # Colors

abline(v=1950)
abline(v=2022)

# box plot
boxplot(USLife, IndiaLife, GermanLife, UKLife, ChinaLife, JapanLife, names = c("US", "India", "Germany", "UK", "China", "Japan"))

# Inferential Statistics
# Shapiro test
shapiro.test(USLife) # null hypo- the distribution is normal (rejected for all the data below)
shapiro.test(GermanLife)
shapiro.test(UKLife)
shapiro.test(ChinaLife)
shapiro.test(IndiaLife)
shapiro.test(JapanLife)

# Wilcoxon test
wilcox.test(USLife, GermanLife, paired = T, alternative = "greater")
wilcox.test(USLife[131], USLife[161], paired = T, alternative = "greater")
wilcox.test(IndiaLife,JapanLife, paired = T, alternative = "less")

################################################################################
# GDP Per Capita
gdp <- read.csv("income_per_person_gdppercapita_ppp_inflation_adjusted.csv", header = T, check.names = F) # remove X from X1800...

# Plot
na.omit(as.numeric(unlist(gdp[gdp$country=="United States",])))->USLife
na.omit(as.numeric(unlist(gdp[gdp$country=="Germany",])))->GermanLife
na.omit(as.numeric(unlist(gdp[gdp$country=="United Kingdom",])))->UKLife
na.omit(as.numeric(unlist(gdp[gdp$country=="China",])))->ChinaLife
na.omit(as.numeric(unlist(gdp[gdp$country=="India",])))->IndiaLife
na.omit(as.numeric(unlist(gdp[gdp$country=="Japan",])))->JapanLife

min(USLife)
max(USLife)
sd(USLife)
mean(USLife)
median(USLife)

min(GermanLife)
max(GermanLife)
sd(GermanLife)
mean(GermanLife)
median(GermanLife)

min(UKLife)
max(UKLife)
sd(UKLife)
mean(UKLife)
median(UKLife)

min(ChinaLife)
max(ChinaLife)
sd(ChinaLife)
mean(ChinaLife)
median(ChinaLife)

min(IndiaLife)
max(IndiaLife)
sd(IndiaLife)
mean(IndiaLife)
median(IndiaLife)

min(JapanLife)
max(JapanLife)
sd(JapanLife)
mean(JapanLife)
median(JapanLife)

par(mfrow=c(3,2))
hist(USLife)
abline(v=mean(USLife))
text(x=mean(USLife), y=75, label=mean(USLife), col=2)

hist(GermanLife)
abline(v=mean(GermanLife))
text(x=mean(GermanLife), y=75, label=mean(GermanLife), col=2)

hist(UKLife)
abline(v=mean(UKLife))
text(x=mean(UKLife), y=50, label=mean(UKLife), col=2)

hist(ChinaLife)
abline(v=mean(ChinaLife))
text(x=mean(ChinaLife), y=75, label=mean(ChinaLife), col=2)

hist(IndiaLife)
abline(v=mean(IndiaLife))
text(x=mean(IndiaLife), y=75, label=mean(IndiaLife), col=2)

hist(JapanLife)
abline(v=mean(JapanLife))
text(x=mean(JapanLife), y=75, label=mean(JapanLife), col=2)

par(mfrow=c(1,1))
plot(c(1800:2040),USLife,col="red",main="GDP Per Capita",pch=15,ylim=c(0,90000),xlab="Years",ylab="GDP Per Capita", lwd=2.0, type = "l")
lines(c(1800:2040),GermanLife,col="skyblue", lwd=2.0)
lines(c(1800:2040),UKLife,col="green", lwd=2.0)
lines(c(1800:2040),ChinaLife,col="orange", lwd=2.0)
lines(c(1800:2040),IndiaLife,col="black", lwd=2.0)
lines(c(1800:2040),JapanLife,col="yellow", lwd=2.0)

legend(x = "topleft",                    # Position
       legend = c("United States", "Germany", "United Kingdom", "China", "India", "Japan"),  # Legend texts
       fill = c("red", "skyblue","green", "orange", "black", "yellow"))              # Colors

abline(v=1930)
abline(v=1935)
abline(v=1945)

# box plot
boxplot(USLife, IndiaLife, GermanLife, UKLife, ChinaLife, JapanLife, names = c("US", "India", "Germany", "UK", "China", "Japan"), outline = F)

# Inferential Statistics
# Shapiro test
shapiro.test(USLife) # null hypo- the distribution is normal (rejected for all the data below)
shapiro.test(GermanLife)
shapiro.test(UKLife)
shapiro.test(ChinaLife)
shapiro.test(IndiaLife)
shapiro.test(JapanLife)

# Wilcoxon test
wilcox.test(USLife, GermanLife, paired = T, alternative = "greater")
wilcox.test(USLife[131:136],USLife[146:151], paired = T, alternative = "greater")
wilcox.test(IndiaLife,JapanLife, paired = T, alternative = "less")

################################################################################
# Children per Woman
CPWoman <- read.csv("children_per_woman_total_fertility.csv", header = T, check.names = F) # remove X from X1800...

# Plot
na.omit(as.numeric(unlist(CPWoman[CPWoman$country=="United States",])))->USLife
na.omit(as.numeric(unlist(CPWoman[CPWoman$country=="Germany",])))->GermanLife
na.omit(as.numeric(unlist(CPWoman[CPWoman$country=="United Kingdom",])))->UKLife
na.omit(as.numeric(unlist(CPWoman[CPWoman$country=="China",])))->ChinaLife
na.omit(as.numeric(unlist(CPWoman[CPWoman$country=="India",])))->IndiaLife
na.omit(as.numeric(unlist(CPWoman[CPWoman$country=="Japan",])))->JapanLife

min(USLife)
max(USLife)
sd(USLife)
mean(USLife)
median(USLife)

min(GermanLife)
max(GermanLife)
sd(GermanLife)
mean(GermanLife)
median(GermanLife)

min(UKLife)
max(UKLife)
sd(UKLife)
mean(UKLife)
median(UKLife)

min(ChinaLife)
max(ChinaLife)
sd(ChinaLife)
mean(ChinaLife)
median(ChinaLife)

min(IndiaLife)
max(IndiaLife)
sd(IndiaLife)
mean(IndiaLife)
median(IndiaLife)

min(JapanLife)
max(JapanLife)
sd(JapanLife)
mean(JapanLife)
median(JapanLife)

par(mfrow=c(3,2))
hist(USLife)
abline(v=mean(USLife))
text(x=mean(USLife), y=75, label=mean(USLife), col=2)

hist(GermanLife)
abline(v=mean(GermanLife))
text(x=mean(GermanLife), y=75, label=mean(GermanLife), col=2)

hist(UKLife)
abline(v=mean(UKLife))
text(x=mean(UKLife), y=50, label=mean(UKLife), col=2)

hist(ChinaLife)
abline(v=mean(ChinaLife))
text(x=mean(ChinaLife), y=75, label=mean(ChinaLife), col=2)

hist(IndiaLife)
abline(v=mean(IndiaLife))
text(x=mean(IndiaLife), y=75, label=mean(IndiaLife), col=2)

hist(JapanLife)
abline(v=mean(JapanLife))
text(x=mean(JapanLife), y=75, label=mean(JapanLife), col=2)

par(mfrow=c(1,1))
plot(c(1800:2100),USLife,col="red",main="Children Per Woman",pch=15,ylim=c(0,9),xlab="Years",ylab="Population", lwd=2.0, type = "l")
lines(c(1800:2100),GermanLife,col="skyblue", lwd=2.0)
lines(c(1800:2100),UKLife,col="green", lwd=2.0)
lines(c(1800:2100),ChinaLife,col="orange", lwd=2.0)
lines(c(1800:2100),IndiaLife,col="black", lwd=2.0)
lines(c(1800:2100),JapanLife,col="yellow", lwd=2.0)

legend(x = "topright",                    # Position
       legend = c("United States", "Germany", "United Kingdom", "China", "India", "Japan"),  # Legend texts
       fill = c("red", "skyblue","green", "orange", "black", "yellow"))              # Colors

abline(v=1959)
abline(v=1961)
abline(v=2022)

# box plot
boxplot(USLife, IndiaLife, GermanLife, UKLife, ChinaLife, JapanLife, names = c("US", "India", "Germany", "UK", "China", "Japan"))

# Inferential Statistics
# Shapiro test
shapiro.test(USLife) # null hypo- the distribution is normal (rejected for all the data below)
shapiro.test(GermanLife)
shapiro.test(UKLife)
shapiro.test(ChinaLife)
shapiro.test(IndiaLife)
shapiro.test(JapanLife)

# Wilcoxon test
wilcox.test(USLife, GermanLife, paired = T, alternative = "greater")
wilcox.test(ChinaLife[160:162], ChinaLife[167:169], paired = T, alternative = "greater")
wilcox.test(IndiaLife,JapanLife, paired = T, alternative = "less")

################################################################################
# Child Mortality
ChildMortality <- read.csv("child_mortality_0_5_year_olds_dying_per_1000_born.csv", header = T, check.names = F) # remove X from X1800...

# Plot
na.omit(as.numeric(unlist(ChildMortality[ChildMortality$country=="United States",])))->USLife
na.omit(as.numeric(unlist(ChildMortality[ChildMortality$country=="Germany",])))->GermanLife
na.omit(as.numeric(unlist(ChildMortality[ChildMortality$country=="United Kingdom",])))->UKLife
na.omit(as.numeric(unlist(ChildMortality[ChildMortality$country=="China",])))->ChinaLife
na.omit(as.numeric(unlist(ChildMortality[ChildMortality$country=="India",])))->IndiaLife
na.omit(as.numeric(unlist(ChildMortality[ChildMortality$country=="Japan",])))->JapanLife

min(USLife)
max(USLife)
sd(USLife)
mean(USLife)
median(USLife)

min(GermanLife)
max(GermanLife)
sd(GermanLife)
mean(GermanLife)
median(GermanLife)

min(UKLife)
max(UKLife)
sd(UKLife)
mean(UKLife)
median(UKLife)

min(ChinaLife)
max(ChinaLife)
sd(ChinaLife)
mean(ChinaLife)
median(ChinaLife)

min(IndiaLife)
max(IndiaLife)
sd(IndiaLife)
mean(IndiaLife)
median(IndiaLife)

min(JapanLife)
max(JapanLife)
sd(JapanLife)
mean(JapanLife)
median(JapanLife)

par(mfrow=c(3,2))
hist(USLife)
abline(v=mean(USLife))
text(x=mean(USLife), y=75, label=mean(USLife), col=2)

hist(GermanLife)
abline(v=mean(GermanLife))
text(x=mean(GermanLife), y=75, label=mean(GermanLife), col=2)

hist(UKLife)
abline(v=mean(UKLife))
text(x=mean(UKLife), y=50, label=mean(UKLife), col=2)

hist(ChinaLife)
abline(v=mean(ChinaLife))
text(x=mean(ChinaLife), y=75, label=mean(ChinaLife), col=2)

hist(IndiaLife)
abline(v=mean(IndiaLife))
text(x=mean(IndiaLife), y=75, label=mean(IndiaLife), col=2)

hist(JapanLife)
abline(v=mean(JapanLife))
text(x=mean(JapanLife), y=75, label=mean(JapanLife), col=2)

par(mfrow=c(1,1))
plot(c(1800:2100),USLife,col="red",main="Child Mortality",pch=15,ylim=c(0,600),xlab="Years",ylab="Child Mortality", lwd=2.0, type = "l")
lines(c(1800:2100),GermanLife,col="skyblue", lwd=2.0)
lines(c(1800:2100),UKLife,col="green", lwd=2.0)
lines(c(1800:2100),ChinaLife,col="orange", lwd=2.0)
lines(c(1800:2100),IndiaLife,col="black", lwd=2.0)
lines(c(1800:2100),JapanLife,col="yellow", lwd=2.0)

legend(x = "topright",                    # Position
       legend = c("United States", "Germany", "United Kingdom", "China", "India", "Japan"),  # Legend texts
       fill = c("red", "skyblue","green", "orange", "black", "yellow"))              # Colors

abline(v=1918)
abline(v=1930)

# box plot
boxplot(USLife, IndiaLife, GermanLife, UKLife, ChinaLife, JapanLife, names = c("US", "India", "Germany", "UK", "China", "Japan"))

# Inferential Statistics
# Shapiro test
shapiro.test(USLife) # null hypo- the distribution is normal (rejected for all the data below)
shapiro.test(GermanLife)
shapiro.test(UKLife)
shapiro.test(ChinaLife)
shapiro.test(IndiaLife)
shapiro.test(JapanLife)

# Wilcoxon test
wilcox.test(USLife, GermanLife, paired = T, alternative = "greater")
wilcox.test(IndiaLife[119], IndiaLife[131], paired = T, alternative = "greater")
wilcox.test(IndiaLife,JapanLife, paired = T, alternative = "less")