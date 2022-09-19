#vect1 <- c(13.128374, 8.072014, -1.857198, 7.004188, 7.823906, 6.952221, 4.639625, 4.950121, 14.314642, 1.533549)
#vect1

#vect2 <- c(12.69424, 10.26503, 11.45592, 12.22541, 13.56497, 10.46164, 12.24059, 12.83301, 12.36917, 11.60806)
#vect2

#t.test(vect1, vect2)
setwd("C:/Users/ajayc/Downloads")
horse <- read.csv("HorseStats.csv", header = TRUE) # by default header= TRUE
head(horse)

shapiro.test(horse[,2]) # 2021 data is normal
shapiro.test(horse[,3]) # 2020 data is normal

t.test(horse[,2],horse[,3], paired=T)
