a <- 60 # this is an assignment
# this is a comment

# Objects ----
# It is case sensitive
A <- 80
A
a

# Overwriting objects ----
a <- 20

# Naming objects ----
my.obj <- 90 # valid
my.obj

my_obj <- 80 # valid
my_obj

.obj <- 90 # valid
my_obj <- 90 # valid
#_obj <- 90 # not valid

# names cannot start with numbers but can contain numbers
# names cannot have special characters except '.' and '_'

# Remove objects ----
rm(my.obj)

# Data types ----
# numeric 
123

# string
"Ajay"

# Logical
TRUE # not true
FALSE # not false
# or 
T
F

# Data structures in R ----
# Vector: 1 column or 1 row of data (1 type numberic or text)
c(1:100)
days <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
numbers <- c(1:1000)
numbers + numbers # 1+1, 2+2, 3+3 ....
# subsetting vectors
numbers[5] 
numbers[2:5]
numbers[1001] # not available
days[c(1,2,6)]
days[-5] # except 5th day
days[c(-4, -5)] # except 4th and 5th day

# List: 1 column or 1 row of data (1 or more types of data)
list(1, 2, 3, "Hello", TRUE, numbers)
list(1, 2, 3, T)
# Matrix: multiple columns or rows of data (1 type numeric or text)
# Data-frame: multiple columns or rows of data (multiple types of data)

?seq
seq(0, 30, by=3)
days[seq(0, 7, by=2)]

# Functions ----
myValues <- c(1:100)
myValues

mean(myValues)
median(myValues)
min(myValues)
max(myValues)
sum(myValues)
sd(myValues)
class(myValues)
length(myValues)
log(myValues)
log10(myValues)

mySqrt <- sqrt(myValues)
mySqrt


# read manual ----
?rnorm
?mean
rnorm(100, mean=5)
rnorm(100, 5) # both are same

?hist
hist(rnorm(100, mean=5))

# data frames ----
id <- 1:20
group <- c(rep("Vehicle", 10), rep("Drug", 10))
response <- c(rnorm(10, mean=25, sd=5), rnorm(10, mean=23, sd=5))

myData <- data.frame(Patient=id,
                     Treatment=group,
                     Response=response)
myData
head(myData)
head(myData, 10)
tail(myData)
tail(myData, 2)

dim(myData) # 20 rows and 3 columns
str(myData) # structure of dataframe
summary(myData)

# typecast ----
as.numeric(c("1", "2", "3", "4", "5"))
as.character(1:10)

# subsetting data frame ----
myData[1,2]
myData[2, ]
myData[1:3, 1:2]
myData[1:10, ]
myData[, 1:2]
myData[, "Response"]
myData$Response
myData$Response <= 23

myData[myData$Response > 26, ]
myData$Positive <- myData$Response > 26
myData
myData[myData$Treatment != "Vehicle" | myData$Response <= 23, ]

age <- round(rnorm(20, 40, 20))
myData$Age <- age
head(myData)

# reading data into R ----
setwd("E:/data")
population <- read.csv("population_total.csv", header = TRUE) # by default header= TRUE
str(population)
dim(population)
summary(population)

install.packages("xlsx")
library(xlsx)
pokemon <- read.xlsx("Pokemon.xlsx", sheetIndex = 1)
head(pokemon)

# to update R
install.packages("installr")
library(installr)
updateR()
