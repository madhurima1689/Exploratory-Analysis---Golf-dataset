#--------------Exploratory Analysis------------------------

#Setting up working directory
setwd("C:/Users/mchoudhu/Documents/********")
getwd()

#Importing and calling libraries
install.packages("readxl")
library("readxl")

#Importing required Golf dataset
golf = read_excel("Golf.xls")
attach(golf)

#Understanding the dataset using the following commands
head(golf)
dim(golf)
names(golf)
str(golf)
summary(golf)

#Univariate analysis
cat("SD for variable Current:",sd(Current),"; SD for variable New:",sd(New))
cat("Variance for variable Current:",var(Current),"; Variance for variable New:",var(New))
cat("SD for difference between New and Current: ",sd(New-Current))
cat("Variance for difference between New and Current: ",var(New-Current))

#Data Visualization using Histograms and Boxplots
par(mfrow=c(2,2),pty="m")
hist(Current,breaks= c(250, 255, 260, 265, 270, 275, 280, 285, 290, 295), 
     freq=TRUE,include.lowest=TRUE, 
     col="pink", 
     main="Histogram denoting current golf balls", 
     xlab="Driving distances with current balls", 
     ylab="Frequency")
hist(New,breaks= c(250, 255, 260, 265, 270, 275, 280, 285, 290, 295), 
     freq=TRUE,include.lowest=TRUE, 
     col="light blue", 
     main="Histogram denoting new golf balls", 
     xlab="Driving distances with new balls", 
     ylab="Frequency")

boxplot(Current,col= "pink", border= "black", 
        main="Boxplot denoting current driving distances", 
        xlab="Driving distances",
        horizontal = TRUE)
boxplot(New,col= "light blue", border= "black", 
        main="Boxplot denoting new driving distances", 
        xlab="Driving distances",
        horizontal = TRUE)

#Checking for missing values and outliers in the dataset
cat("Number of missing values =", sum(is.na(golf)))
cat("Outliers in Current:",boxplot(Current,plot=FALSE)$Out,"; Outliers in New:",boxplot(New,plot=FALSE)$Out)

#T-test
print("TWO TAILED INDEPENDENT TWO SAMPLE T TEST FOR MEANS")
t.test(Current, New, 
       paired = FALSE, 
       conf.level = 0.95, 
       alternative = "t")
print("TWO TAILED INDEPENDENT ONE SAMPLE T TEST FOR CURRENT MEAN")
t.test(Current, 
       paired = FALSE, 
       conf.level = 0.95, 
       alternative = "t")
print("TWO TAILED INDEPENDENT ONE SAMPLE T TEST FOR NEW MEAN")
t.test(New, 
       paired = FALSE, 
       conf.level = 0.95, 
       alternative = "t")

#Determination of the Power of test

diffsd=sd(Current - New)
cat("SD for difference is ",diffsd)
d=mean(New) - mean(Current)
cat("Difference in mean is " ,d)

power.t.test(n = 40, delta = d, sd = diffsd, 
             type = "t", 
             alternative = "t", 
             sig.level = .05)

#assuming difference in means as 5
power.t.test(power = .95, delta = 5, sd = diffsd, 
             type = "t", 
             alternative = "t",
             sig.level = .05)


#-------------------------------THE END----------------------------------
