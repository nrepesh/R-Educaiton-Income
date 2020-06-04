#------------------------------------------------------
# Part 1 Importing data

dset <- read.csv("Nrepesh Joshi - Twins (Missing Removed).csv", header = TRUE); 
head(dset)

#--------------------------------------------------------------------------------------------

# Part 2 Strip Charts

# Stripchart for the difference of logs of wages

stripchart(dset$DLHRWAGE, main="Stripchart for the difference of logs of wages", xlab="Difference in dollars",
           col="green")

# New variable for difference in hourly wages of twin 1 and twin 2

dset$DHRWAGE <- dset$HRWAGEL - dset$HRWAGEH; View(dset)

# Stripchart for the difference of wages

stripchart(dset$DHRWAGE, main= "Stripchart for the difference of wages", xlab="Difference in dollars",
           col="red")

# Multiple strip charts

x <- list("Log difference of wages"=dset$DLHRWAGE, "Non-Log difference of wages"=dset$DHRWAGE)
stripchart(x,
           main="Multiple stripchart for difference comparision",
           xlab="difference in dollars",
           ylab="Comparisions",
           col=c("green","red"),
           pch=16
)

#--------------------------------------------------------------------------------------------

# Part 3 Linear Regression 

model1 <- lm(dset$DLHRWAGE ~ dset$DEDUC1); summary(model1)
plot(dset$DEDUC1,dset$DLHRWAGE, main = "Difference in log hourly wage vs self-reported education",
     xlab = "self-reported education",
     ylab = "Difference in log hourly wage",
     pch = 16,
     col = "red")
cor(dset$DEDUC1,dset$DLHRWAGE)
abline(model1, col="blue")

#--------------------------------------------------------------------------------------------

# Part 4 Second Linear Regression 

model2 <- lm(dset$DLHRWAGE ~ dset$DEDUC2); summary(model2)
plot(dset$DEDUC2,dset$DLHRWAGE, main = "Difference in log hourly wage vs cross-reported education",
     xlab = "cross-reported education",
     ylab = "Difference in log hourly wage",
     pch = 16,
     col = "red")
cor(dset$DEDUC2,dset$DLHRWAGE)
abline(model2, col="blue")
exp(0.09234)

#--------------------------------------------------------------------------------------------

# Part 5 Diagonostic testing 

par(mfrow=c(2,2)) # Split screen 2,2

plot(model1)

par(mfrow=c(1,1))

myfits <- fitted(model2)

myresids <- residuals(model2)

qqnorm(myresids)    # Checks normality of model 

# Residual vs fits plot 
plot(myfits, myresids, main = "Residual-Fits Plot", xlab = "Fits", ylab = "Residuals")

# Time series plot 
plot(myresids, main = "Residual time series plot")

# Residual histogram 
hist(myresids)









